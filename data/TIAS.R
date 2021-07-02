########### Setup Section ########### This section downloads the necessary R packages and functions we need to run the analysis and create visuals. 

# clear global environment

rm(list=ls())

# detach all libraries

detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# function to load libraries

pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("tidyverse"),  pkgTest)
lapply(c("ggplot2"),  pkgTest)
lapply(c("ggthemes"),  pkgTest)
lapply(c("ggpubr"),  pkgTest)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)

# set working directory 

setwd("/Users/kdlee/Desktop/Lebowitz_Lab/FTL/data")

########### Isolating TIAS FTL Participants In Waves 2005-2017 ###########

# The below code clears your R environment (found on the right side of the console and includes data tables and values). 
# Clearing the environment after you make heatmaps for each wave is important because otherwise the environment gets crowded – it would be hard to find data and values you need from each wave). 

rm(list=ls())

# Import the TIAS data. This dataset was downloaded from the PSID website and includes values for all waves. The raw csv file can be found on github. 

TIAS <- read.csv("https://raw.githubusercontent.com/carolinelee78/FTL/main/data/raw/PSID/TIAS/TIAS.csv")

# Before subsetting the data to only include data for the wave of interest, we are adding IDs for each row in a new column ('ID') to consistently identify each row (participant).

TIAS$ID <- seq.int(nrow(TIAS)) 

### TIAS 2005 IAC vs. FTL ###

#The below code applies the chosen criteria to the dataset and isolates the participants that are FTL from the 2005 wave. You will be shown the number of FTL participants,
#the number of IAC participants, and the participant IDs of all individuals who are FTL. 

# This selects the data only from the wave of interest, which in this case is 2005. 

TIAS2005 <- TIAS[!is.na(TIAS$TAS05),]

# Now, we select the variables of interest that filter for participants who meet FTL criteria. See TIAS-C variable table for the names and details of these variables. 

TIAS2005$CAT <- with(TIAS2005, ifelse(
  TA050042 == 1 & TA050043 %in% c("1", "96") & TA050595 %in% c("5", "0") & TA050631 %in% c("5", "0") & TA050127 == 3 & TA050769 < 60 & 
    TA050712 == 0 & TA050715 == 0 & TA050716 == 0 & TA050678 %in% c("3", "5", "7", "0") & TA050785 == 0 & TA050809 == 0 & TA050793 == 0 & TA050777 == 0 & TA050825 == 0 & 
    TA050817 == 0 & TA050798 == 0 & TA050394 %in% c("1", "2", "3", "4", "7", "8", "97", "99"), "FTL_05", ifelse(
      TA050042 == 1 & TA050043 %in% c("1", "96") & TA050595 %in% c("5", "0") & TA050631 %in% c("5", "0") & TA050127 == 3 & TA050769 < 60 & 
        TA050712 == 0 & TA050715 == 0 & TA050716 == 0 & TA050678 %in% c("3", "5", "7", "0") & TA050785 == 0 & TA050809 == 0 & TA050793 == 0 & TA050777 == 0 & TA050825 == 0 & 
        TA050817 == 0 & TA050798 == 0 & TA050394 == 0 & TA050371 == 1, "FTL_05", "IAC_05")))

# Before subsetting the data to only include data for the wave of interest, we are adding IDs for each row in a new column ('ID') to consistently identify each row (participant).

T05_ID <- TIAS2005$ID

# Extract IDs of participants who have been identified as FTL for the 2005 wave 

FTL05_ID <- TIAS2005[TIAS2005$CAT == "FTL_05", "ID"]

# View the IDs of participants who have been identified as FTL for the 2005 wave 

print(FTL05_ID)

# View the number of FTL vs. IAC participants for the 2005 wave 

table(TIAS2005$CAT)

# Create a new variable ('CAT_05') for FTL vs. IAC vs. NA (no data) categorization in the 2005 wave 

TIAS$CAT_05 <- with(TIAS, ifelse(
  ID %in% FTL05_ID, "FTL_05", ifelse(
    ID %in% T05_ID, "IAC_05", "NA_05")))

# View the distribution for CAT_05

table(TIAS$CAT_05)

### TIAS 2007 IAC vs. FTL ###

#The below code applies the chosen criteria to the dataset and isolates the participants that are FTL from the 2007 wave. You will be shown the number of FTL participants,
#the number of IAC participants, and the participant IDs of all individuals who are FTL.

# This selects the data only from the wave of interest, which in this case is 2007. 

TIAS2007 <- TIAS[!is.na(TIAS$TAS07),]

# Now, we select the variables of interest that filter for participants who meet FTL criteria. See TIAS-C variable table for the names and details of these variables. 

TIAS2007$CAT <- with(TIAS2007, ifelse(
  TA070042 == 1 & TA070043 %in% c("1", "96") & TA070570 %in% c("5", "0") & TA070602 %in% c("5", "0") & TA070127 == 3 & TA070740 < 60 & 
    TA070683 == 0 & TA070686 == 0 & TA070687 == 0 & TA070649 %in% c("3", "5", "7", "0") & TA070756 == 0 & TA070777 == 0 & TA070764 == 0 & TA070748 == 0 & TA070793 == 0 & 
    TA070785 == 0 & TA070769 == 0 & TA070368 %in% c("1", "2", "3", "4", "7", "8", "97", "99"), "FTL_07", ifelse(
      TA070042 == 1 & TA070043 %in% c("1", "96") & TA070570 %in% c("5", "0") & TA070602 %in% c("5", "0") & TA070127 == 3 & TA070740 < 60 & 
        TA070683 == 0 & TA070686 == 0 & TA070687 == 0 & TA070649 %in% c("3", "5", "7", "0") & TA070756 == 0 & TA070777 == 0 & TA070764 == 0 & TA070748 == 0 & TA070793 == 0 & 
        TA070785 == 0 & TA070769 == 0 & TA070368 == 0 & TA070344 == 1, "FTL_07", "IAC_07")))

# Before subsetting the data to only include data for the wave of interest, we are adding IDs for each row in a new column ('ID') to consistently identify each row (participant).

T07_ID <- TIAS2007$ID

# Extract IDs of participants who have been identified as FTL for the 2007 wave 

FTL07_ID <- TIAS2007[TIAS2007$CAT == "FTL_07", "ID"]

# View the IDs of participants who have been identified as FTL for the 2007 wave 

print(FTL07_ID)

# View the number of FTL vs. IAC participants for the 2007 wave 

table(TIAS2007$CAT)

# Create a new variable ('CAT_07') for FTL vs. IAC vs. NA (no data) categorization in the 2007 wave 

TIAS$CAT_07 <- with(TIAS, ifelse(
  ID %in% FTL07_ID, "FTL_07", ifelse(
    ID %in% T07_ID, "IAC_07", "NA_07")))

# View the distribution for CAT_07

table(TIAS$CAT_07)

### TIAS 2009 IAC vs. FTL ###

#The below code applies the chosen criteria to the dataset and isolates the participants that are FTL from the 2009 wave. You will be shown the number of FTL participants,
#the number of IAC participants, and the participant IDs of all individuals who are FTL.

# This selects the data only from the wave of interest, which in this case is 2009. 

TIAS2009 <- TIAS[!is.na(TIAS$TAS09),]

# Now, we select the variables of interest that filter for participants who meet FTL criteria. See TIAS-C variable table for the names and details of these variables. 

TIAS2009$CAT <- with(TIAS2009, ifelse(
  TA090043 == 1 & TA090044 %in% c("1", "96") & TA090612 %in% c("5", "0") & TA090655 %in% c("5", "0") & TA090136 == 3 & TA090799 < 60 & 
    TA090739 == 0 & TA090742 == 0 & TA090743 == 0 & TA090705 %in% c("3", "5", "7", "0") & TA090815 == 0 & TA090836 == 0 & TA090823 == 0 & TA090807 == 0 & TA090852 == 0 & 
    TA090844 == 0 & TA090828 == 0 & TA090385 %in% c("1", "2", "3", "4", "7", "8", "97", "99"), "FTL_09", ifelse(
      TA090043 == 1 & TA090044 %in% c("1", "96") & TA090612 %in% c("5", "0") & TA090655 %in% c("5", "0") & TA090136 == 3 & TA090799 < 60 & 
        TA090739 == 0 & TA090742 == 0 & TA090743 == 0 & TA090705 %in% c("3", "5", "7", "0") & TA090815 == 0 & TA090836 == 0 & TA090823 == 0 & TA090807 == 0 & TA090852 == 0 & 
        TA090844 == 0 & TA090828 == 0 & TA090385 == 0 & TA090361 == 1, "FTL_09", "IAC_07")))

# Before subsetting the data to only include data for the wave of interest, we are adding IDs for each row in a new column ('ID') to consistently identify each row (participant).

T09_ID <- TIAS2009$ID

# Extract IDs of participants who have been identified as FTL for the 2009 wave 

FTL09_ID <- TIAS2009[TIAS2009$CAT == "FTL_09", "ID"]

# View the IDs of participants who have been identified as FTL for the 2009 wave 

print(FTL09_ID)

# View the number of FTL vs. IAC participants for the 2009 wave 

table(TIAS2009$CAT)

# Create a new variable ('CAT_09') for FTL vs. IAC vs. NA (no data) categorization in the 2009 wave 

TIAS$CAT_09 <- with(TIAS, ifelse(
  ID %in% FTL09_ID, "FTL_09", ifelse(
    ID %in% T09_ID, "IAC_09", "NA_09")))

# View the distribution for CAT_09

table(TIAS$CAT_09)

### TIAS 2011 IAC vs. FTL ###

#The below code applies the chosen criteria to the dataset and isolates the participants that are FTL from the 2011 wave. You will be shown the number of FTL participants,
#the number of IAC participants, and the participant IDs of all individuals who are FTL.

# This selects the data only from the wave of interest, which in this case is 2011. 

TIAS2011 <- TIAS[!is.na(TIAS$TAS11),]

# Now, we select the variables of interest that filter for participants who meet FTL criteria. See TIAS-C variable table for the names and details of these variables. 

TIAS2011$CAT <- with(TIAS2011, ifelse(
  TA110044 == 1 & TA110045 %in% c("1", "96") & TA110699 %in% c("5", "0") & TA110743 %in% c("5", "0") & TA110137 == 3 & TA110915 < 60 & 
    TA110829 == 0 & TA110832 == 0 & TA110833 == 0 & TA110793 %in% c("3", "5", "7", "0") & TA110931 == 0 & TA110952 == 0 & TA110939 == 0 & TA110923 == 0 & TA110968 == 0 & 
    TA110960 == 0 & TA110944 == 0 & TA110462 %in% c("1", "2", "3", "4", "7", "8", "97", "99"), "FTL_11", ifelse(
      TA110044 == 1 & TA110045 %in% c("1", "96") & TA110699 %in% c("5", "0") & TA110743 %in% c("5", "0") & TA110137 == 3 & TA110915 < 60 & 
        TA110829 == 0 & TA110832 == 0 & TA110833 == 0 & TA110793 %in% c("3", "5", "7", "0") & TA110931 == 0 & TA110952 == 0 & TA110939 == 0 & TA110923 == 0 & TA110968 == 0 & 
        TA110960 == 0 & TA110944 == 0 & TA110462 == 0 & TA110351 == 1, "FTL_11", "IAC_11")))

# Before subsetting the data to only include data for the wave of interest, we are adding IDs for each row in a new column ('ID') to consistently identify each row (participant).

T11_ID <- TIAS2011$ID

# Extract IDs of participants who have been identified as FTL for the 2011 wave 

FTL11_ID <- TIAS2011[TIAS2011$CAT == "FTL_11", "ID"]

# View the IDs of participants who have been identified as FTL for the 2011 wave 

print(FTL11_ID)

# View the number of FTL vs. IAC participants for the 2011 wave 

table(TIAS2011$CAT)

# Create a new variable ('CAT_11') for FTL vs. IAC vs. NA (no data) categorization in the 2011 wave 

TIAS$CAT_11 <- with(TIAS, ifelse(
  ID %in% FTL11_ID, "FTL_11", ifelse(
    ID %in% T11_ID, "IAC_11", "NA_11")))

# View the distribution for CAT_11

table(TIAS$CAT_11)

### TIAS 2013 IAC vs. FTL ###

#The below code applies the chosen criteria to the dataset and isolates the participants that are FTL from the 2013 wave. You will be shown the number of FTL participants,
#the number of IAC participants, and the participant IDs of all individuals who are FTL.

# This selects the data only from the wave of interest, which in this case is 2013. 

TIAS2013 <- TIAS[!is.na(TIAS$TAS13),]

# Now, we select the variables of interest that filter for participants who meet FTL criteria. See TIAS-C variable table for the names and details of these variables. 

TIAS2013$CAT <- with(TIAS2013, ifelse(
  TA130043 == 1 & TA130044 %in% c("1", "96") & TA130719 %in% c("5", "0") & TA130763 %in% c("5", "0") & TA130136 == 3 & TA130948 < 60 & 
    TA130852 == 0 & TA130855 == 0 & TA130856 == 0 & TA130813 %in% c("3", "5", "7", "0") & TA130961 == 0 & TA130982 == 0 & TA130969 == 0 & TA130861 == 0 & TA130998 == 0 &
    TA130990 == 0 & TA130977 == 0 & TA130482 %in% c("1", "2", "3", "4", "7", "8", "97", "99"), "FTL_13", ifelse(
      TA130043 == 1 & TA130044 %in% c("1", "96") & TA130719 %in% c("5", "0") & TA130763 %in% c("5", "0") & TA130136 == 3 & TA130948 < 60 & 
        TA130852 == 0 & TA130855 == 0 & TA130856 == 0 & TA130813 %in% c("3", "5", "7", "0") & TA130961 == 0 & TA130982 == 0 & TA130969 == 0 & TA130861 == 0 & TA130998 == 0 & 
        TA130990 == 0 & TA130977 == 0 & TA130482 == 0 & TA130350 == 1, "FTL_13", "IAC_13")))

# Before subsetting the data to only include data for the wave of interest, we are adding IDs for each row in a new column ('ID') to consistently identify each row (participant).

T13_ID <- TIAS2013$ID

# Extract IDs of participants who have been identified as FTL for the 2013 wave 

FTL13_ID <- TIAS2013[TIAS2013$CAT == "FTL_13", "ID"]

# View the IDs of participants who have been identified as FTL for the 2013 wave 

print(FTL13_ID)

# View the number of FTL vs. IAC participants for the 2013 wave 

table(TIAS2013$CAT)

# Create a new variable ('CAT_13') for FTL vs. IAC vs. NA (no data) categorization in the 2013 wave 

TIAS$CAT_13 <- with(TIAS, ifelse(
  ID %in% FTL13_ID, "FTL_13", ifelse(
    ID %in% T13_ID, "IAC_13", "NA_13")))

# View the distribution for CAT_13

table(TIAS$CAT_13)

### TIAS 2015 IAC vs. FTL ###

#The below code applies the chosen criteria to the dataset and isolates the participants that are FTL from the 2015 wave. You will be shown the number of FTL participants,
#the number of IAC participants, and the participant IDs of all individuals who are FTL.

# This selects the data only from the wave of interest, which in this case is 2015. 

TIAS2015 <- TIAS[!is.na(TIAS$TAS15),]

# Now, we select the variables of interest that filter for participants who meet FTL criteria. See TIAS-C variable table for the names and details of these variables. 

TIAS2015$CAT <- with(TIAS2015, ifelse(
  TA150043 == 1 & TA150044 %in% c("1", "96") & TA150731 %in% c("5", "0") & TA150776 %in% c("5", "0") & TA150128 == 3 & TA150970 < 60 & 
    TA150869 == 0 & TA150872 == 0 & TA150873 == 0 & TA150826 %in% c("3", "5", "7", "0") & TA150986 == 0 & TA151007 == 0 & TA150994 == 0 & TA150978 == 0 & TA151023 == 0 & 
    TA151015 == 0 & TA150999 == 0 & TA150491 %in% c("1", "2", "3", "4", "7", "8", "97", "99"), "FTL_15", ifelse(
      TA150043 == 1 & TA150044 %in% c("1", "96") & TA150731 %in% c("5", "0") & TA150776 %in% c("5", "0") & TA150128 == 3 & TA150970 < 60 & 
        TA150869 == 0 & TA150872 == 0 & TA150873 == 0 & TA150826 %in% c("3", "5", "7", "0") & TA150986 == 0 & TA151007 == 0 & TA150994 == 0 & TA150978 == 0 & TA151023 == 0 & 
        TA151015 == 0 & TA150999 == 0 & TA150491 == 0 & TA150352 == 1, "FTL_15", "IAC_15")))

# Before subsetting the data to only include data for the wave of interest, we are adding IDs for each row in a new column ('ID') to consistently identify each row (participant).

T15_ID <- TIAS2015$ID

# Extract IDs of participants who have been identified as FTL for the 2015 wave 

FTL15_ID <- TIAS2015[TIAS2015$CAT == "FTL_15", "ID"]

# View the IDs of participants who have been identified as FTL for the 2015 wave 

print(FTL15_ID)

# View the number of FTL vs. IAC participants for the 2015 wave 

table(TIAS2015$CAT)

# Create a new variable ('CAT_15') for FTL vs. IAC vs. NA (no data) categorization in the 2015 wave 

TIAS$CAT_15 <- with(TIAS, ifelse(
  ID %in% FTL15_ID, "FTL_15", ifelse(
    ID %in% T15_ID, "IAC_15", "NA_15")))

# View the distribution for CAT_15

table(TIAS$CAT_15)

### TIAS 2017 IAC vs. FTL ###

#The below code applies the chosen criteria to the dataset and isolates the participants that are FTL from the 2017 wave. You will be shown the number of FTL participants,
#the number of IAC participants, and the participant IDs of all individuals who are FTL.

# This selects the data only from the wave of interest, which in this case is 2017. 

TIAS2017 <- TIAS[!is.na(TIAS$TAS17),]

# Now, we select the variables of interest that filter for participants who meet FTL criteria. See TIAS-C variable table for the names and details of these variables. 

TIAS2017$CAT <- with(TIAS2017, ifelse(
  TA170058 == 1 & TA170059 %in% c("1", "96") & TA170790 %in% c("5", "0") & TA170416 %in% c("5", "0") & TA170183 == 3 & TA171827 < 60 & 
    TA170909 == 0 & TA170912 == 0 & TA170913 == 0 & TA170866 %in% c("3", "5", "7", "0") & TA171869 == 0 & TA171885 == 0 & TA171835 == 0 & TA171861 == 0 & TA171877 == 0 & 
    TA171835 == 0 & TA171840 == 0 & TA170389 == 1, "FTL_17", "IAC_17"))

# Before subsetting the data to only include data for the wave of interest, we are adding IDs for each row in a new column ('ID') to consistently identify each row (participant).

T17_ID <- TIAS2017$ID

# Extract IDs of participants who have been identified as FTL for the 2017 wave 

FTL17_ID <- TIAS2017[TIAS2017$CAT == "FTL_17", "ID"]

# View the IDs of participants who have been identified as FTL for the 2017 wave 

print(FTL17_ID)

# View the number of FTL vs. IAC participants for the 2017 wave 

table(TIAS2017$CAT)

# Create a new variable ('CAT_17') for FTL vs. IAC vs. NA (no data) categorization in the 2017 wave 

TIAS$CAT_17 <- with(TIAS, ifelse(
  ID %in% FTL17_ID, "FTL_17", ifelse(
    ID %in% T17_ID, "IAC_17", "NA_17")))

# View the distribution for CAT_17

table(TIAS$CAT_17)
