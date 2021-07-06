# There are three ways to collapse/expand sections: 
# (1) Go to the 'Edit' tab, and select 'Folding' to collapse/expand certain sections or collapse/expand all sections 
# (2) Shortcuts: collapse (option + command + L); expand (option + shift + command + L); collapse all (option + command + O); expand all (option + shift + command + O)
# (3) Click on the small upside down triangles to the left of each section marker to collapse/expand each section 

######################## Setup Section ######################## 

# This section downloads the necessary R packages and functions we need to run the analysis and create visuals. 

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

lapply(c("tidyverse"),pkgTest)
lapply(c("ggplot2"), pkgTest)
lapply(c("ggthemes"), pkgTest)
lapply(c("ggpubr"), pkgTest)
lapply(c("naniar"), pkgTest)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(naniar)
library(RColorBrewer)

# set working directory 

setwd("/Users/kdlee/Desktop/Lebowitz_Lab/FTL/data")

######################## Isolating TIAS FTL Participants In Waves 2005-2017 ######################## 

# The below code clears your R environment (found on the right side of the console and includes data tables and values). 
# Clearing the environment after you make heatmaps for each wave is important because otherwise the environment gets crowded – it would be hard to find data and values you need from each wave). 

rm(list=ls())

# Import the TIAS data. This dataset was downloaded from the PSID website and includes values for all waves. The raw csv file can be found on github. 

TIAS <- read.csv("https://raw.githubusercontent.com/carolinelee78/FTL/main/data/raw/PSID/TIAS/TIAS.csv")

# Before subsetting the data to only include data for the wave of interest, we are adding IDs for each row in a new column ('ID') to consistently identify each row (participant).

TIAS$ID <- seq.int(nrow(TIAS)) 

### TIAS 2005 IAC vs. FTL ============================================================================================================

#The below code applies the chosen criteria to the dataset and isolates the participants that are FTL from the 2005 wave. You will be shown the number of FTL participants,
#the number of IAC participants, and the participant IDs of all individuals who are FTL. 

# This selects the data only from the wave of interest, which in this case is 2005. 

TIAS2005 <- TIAS[!is.na(TIAS$TAS05),]

# Creating a subsetted dataframe including only FTL participants for the 2005 wave 

TIAS2005_FTL <- subset(TIAS2005, CAT == "FTL_05")

# Creating a subsetted dataframe including only IAC participants for the 2005 wave 

TIAS2005_IAC <- subset(TIAS2005, CAT == "IAC_05")

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

### TIAS 2007 IAC vs. FTL ============================================================================================================

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

### TIAS 2009 IAC vs. FTL ============================================================================================================

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

### TIAS 2011 IAC vs. FTL ============================================================================================================

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

### TIAS 2013 IAC vs. FTL ============================================================================================================

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

### TIAS 2015 IAC vs. FTL ============================================================================================================

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

### TIAS 2017 IAC vs. FTL ============================================================================================================

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

### TIAS FTL Wave Count ### 

# Creating a variable for the total number of waves that each participant identified as FTL (as opposed to IAC or NA)

TIAS$FTL_COUNT <- as.integer(as.logical(TIAS$CAT_05 == "FTL_05")) + as.integer(as.logical(TIAS$CAT_07 == "FTL_07")) + as.integer(as.logical(TIAS$CAT_09 == "FTL_09")) +
  as.integer(as.logical(TIAS$CAT_11 == "FTL_11")) + as.integer(as.logical(TIAS$CAT_13 == "FTL_13")) + as.integer(as.logical(TIAS$CAT_15 == "FTL_15")) + as.integer(as.logical(TIAS$CAT_17 == "FTL_17"))

# Viewing the distribution of FTL wave counts 

table(TIAS$FTL_COUNT)                        

# Creating a variable distinguishing participants who identified as FTL for at least one wave (greater than or equal to 1; GREQ1) vs. who never identified as FTL

TIAS$GREQ1_FTL <- with(TIAS, ifelse(FTL_COUNT >= 1, "Yes", "No"))      

######################## TIAS-D Analysis - TIAS 2005 ######################## 

### Amphetamine Usage ===========================================================================================================

####
# H44B. # of Times Took w/o Doc in Past 12mos: "On how many occasions (if any) have you taken/used amphetamine on your own
# w/o a doctor telling you to take them during the last 12 months?" 
# Answers: 1 (1-2); 2 (3-5); 3 (6-9); 4 (10-19); 5 (20-39); 6 (40 or more); 8 (DK); 9 (NA/refused); 0 (Inap: 0 in past 12 mos or never)
#### 

table(TIAS$TA050784)

table(TIAS$TA050784, TIAS$FTL_COUNT)

### Barbiturate Usage ===========================================================================================================

####
# H44E. # of Times Took w/o Doc in Past 12mos: "On how many occasions (if any) have you taken/used barbiturates on your own
# w/o a doctor telling you to take them during the last 12 months?" 
# Answers: 1 (1-2); 2 (3-5); 3 (6-9); 4 (10-19); 5 (20-39); 6 (40 or more); 8 (DK); 9 (NA/refused); 0 (Inap: 0 in past 12 mos or never)
#### 

### Marijuana Usage =============================================================================================================

### Steroid Usage ===============================================================================================================

### Tranquilizer Usage ==========================================================================================================

### Cocaine Usage ===============================================================================================================

### Average Alcohol Consumption Frequency Over Past Year ========================================================================

### Average Daily Alcohol Consumption ===========================================================================================

### Degree to Which Condition Limits Normal Daily Activities ====================================================================

### Depression Over Past Year ===================================================================================================

####
# H15. WTR>2 Wks Depressed In Past 12mos: “Now I want to ask you about periods of feeling sad, empty, or depressed. 
# In the past 12 months, have you had two weeks or longer when nearly every day you felt sad, empty, or depressed for most of the day?”
# Answers: 1 (Yes); 5 (No); 8 (DK); 9 (NA; Refused)
####

table(TIAS$TA050733)

table(TIAS$TA050733, TIAS$FTL_COUNT)

ggplot(TIAS) + 
  geom_bar(mapping = aes(x = TA050733, fill = as.factor(FTL_COUNT)), position = position_dodge(), na.rm = T) + 
  scale_x_continuous(breaks = seq(1, 9, by = 4)) + 
  labs(x = ">2 Weeks Depressed in Past 12 Months (1 = Yes; 5 = No; 9 = NA/Refused)", y = "Count") + 
  guides(fill = guide_legend(title = "# of FTL Waves"))

table(TIAS2005$TA050733, TIAS2005$CAT)

legend_title <- "Depressed >2 Weeks"
ggplot(TIAS2005) + 
  geom_bar(mapping = aes(x = CAT, fill = as.factor(TA050733)), position = position_dodge(), na.rm = T) + 
  labs(x = "Category", y = "Count") +
  scale_fill_manual(legend_title, values = c("aquamarine3", "cadetblue2", "cornflowerblue"), labels = c("Yes", "No", "NA/Refused"))

prop.table(table(TIAS2005_FTL$TA050733))

depr.pie.ftl.05 <- ggplot(data = TIAS2005_FTL, aes(x = " ", y = TA050733, fill = as.factor(TA050733))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual(legend_title, values = c("aquamarine3", "cadetblue2", "cornflowerblue"), labels = c("Yes", "No", "NA/Refused")) + 
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050733))

depr.pie.iac.05 <- ggplot(data = TIAS2005_IAC, aes(x = " ", y = TA050733, fill = as.factor(TA050733))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual(legend_title, values = c("aquamarine3", "cadetblue2", "cornflowerblue"), labels = c("Yes", "No", "NA/Refused")) + 
  theme_void() 

ggarrange(depr.pie.ftl.05, depr.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### Depression - Anhedonia =======================================================================================================

### Depression Diagnosis =========================================================================================================

### Bipolar Disorder Diagnosis ===================================================================================================

### Phobia Diagnosis =============================================================================================================

### Anxiety Disorders Diagnosis ==================================================================================================

#### 
# H12B. What was the diagnosis? What is the emotional or psychiatric disorder? -- Anxiety 
# Answers: 1 (Diagnosed w/ Anxiety); 8 (DK); 9 (NA/refused); 
# 0 (Inap. Never Diagnosed w/ Anxiety or Never Diagnosed with Emotional/Nervous/Psychiatric Problems)
####

table(TIAS$TA050713)

table(TIAS$TA050713, TIAS$FTL_COUNT)

table(TIAS2005$TA050713, TIAS2005$CAT)

### OCD Diagnosis ================================================================================================================

### Mental Health: Non-Spec Psych Distress =======================================================================================

####
# Cumulative score from answers to NSPD scale questions: 
# (how often felt nervous/hopeless/restless/everything as an effort/sad/worthless in past mo.)
# Possible scores: 0-24; 99 (all items are DK/NA/refused)
####

# Replacing 99 (value for 'all items are DK/NA/refused') with NA in TIAS and TIAS2005

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050938 = 99)) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050938 = 99)) 

# Viewing distribution for TA050938 (score from NSPD scale; possible values: 0-24)

table(TIAS2005$TA050938)

# Viewing distribution of NSPD scale score across TIAS dataset 

ggplot(TIAS2005) +
  geom_bar(mapping = aes(x = TA050938, fill = as.factor(TA050938)), na.rm = T) + 
  labs(x = "NSPD Scale Score (Min=0, Max=24)", y = "Count") + 
  theme(legend.position = "none") + 
  scale_x_continuous(breaks = seq(0, 24, by = 1))
  
# Aggregating mean NSPD scale scores for individuals with different total # of FTL wave match counts 

nspd.ftl.count <- aggregate.data.frame(TIAS$TA050938, list(TIAS$FTL_COUNT), mean, na.rm=T)

# Assigning column labels for resulting data frame 

colnames(nspd.ftl.count) <- c("FTL_Wave_Count", "Mean_NSPD")

# Viewing mean NSPD scale score for FTL wave match count total no. categories 

ggplot(nspd.ftl.count, aes(x = FTL_Wave_Count, y = Mean_NSPD, fill = FTL_Wave_Count)) +
  geom_bar(stat = "identity") +
  labs(x = "# of Waves for Which Participant Identified as FTL", y = "Mean NSPD Scale Score") + 
  scale_y_continuous(limits = c(0, 24), breaks = seq(0, 24, by = 2)) + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) +
  theme(legend.position = "none") +
  scale_fill_viridis_c() 

# Aggregating mean NSPD scale scores for individuals who were FTL for at least one wave vs. never FTL 

nspd.ftl.cat <- aggregate.data.frame(TIAS$TA050938, list(TIAS$GREQ1_FTL), mean, na.rm=T)

colnames(nspd.ftl.cat) <- c("At_Least_One_FTL", "Mean_NSPD")

ggplot(nspd.ftl.cat, aes(x = At_Least_One_FTL, y = Mean_NSPD, fill = as.factor(At_Least_One_FTL))) +
  geom_bar(stat = "identity") +
  labs(x = "Identified as FTL for at least one wave", y = "Mean NSPD Scale Score") + 
  scale_y_continuous(limits = c(0, 24), breaks = seq(0, 24, by = 2)) + 
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "none") 

### Mental Health: Social Anxiety ================================================================================================

### Mental Health: Worry =========================================================================================================

### Paid Employment Since Jan 1 of Prior Year ====================================================================================

### Paid Employment History ======================================================================================================

### Current Marital Status =======================================================================================================

### Romantic Relationships =======================================================================================================

### Number of Children ===========================================================================================================

### High School Education ========================================================================================================

### College Education ============================================================================================================

### Highest Education Level ======================================================================================================

### Responsibility - Earning Own Living ==========================================================================================

### Responsibility - Paying Own Rent =============================================================================================

### Responsibility - Paying Own Bills ============================================================================================

### Responsibility - Managing Money ==============================================================================================

### How Good at Taking Responsibility for Actions ================================================================================

### How Good at Money Management =================================================================================================

### How Good at Paying Off Credit Card Balances ==================================================================================

### MIDUS M1 - Happiness =========================================================================================================

### MIDUS M2 - Interest in Life ==================================================================================================

### MIDUS M3 - Satisfaction ======================================================================================================

### MIDUS M4 - Contribution of Something Important to Society ====================================================================

### MIDUS M5 - Belonging to Community ============================================================================================

### MIDUS M6 - Society Getting Better ============================================================================================

### MIDUS M7 - People Basically Good =============================================================================================

### MIDUS M8 - Way Society Makes Sense ===========================================================================================

### MIDUS M9 - Managing Daily Responsibility =====================================================================================

### MIDUS M10 - Trusting Relationships w/ Others =================================================================================

### MIDUS M11 - Challenged to Grow ===============================================================================================

### MIDUS M12 - Confident of Own Ideas ===========================================================================================

### MIDUS M13 - Liked Personality ================================================================================================

### MIDUS M14 - Life Had Direction ===============================================================================================

### MIDUS Subscale - Emotional ===================================================================================================

### MIDUS Subscale - Psychological ===============================================================================================

### Time Use - Involved in Arts ==================================================================================================

### Time Use - Type of Art (Involvement) =========================================================================================

### Time Use - Art Involvement Frequency (Over 12 Mos) ===========================================================================

### Time Use - Involved in Sports ================================================================================================

### Time Use - Watching/Reading News (Over 12 Mos) ===============================================================================

### Time Use - Reading for Pleasure (Over 12 Mos) ================================================================================

### Time Use - Watching Non-News TV Shows (Over 12 Mos) ==========================================================================

### Time Use - Internet Usage ====================================================================================================

### Time Use - Internet Usage for Email/Instant Messenger (Over 12 Mos) ==========================================================

### Time Use - Internet Usage for School/Research (Over 12 Mos) ==================================================================

### Time Use - Internet Usage for Shopping (Over 12 Mos) =========================================================================

### Time Use - Internet Usage for Playing Games (Over 12 Mos) ====================================================================

### Time Use - Voted in Last Presidential Election ===============================================================================

### Time Use - Involvement in Social Action Groups ===============================================================================

### Time Use - Type of Social Action Group (If Involved) =========================================================================

### Time Use - Social Action Group Involvement (Over 12 Mos) =====================================================================

### Time Use - School Club/Student Gov Involvement ===============================================================================

### Time Use - School Club/Student Gov Involvement Frequency (Over 12 Mos) =======================================================

### Time Use - Unpaid Volunteer/Community Sevice Work (Over 12 Mos) ==============================================================

### Time Use - Type of Volunteer Work ============================================================================================

### Time Use - How Often Volunteered (Over 12 Mos) ===============================================================================

### Self-Rating (Compared to Others) - Listening & Understanding Others ==========================================================

### Self-Rating (Compared to Others) - Teaching & Explaining to Others ===========================================================

### Self-Rating (Compared to Others) - Supervising Others ========================================================================

### Self-Rating (Compared to Others) - Being a Leader ============================================================================

### Self-Rating (Compared to Others) - Logical/Analytic Thinking =================================================================

### Self-Rating (Compared to Others) - Helping Others Solve Their Problems =======================================================

### How Good at Problem Solving ==================================================================================================

### Self-Rating (Compared to Others) - Intelligence ==============================================================================

### Self-Rating (Compared to Others) - Independence ==============================================================================

### Self-Rating (Compared to Others) - Confidence ================================================================================

### Self-Rating (Compared to Others) - Decisiveness ==============================================================================

### Frequency - Snacking Instead of Consuming Regular Meals ======================================================================

### Binge Eating Frequency =======================================================================================================

### Race - Hispanic ==============================================================================================================

### Race Identification ==========================================================================================================

### Daily Cigarette Usage ========================================================================================================

### Body Mass Index (BMI) ========================================================================================================

### Usual Amount of Daily Sleep ==================================================================================================

######################## TIAS-D Analysis - TIAS 2007 ######################## 

### Amphetamine Usage ===========================================================================================================

### Barbiturate Usage ===========================================================================================================

### Marijuana Usage =============================================================================================================

### Steroid Usage ===============================================================================================================

### Tranquilizer Usage ==========================================================================================================

### Cocaine Usage ===============================================================================================================

### Average Alcohol Consumption Frequency Over Past Year ========================================================================

### Average Daily Alcohol Consumption ===========================================================================================

### Degree to Which Condition Limits Normal Daily Activities ====================================================================

### Depression Over Past Year ===================================================================================================

### Depression - Anhedonia =======================================================================================================

### Depression Diagnosis =========================================================================================================

### Bipolar Disorder Diagnosis ===================================================================================================

### Phobia Diagnosis =============================================================================================================

### Anxiety Disorders Diagnosis ==================================================================================================

### OCD Diagnosis ================================================================================================================

### Mental Health: Non-Spec Psych Distress =======================================================================================

### Mental Health: Social Anxiety ================================================================================================

### Mental Health: Worry =========================================================================================================

### Paid Employment Since Jan 1 of Prior Year ====================================================================================

### Paid Employment History ======================================================================================================

### Current Marital Status =======================================================================================================

### Romantic Relationships =======================================================================================================

### Number of Children ===========================================================================================================

### High School Education ========================================================================================================

### College Education ============================================================================================================

### Highest Education Level ======================================================================================================

### Responsibility - Earning Own Living ==========================================================================================

### Responsibility - Paying Own Rent =============================================================================================

### Responsibility - Paying Own Bills ============================================================================================

### Responsibility - Managing Money ==============================================================================================

### How Good at Taking Responsibility for Actions ================================================================================

### How Good at Money Management =================================================================================================

### How Good at Paying Off Credit Card Balances ==================================================================================

### MIDUS M1 - Happiness =========================================================================================================

### MIDUS M2 - Interest in Life ==================================================================================================

### MIDUS M3 - Satisfaction ======================================================================================================

### MIDUS M4 - Contribution of Something Important to Society ====================================================================

### MIDUS M5 - Belonging to Community ============================================================================================

### MIDUS M6 - Society Getting Better ============================================================================================

### MIDUS M7 - People Basically Good =============================================================================================

### MIDUS M8 - Way Society Makes Sense ===========================================================================================

### MIDUS M9 - Managing Daily Responsibility =====================================================================================

### MIDUS M10 - Trusting Relationships w/ Others =================================================================================

### MIDUS M11 - Challenged to Grow ===============================================================================================

### MIDUS M12 - Confident of Own Ideas ===========================================================================================

### MIDUS M13 - Liked Personality ================================================================================================

### MIDUS M14 - Life Had Direction ===============================================================================================

### MIDUS Subscale - Emotional ===================================================================================================

### MIDUS Subscale - Psychological ===============================================================================================

### Time Use - Involved in Arts ==================================================================================================

### Time Use - Type of Art (Involvement) =========================================================================================

### Time Use - Art Involvement Frequency (Over 12 Mos) ===========================================================================

### Time Use - Involved in Sports ================================================================================================

### Time Use - Watching/Reading News (Over 12 Mos) ===============================================================================

### Time Use - Reading for Pleasure (Over 12 Mos) ================================================================================

### Time Use - Watching Non-News TV Shows (Over 12 Mos) ==========================================================================

### Time Use - Internet Usage ====================================================================================================

### Time Use - Internet Usage for Email/Instant Messenger (Over 12 Mos) ==========================================================

### Time Use - Internet Usage for School/Research (Over 12 Mos) ==================================================================

### Time Use - Internet Usage for Shopping (Over 12 Mos) =========================================================================

### Time Use - Internet Usage for Playing Games (Over 12 Mos) ====================================================================

### Time Use - Voted in Last Presidential Election ===============================================================================

### Time Use - Involvement in Social Action Groups ===============================================================================

### Time Use - Type of Social Action Group (If Involved) =========================================================================

### Time Use - Social Action Group Involvement (Over 12 Mos) =====================================================================

### Time Use - School Club/Student Gov Involvement ===============================================================================

### Time Use - School Club/Student Gov Involvement Frequency (Over 12 Mos) =======================================================

### Time Use - Unpaid Volunteer/Community Sevice Work (Over 12 Mos) ==============================================================

### Time Use - Type of Volunteer Work ============================================================================================

### Time Use - How Often Volunteered (Over 12 Mos) ===============================================================================

### Self-Rating (Compared to Others) - Listening & Understanding Others ==========================================================

### Self-Rating (Compared to Others) - Teaching & Explaining to Others ===========================================================

### Self-Rating (Compared to Others) - Supervising Others ========================================================================

### Self-Rating (Compared to Others) - Being a Leader ============================================================================

### Self-Rating (Compared to Others) - Logical/Analytic Thinking =================================================================

### Self-Rating (Compared to Others) - Helping Others Solve Their Problems =======================================================

### How Good at Problem Solving ==================================================================================================

### Self-Rating (Compared to Others) - Intelligence ==============================================================================

### Self-Rating (Compared to Others) - Independence ==============================================================================

### Self-Rating (Compared to Others) - Confidence ================================================================================

### Self-Rating (Compared to Others) - Decisiveness ==============================================================================

### Frequency - Snacking Instead of Consuming Regular Meals ======================================================================

### Binge Eating Frequency =======================================================================================================

### Race - Hispanic ==============================================================================================================

### Race Identification ==========================================================================================================

### Daily Cigarette Usage ========================================================================================================

### Body Mass Index (BMI) ========================================================================================================

### Usual Amount of Daily Sleep ==================================================================================================

######################## TIAS-D Analysis - TIAS 2009 ######################## 

### Amphetamine Usage ===========================================================================================================

### Barbiturate Usage ===========================================================================================================

### Marijuana Usage =============================================================================================================

### Steroid Usage ===============================================================================================================

### Tranquilizer Usage ==========================================================================================================

### Cocaine Usage ===============================================================================================================

### Average Alcohol Consumption Frequency Over Past Year ========================================================================

### Average Daily Alcohol Consumption ===========================================================================================

### Degree to Which Condition Limits Normal Daily Activities ====================================================================

### Depression Over Past Year ===================================================================================================

### Depression - Anhedonia =======================================================================================================

### Depression Diagnosis =========================================================================================================

### Bipolar Disorder Diagnosis ===================================================================================================

### Phobia Diagnosis =============================================================================================================

### Anxiety Disorders Diagnosis ==================================================================================================

### OCD Diagnosis ================================================================================================================

### Mental Health: Non-Spec Psych Distress =======================================================================================

### Mental Health: Social Anxiety ================================================================================================

### Mental Health: Worry =========================================================================================================

### Paid Employment Since Jan 1 of Prior Year ====================================================================================

### Paid Employment History ======================================================================================================

### Current Marital Status =======================================================================================================

### Romantic Relationships =======================================================================================================

### Number of Children ===========================================================================================================

### High School Education ========================================================================================================

### College Education ============================================================================================================

### Highest Education Level ======================================================================================================

### Responsibility - Earning Own Living ==========================================================================================

### Responsibility - Paying Own Rent =============================================================================================

### Responsibility - Paying Own Bills ============================================================================================

### Responsibility - Managing Money ==============================================================================================

### How Good at Taking Responsibility for Actions ================================================================================

### How Good at Money Management =================================================================================================

### How Good at Paying Off Credit Card Balances ==================================================================================

### MIDUS M1 - Happiness =========================================================================================================

### MIDUS M2 - Interest in Life ==================================================================================================

### MIDUS M3 - Satisfaction ======================================================================================================

### MIDUS M4 - Contribution of Something Important to Society ====================================================================

### MIDUS M5 - Belonging to Community ============================================================================================

### MIDUS M6 - Society Getting Better ============================================================================================

### MIDUS M7 - People Basically Good =============================================================================================

### MIDUS M8 - Way Society Makes Sense ===========================================================================================

### MIDUS M9 - Managing Daily Responsibility =====================================================================================

### MIDUS M10 - Trusting Relationships w/ Others =================================================================================

### MIDUS M11 - Challenged to Grow ===============================================================================================

### MIDUS M12 - Confident of Own Ideas ===========================================================================================

### MIDUS M13 - Liked Personality ================================================================================================

### MIDUS M14 - Life Had Direction ===============================================================================================

### MIDUS Subscale - Emotional ===================================================================================================

### MIDUS Subscale - Psychological ===============================================================================================

### Time Use - Involved in Arts ==================================================================================================

### Time Use - Type of Art (Involvement) =========================================================================================

### Time Use - Art Involvement Frequency (Over 12 Mos) ===========================================================================

### Time Use - Involved in Sports ================================================================================================

### Time Use - Watching/Reading News (Over 12 Mos) ===============================================================================

### Time Use - Reading for Pleasure (Over 12 Mos) ================================================================================

### Time Use - Watching Non-News TV Shows (Over 12 Mos) ==========================================================================

### Time Use - Internet Usage ====================================================================================================

### Time Use - Internet Usage for Email/Instant Messenger (Over 12 Mos) ==========================================================

### Time Use - Internet Usage for School/Research (Over 12 Mos) ==================================================================

### Time Use - Internet Usage for Shopping (Over 12 Mos) =========================================================================

### Time Use - Internet Usage for Playing Games (Over 12 Mos) ====================================================================

### Time Use - Voted in Last Presidential Election ===============================================================================

### Time Use - Involvement in Social Action Groups ===============================================================================

### Time Use - Type of Social Action Group (If Involved) =========================================================================

### Time Use - Social Action Group Involvement (Over 12 Mos) =====================================================================

### Time Use - School Club/Student Gov Involvement ===============================================================================

### Time Use - School Club/Student Gov Involvement Frequency (Over 12 Mos) =======================================================

### Time Use - Unpaid Volunteer/Community Sevice Work (Over 12 Mos) ==============================================================

### Time Use - Type of Volunteer Work ============================================================================================

### Time Use - How Often Volunteered (Over 12 Mos) ===============================================================================

### Self-Rating (Compared to Others) - Listening & Understanding Others ==========================================================

### Self-Rating (Compared to Others) - Teaching & Explaining to Others ===========================================================

### Self-Rating (Compared to Others) - Supervising Others ========================================================================

### Self-Rating (Compared to Others) - Being a Leader ============================================================================

### Self-Rating (Compared to Others) - Logical/Analytic Thinking =================================================================

### Self-Rating (Compared to Others) - Helping Others Solve Their Problems =======================================================

### How Good at Problem Solving ==================================================================================================

### Self-Rating (Compared to Others) - Intelligence ==============================================================================

### Self-Rating (Compared to Others) - Independence ==============================================================================

### Self-Rating (Compared to Others) - Confidence ================================================================================

### Self-Rating (Compared to Others) - Decisiveness ==============================================================================

### Frequency - Snacking Instead of Consuming Regular Meals ======================================================================

### Binge Eating Frequency =======================================================================================================

### Race - Hispanic ==============================================================================================================

### Race Identification ==========================================================================================================

### Daily Cigarette Usage ========================================================================================================

### Body Mass Index (BMI) ========================================================================================================

### Usual Amount of Daily Sleep ==================================================================================================


######################## TIAS-D Analysis - TIAS 2011 ######################## 

### Amphetamine Usage ===========================================================================================================

### Barbiturate Usage ===========================================================================================================

### Marijuana Usage =============================================================================================================

### Steroid Usage ===============================================================================================================

### Tranquilizer Usage ==========================================================================================================

### Cocaine Usage ===============================================================================================================

### Average Alcohol Consumption Frequency Over Past Year ========================================================================

### Average Daily Alcohol Consumption ===========================================================================================

### Degree to Which Condition Limits Normal Daily Activities ====================================================================

### Depression Over Past Year ===================================================================================================

### Depression - Anhedonia =======================================================================================================

### Depression Diagnosis =========================================================================================================

### Bipolar Disorder Diagnosis ===================================================================================================

### Phobia Diagnosis =============================================================================================================

### Anxiety Disorders Diagnosis ==================================================================================================

### OCD Diagnosis ================================================================================================================

### Mental Health: Non-Spec Psych Distress =======================================================================================

### Mental Health: Social Anxiety ================================================================================================

### Mental Health: Worry =========================================================================================================

### Paid Employment Since Jan 1 of Prior Year ====================================================================================

### Paid Employment History ======================================================================================================

### Current Marital Status =======================================================================================================

### Romantic Relationships =======================================================================================================

### Number of Children ===========================================================================================================

### High School Education ========================================================================================================

### College Education ============================================================================================================

### Highest Education Level ======================================================================================================

### Responsibility - Earning Own Living ==========================================================================================

### Responsibility - Paying Own Rent =============================================================================================

### Responsibility - Paying Own Bills ============================================================================================

### Responsibility - Managing Money ==============================================================================================

### How Good at Taking Responsibility for Actions ================================================================================

### How Good at Money Management =================================================================================================

### How Good at Paying Off Credit Card Balances ==================================================================================

### MIDUS M1 - Happiness =========================================================================================================

### MIDUS M2 - Interest in Life ==================================================================================================

### MIDUS M3 - Satisfaction ======================================================================================================

### MIDUS M4 - Contribution of Something Important to Society ====================================================================

### MIDUS M5 - Belonging to Community ============================================================================================

### MIDUS M6 - Society Getting Better ============================================================================================

### MIDUS M7 - People Basically Good =============================================================================================

### MIDUS M8 - Way Society Makes Sense ===========================================================================================

### MIDUS M9 - Managing Daily Responsibility =====================================================================================

### MIDUS M10 - Trusting Relationships w/ Others =================================================================================

### MIDUS M11 - Challenged to Grow ===============================================================================================

### MIDUS M12 - Confident of Own Ideas ===========================================================================================

### MIDUS M13 - Liked Personality ================================================================================================

### MIDUS M14 - Life Had Direction ===============================================================================================

### MIDUS Subscale - Emotional ===================================================================================================

### MIDUS Subscale - Psychological ===============================================================================================

### Time Use - Involved in Arts ==================================================================================================

### Time Use - Type of Art (Involvement) =========================================================================================

### Time Use - Art Involvement Frequency (Over 12 Mos) ===========================================================================

### Time Use - Involved in Sports ================================================================================================

### Time Use - Watching/Reading News (Over 12 Mos) ===============================================================================

### Time Use - Reading for Pleasure (Over 12 Mos) ================================================================================

### Time Use - Watching Non-News TV Shows (Over 12 Mos) ==========================================================================

### Time Use - Internet Usage ====================================================================================================

### Time Use - Internet Usage for Email/Instant Messenger (Over 12 Mos) ==========================================================

### Time Use - Internet Usage for School/Research (Over 12 Mos) ==================================================================

### Time Use - Internet Usage for Shopping (Over 12 Mos) =========================================================================

### Time Use - Internet Usage for Playing Games (Over 12 Mos) ====================================================================

### Time Use - Voted in Last Presidential Election ===============================================================================

### Time Use - Involvement in Social Action Groups ===============================================================================

### Time Use - Type of Social Action Group (If Involved) =========================================================================

### Time Use - Social Action Group Involvement (Over 12 Mos) =====================================================================

### Time Use - School Club/Student Gov Involvement ===============================================================================

### Time Use - School Club/Student Gov Involvement Frequency (Over 12 Mos) =======================================================

### Time Use - Unpaid Volunteer/Community Sevice Work (Over 12 Mos) ==============================================================

### Time Use - Type of Volunteer Work ============================================================================================

### Time Use - How Often Volunteered (Over 12 Mos) ===============================================================================

### Self-Rating (Compared to Others) - Listening & Understanding Others ==========================================================

### Self-Rating (Compared to Others) - Teaching & Explaining to Others ===========================================================

### Self-Rating (Compared to Others) - Supervising Others ========================================================================

### Self-Rating (Compared to Others) - Being a Leader ============================================================================

### Self-Rating (Compared to Others) - Logical/Analytic Thinking =================================================================

### Self-Rating (Compared to Others) - Helping Others Solve Their Problems =======================================================

### How Good at Problem Solving ==================================================================================================

### Self-Rating (Compared to Others) - Intelligence ==============================================================================

### Self-Rating (Compared to Others) - Independence ==============================================================================

### Self-Rating (Compared to Others) - Confidence ================================================================================

### Self-Rating (Compared to Others) - Decisiveness ==============================================================================

### Frequency - Snacking Instead of Consuming Regular Meals ======================================================================

### Binge Eating Frequency =======================================================================================================

### Race - Hispanic ==============================================================================================================

### Race Identification ==========================================================================================================

### Daily Cigarette Usage ========================================================================================================

### Body Mass Index (BMI) ========================================================================================================

### Usual Amount of Daily Sleep ==================================================================================================

######################## TIAS-D Analysis - TIAS 2013 ######################## 

### Amphetamine Usage ===========================================================================================================

### Barbiturate Usage ===========================================================================================================

### Marijuana Usage =============================================================================================================

### Steroid Usage ===============================================================================================================

### Tranquilizer Usage ==========================================================================================================

### Cocaine Usage ===============================================================================================================

### Average Alcohol Consumption Frequency Over Past Year ========================================================================

### Average Daily Alcohol Consumption ===========================================================================================

### Degree to Which Condition Limits Normal Daily Activities ====================================================================

### Depression Over Past Year ===================================================================================================

### Depression - Anhedonia =======================================================================================================

### Depression Diagnosis =========================================================================================================

### Bipolar Disorder Diagnosis ===================================================================================================

### Phobia Diagnosis =============================================================================================================

### Anxiety Disorders Diagnosis ==================================================================================================

### OCD Diagnosis ================================================================================================================

### Mental Health: Non-Spec Psych Distress =======================================================================================

### Mental Health: Social Anxiety ================================================================================================

### Mental Health: Worry =========================================================================================================

### Paid Employment Since Jan 1 of Prior Year ====================================================================================

### Paid Employment History ======================================================================================================

### Current Marital Status =======================================================================================================

### Romantic Relationships =======================================================================================================

### Number of Children ===========================================================================================================

### High School Education ========================================================================================================

### College Education ============================================================================================================

### Highest Education Level ======================================================================================================

### Responsibility - Earning Own Living ==========================================================================================

### Responsibility - Paying Own Rent =============================================================================================

### Responsibility - Paying Own Bills ============================================================================================

### Responsibility - Managing Money ==============================================================================================

### How Good at Taking Responsibility for Actions ================================================================================

### How Good at Money Management =================================================================================================

### How Good at Paying Off Credit Card Balances ==================================================================================

### MIDUS M1 - Happiness =========================================================================================================

### MIDUS M2 - Interest in Life ==================================================================================================

### MIDUS M3 - Satisfaction ======================================================================================================

### MIDUS M4 - Contribution of Something Important to Society ====================================================================

### MIDUS M5 - Belonging to Community ============================================================================================

### MIDUS M6 - Society Getting Better ============================================================================================

### MIDUS M7 - People Basically Good =============================================================================================

### MIDUS M8 - Way Society Makes Sense ===========================================================================================

### MIDUS M9 - Managing Daily Responsibility =====================================================================================

### MIDUS M10 - Trusting Relationships w/ Others =================================================================================

### MIDUS M11 - Challenged to Grow ===============================================================================================

### MIDUS M12 - Confident of Own Ideas ===========================================================================================

### MIDUS M13 - Liked Personality ================================================================================================

### MIDUS M14 - Life Had Direction ===============================================================================================

### MIDUS Subscale - Emotional ===================================================================================================

### MIDUS Subscale - Psychological ===============================================================================================

### Time Use - Involved in Arts ==================================================================================================

### Time Use - Type of Art (Involvement) =========================================================================================

### Time Use - Art Involvement Frequency (Over 12 Mos) ===========================================================================

### Time Use - Involved in Sports ================================================================================================

### Time Use - Watching/Reading News (Over 12 Mos) ===============================================================================

### Time Use - Reading for Pleasure (Over 12 Mos) ================================================================================

### Time Use - Watching Non-News TV Shows (Over 12 Mos) ==========================================================================

### Time Use - Internet Usage ====================================================================================================

### Time Use - Internet Usage for Email/Instant Messenger (Over 12 Mos) ==========================================================

### Time Use - Internet Usage for School/Research (Over 12 Mos) ==================================================================

### Time Use - Internet Usage for Shopping (Over 12 Mos) =========================================================================

### Time Use - Internet Usage for Playing Games (Over 12 Mos) ====================================================================

### Time Use - Voted in Last Presidential Election ===============================================================================

### Time Use - Involvement in Social Action Groups ===============================================================================

### Time Use - Type of Social Action Group (If Involved) =========================================================================

### Time Use - Social Action Group Involvement (Over 12 Mos) =====================================================================

### Time Use - School Club/Student Gov Involvement ===============================================================================

### Time Use - School Club/Student Gov Involvement Frequency (Over 12 Mos) =======================================================

### Time Use - Unpaid Volunteer/Community Sevice Work (Over 12 Mos) ==============================================================

### Time Use - Type of Volunteer Work ============================================================================================

### Time Use - How Often Volunteered (Over 12 Mos) ===============================================================================

### Self-Rating (Compared to Others) - Listening & Understanding Others ==========================================================

### Self-Rating (Compared to Others) - Teaching & Explaining to Others ===========================================================

### Self-Rating (Compared to Others) - Supervising Others ========================================================================

### Self-Rating (Compared to Others) - Being a Leader ============================================================================

### Self-Rating (Compared to Others) - Logical/Analytic Thinking =================================================================

### Self-Rating (Compared to Others) - Helping Others Solve Their Problems =======================================================

### How Good at Problem Solving ==================================================================================================

### Self-Rating (Compared to Others) - Intelligence ==============================================================================

### Self-Rating (Compared to Others) - Independence ==============================================================================

### Self-Rating (Compared to Others) - Confidence ================================================================================

### Self-Rating (Compared to Others) - Decisiveness ==============================================================================

### Frequency - Snacking Instead of Consuming Regular Meals ======================================================================

### Binge Eating Frequency =======================================================================================================

### Race - Hispanic ==============================================================================================================

### Race Identification ==========================================================================================================

### Daily Cigarette Usage ========================================================================================================

### Body Mass Index (BMI) ========================================================================================================

### Usual Amount of Daily Sleep ==================================================================================================

######################## TIAS-D Analysis - TIAS 2015 ######################## 

### Amphetamine Usage ===========================================================================================================

### Barbiturate Usage ===========================================================================================================

### Marijuana Usage =============================================================================================================

### Steroid Usage ===============================================================================================================

### Tranquilizer Usage ==========================================================================================================

### Cocaine Usage ===============================================================================================================

### Average Alcohol Consumption Frequency Over Past Year ========================================================================

### Average Daily Alcohol Consumption ===========================================================================================

### Degree to Which Condition Limits Normal Daily Activities ====================================================================

### Depression Over Past Year ===================================================================================================

### Depression - Anhedonia =======================================================================================================

### Depression Diagnosis =========================================================================================================

### Bipolar Disorder Diagnosis ===================================================================================================

### Phobia Diagnosis =============================================================================================================

### Anxiety Disorders Diagnosis ==================================================================================================

### OCD Diagnosis ================================================================================================================

### Mental Health: Non-Spec Psych Distress =======================================================================================

### Mental Health: Social Anxiety ================================================================================================

### Mental Health: Worry =========================================================================================================

### Paid Employment Since Jan 1 of Prior Year ====================================================================================

### Paid Employment History ======================================================================================================

### Current Marital Status =======================================================================================================

### Romantic Relationships =======================================================================================================

### Number of Children ===========================================================================================================

### High School Education ========================================================================================================

### College Education ============================================================================================================

### Highest Education Level ======================================================================================================

### Responsibility - Earning Own Living ==========================================================================================

### Responsibility - Paying Own Rent =============================================================================================

### Responsibility - Paying Own Bills ============================================================================================

### Responsibility - Managing Money ==============================================================================================

### How Good at Taking Responsibility for Actions ================================================================================

### How Good at Money Management =================================================================================================

### How Good at Paying Off Credit Card Balances ==================================================================================

### MIDUS M1 - Happiness =========================================================================================================

### MIDUS M2 - Interest in Life ==================================================================================================

### MIDUS M3 - Satisfaction ======================================================================================================

### MIDUS M4 - Contribution of Something Important to Society ====================================================================

### MIDUS M5 - Belonging to Community ============================================================================================

### MIDUS M6 - Society Getting Better ============================================================================================

### MIDUS M7 - People Basically Good =============================================================================================

### MIDUS M8 - Way Society Makes Sense ===========================================================================================

### MIDUS M9 - Managing Daily Responsibility =====================================================================================

### MIDUS M10 - Trusting Relationships w/ Others =================================================================================

### MIDUS M11 - Challenged to Grow ===============================================================================================

### MIDUS M12 - Confident of Own Ideas ===========================================================================================

### MIDUS M13 - Liked Personality ================================================================================================

### MIDUS M14 - Life Had Direction ===============================================================================================

### MIDUS Subscale - Emotional ===================================================================================================

### MIDUS Subscale - Psychological ===============================================================================================

### Time Use - Involved in Arts ==================================================================================================

### Time Use - Type of Art (Involvement) =========================================================================================

### Time Use - Art Involvement Frequency (Over 12 Mos) ===========================================================================

### Time Use - Involved in Sports ================================================================================================

### Time Use - Watching/Reading News (Over 12 Mos) ===============================================================================

### Time Use - Reading for Pleasure (Over 12 Mos) ================================================================================

### Time Use - Watching Non-News TV Shows (Over 12 Mos) ==========================================================================

### Time Use - Internet Usage ====================================================================================================

### Time Use - Internet Usage for Email/Instant Messenger (Over 12 Mos) ==========================================================

### Time Use - Internet Usage for School/Research (Over 12 Mos) ==================================================================

### Time Use - Internet Usage for Shopping (Over 12 Mos) =========================================================================

### Time Use - Internet Usage for Playing Games (Over 12 Mos) ====================================================================

### Time Use - Voted in Last Presidential Election ===============================================================================

### Time Use - Involvement in Social Action Groups ===============================================================================

### Time Use - Type of Social Action Group (If Involved) =========================================================================

### Time Use - Social Action Group Involvement (Over 12 Mos) =====================================================================

### Time Use - School Club/Student Gov Involvement ===============================================================================

### Time Use - School Club/Student Gov Involvement Frequency (Over 12 Mos) =======================================================

### Time Use - Unpaid Volunteer/Community Sevice Work (Over 12 Mos) ==============================================================

### Time Use - Type of Volunteer Work ============================================================================================

### Time Use - How Often Volunteered (Over 12 Mos) ===============================================================================

### Self-Rating (Compared to Others) - Listening & Understanding Others ==========================================================

### Self-Rating (Compared to Others) - Teaching & Explaining to Others ===========================================================

### Self-Rating (Compared to Others) - Supervising Others ========================================================================

### Self-Rating (Compared to Others) - Being a Leader ============================================================================

### Self-Rating (Compared to Others) - Logical/Analytic Thinking =================================================================

### Self-Rating (Compared to Others) - Helping Others Solve Their Problems =======================================================

### How Good at Problem Solving ==================================================================================================

### Self-Rating (Compared to Others) - Intelligence ==============================================================================

### Self-Rating (Compared to Others) - Independence ==============================================================================

### Self-Rating (Compared to Others) - Confidence ================================================================================

### Self-Rating (Compared to Others) - Decisiveness ==============================================================================

### Frequency - Snacking Instead of Consuming Regular Meals ======================================================================

### Binge Eating Frequency =======================================================================================================

### Race - Hispanic ==============================================================================================================

### Race Identification ==========================================================================================================

### Daily Cigarette Usage ========================================================================================================

### Body Mass Index (BMI) ========================================================================================================

### Usual Amount of Daily Sleep ==================================================================================================

######################## TIAS-D Analysis - TIAS 2017 ######################## 

### Amphetamine Usage ===========================================================================================================

### Barbiturate Usage ===========================================================================================================

### Marijuana Usage =============================================================================================================

### Steroid Usage ===============================================================================================================

### Tranquilizer Usage ==========================================================================================================

### Cocaine Usage ===============================================================================================================

### Average Alcohol Consumption Frequency Over Past Year ========================================================================

### Average Daily Alcohol Consumption ===========================================================================================

### Degree to Which Condition Limits Normal Daily Activities ====================================================================

### Depression Over Past Year ===================================================================================================

### Depression - Anhedonia =======================================================================================================

### Depression Diagnosis =========================================================================================================

### Bipolar Disorder Diagnosis ===================================================================================================

### Phobia Diagnosis =============================================================================================================

### Anxiety Disorders Diagnosis ==================================================================================================

### OCD Diagnosis ================================================================================================================

### Mental Health: Non-Spec Psych Distress =======================================================================================

### Mental Health: Social Anxiety ================================================================================================

### Mental Health: Worry =========================================================================================================

### Paid Employment Since Jan 1 of Prior Year ====================================================================================

### Paid Employment History ======================================================================================================

### Current Marital Status =======================================================================================================

### Romantic Relationships =======================================================================================================

### Number of Children ===========================================================================================================

### High School Education ========================================================================================================

### College Education ============================================================================================================

### Highest Education Level ======================================================================================================

### Responsibility - Earning Own Living ==========================================================================================

### Responsibility - Paying Own Rent =============================================================================================

### Responsibility - Paying Own Bills ============================================================================================

### Responsibility - Managing Money ==============================================================================================

### How Good at Taking Responsibility for Actions ================================================================================

### How Good at Money Management =================================================================================================

### How Good at Paying Off Credit Card Balances ==================================================================================

### MIDUS M1 - Happiness =========================================================================================================

### MIDUS M2 - Interest in Life ==================================================================================================

### MIDUS M3 - Satisfaction ======================================================================================================

### MIDUS M4 - Contribution of Something Important to Society ====================================================================

### MIDUS M5 - Belonging to Community ============================================================================================

### MIDUS M6 - Society Getting Better ============================================================================================

### MIDUS M7 - People Basically Good =============================================================================================

### MIDUS M8 - Way Society Makes Sense ===========================================================================================

### MIDUS M9 - Managing Daily Responsibility =====================================================================================

### MIDUS M10 - Trusting Relationships w/ Others =================================================================================

### MIDUS M11 - Challenged to Grow ===============================================================================================

### MIDUS M12 - Confident of Own Ideas ===========================================================================================

### MIDUS M13 - Liked Personality ================================================================================================

### MIDUS M14 - Life Had Direction ===============================================================================================

### MIDUS Subscale - Emotional ===================================================================================================

### MIDUS Subscale - Psychological ===============================================================================================

### Time Use - Involved in Arts ==================================================================================================

### Time Use - Type of Art (Involvement) =========================================================================================

### Time Use - Art Involvement Frequency (Over 12 Mos) ===========================================================================

### Time Use - Involved in Sports ================================================================================================

### Time Use - Watching/Reading News (Over 12 Mos) ===============================================================================

### Time Use - Reading for Pleasure (Over 12 Mos) ================================================================================

### Time Use - Watching Non-News TV Shows (Over 12 Mos) ==========================================================================

### Time Use - Internet Usage ====================================================================================================

### Time Use - Internet Usage for Email/Instant Messenger (Over 12 Mos) ==========================================================

### Time Use - Internet Usage for School/Research (Over 12 Mos) ==================================================================

### Time Use - Internet Usage for Shopping (Over 12 Mos) =========================================================================

### Time Use - Internet Usage for Playing Games (Over 12 Mos) ====================================================================

### Time Use - Voted in Last Presidential Election ===============================================================================

### Time Use - Involvement in Social Action Groups ===============================================================================

### Time Use - Type of Social Action Group (If Involved) =========================================================================

### Time Use - Social Action Group Involvement (Over 12 Mos) =====================================================================

### Time Use - School Club/Student Gov Involvement ===============================================================================

### Time Use - School Club/Student Gov Involvement Frequency (Over 12 Mos) =======================================================

### Time Use - Unpaid Volunteer/Community Sevice Work (Over 12 Mos) ==============================================================

### Time Use - Type of Volunteer Work ============================================================================================

### Time Use - How Often Volunteered (Over 12 Mos) ===============================================================================

### Self-Rating (Compared to Others) - Listening & Understanding Others ==========================================================

### Self-Rating (Compared to Others) - Teaching & Explaining to Others ===========================================================

### Self-Rating (Compared to Others) - Supervising Others ========================================================================

### Self-Rating (Compared to Others) - Being a Leader ============================================================================

### Self-Rating (Compared to Others) - Logical/Analytic Thinking =================================================================

### Self-Rating (Compared to Others) - Helping Others Solve Their Problems =======================================================

### How Good at Problem Solving ==================================================================================================

### Self-Rating (Compared to Others) - Intelligence ==============================================================================

### Self-Rating (Compared to Others) - Independence ==============================================================================

### Self-Rating (Compared to Others) - Confidence ================================================================================

### Self-Rating (Compared to Others) - Decisiveness ==============================================================================

### Frequency - Snacking Instead of Consuming Regular Meals ======================================================================

### Binge Eating Frequency =======================================================================================================

### Race - Hispanic ==============================================================================================================

### Race Identification ==========================================================================================================

### Daily Cigarette Usage ========================================================================================================

### Body Mass Index (BMI) ========================================================================================================

### Usual Amount of Daily Sleep ==================================================================================================

######################## TIAS-D Analysis - TIAS 2005-2017 ######################## 

### Amphetamine Usage ===========================================================================================================

### Barbiturate Usage ===========================================================================================================

### Marijuana Usage =============================================================================================================

### Steroid Usage ===============================================================================================================

### Tranquilizer Usage ==========================================================================================================

### Cocaine Usage ===============================================================================================================

### Average Alcohol Consumption Frequency Over Past Year ========================================================================

### Average Daily Alcohol Consumption ===========================================================================================

### Degree to Which Condition Limits Normal Daily Activities ====================================================================

### Depression Over Past Year ===================================================================================================

### Depression - Anhedonia =======================================================================================================

### Depression Diagnosis =========================================================================================================

### Bipolar Disorder Diagnosis ===================================================================================================

### Phobia Diagnosis =============================================================================================================

### Anxiety Disorders Diagnosis ==================================================================================================

### OCD Diagnosis ================================================================================================================

### Mental Health: Non-Spec Psych Distress =======================================================================================

### Mental Health: Social Anxiety ================================================================================================

### Mental Health: Worry =========================================================================================================

### Paid Employment Since Jan 1 of Prior Year ====================================================================================

### Paid Employment History ======================================================================================================

### Current Marital Status =======================================================================================================

### Romantic Relationships =======================================================================================================

### Number of Children ===========================================================================================================

### High School Education ========================================================================================================

### College Education ============================================================================================================

### Highest Education Level ======================================================================================================

### Responsibility - Earning Own Living ==========================================================================================

### Responsibility - Paying Own Rent =============================================================================================

### Responsibility - Paying Own Bills ============================================================================================

### Responsibility - Managing Money ==============================================================================================

### How Good at Taking Responsibility for Actions ================================================================================

### How Good at Money Management =================================================================================================

### How Good at Paying Off Credit Card Balances ==================================================================================

### MIDUS M1 - Happiness =========================================================================================================

### MIDUS M2 - Interest in Life ==================================================================================================

### MIDUS M3 - Satisfaction ======================================================================================================

### MIDUS M4 - Contribution of Something Important to Society ====================================================================

### MIDUS M5 - Belonging to Community ============================================================================================

### MIDUS M6 - Society Getting Better ============================================================================================

### MIDUS M7 - People Basically Good =============================================================================================

### MIDUS M8 - Way Society Makes Sense ===========================================================================================

### MIDUS M9 - Managing Daily Responsibility =====================================================================================

### MIDUS M10 - Trusting Relationships w/ Others =================================================================================

### MIDUS M11 - Challenged to Grow ===============================================================================================

### MIDUS M12 - Confident of Own Ideas ===========================================================================================

### MIDUS M13 - Liked Personality ================================================================================================

### MIDUS M14 - Life Had Direction ===============================================================================================

### MIDUS Subscale - Emotional ===================================================================================================

### MIDUS Subscale - Psychological ===============================================================================================

### Time Use - Involved in Arts ==================================================================================================

### Time Use - Type of Art (Involvement) =========================================================================================

### Time Use - Art Involvement Frequency (Over 12 Mos) ===========================================================================

### Time Use - Involved in Sports ================================================================================================

### Time Use - Watching/Reading News (Over 12 Mos) ===============================================================================

### Time Use - Reading for Pleasure (Over 12 Mos) ================================================================================

### Time Use - Watching Non-News TV Shows (Over 12 Mos) ==========================================================================

### Time Use - Internet Usage ====================================================================================================

### Time Use - Internet Usage for Email/Instant Messenger (Over 12 Mos) ==========================================================

### Time Use - Internet Usage for School/Research (Over 12 Mos) ==================================================================

### Time Use - Internet Usage for Shopping (Over 12 Mos) =========================================================================

### Time Use - Internet Usage for Playing Games (Over 12 Mos) ====================================================================

### Time Use - Voted in Last Presidential Election ===============================================================================

### Time Use - Involvement in Social Action Groups ===============================================================================

### Time Use - Type of Social Action Group (If Involved) =========================================================================

### Time Use - Social Action Group Involvement (Over 12 Mos) =====================================================================

### Time Use - School Club/Student Gov Involvement ===============================================================================

### Time Use - School Club/Student Gov Involvement Frequency (Over 12 Mos) =======================================================

### Time Use - Unpaid Volunteer/Community Sevice Work (Over 12 Mos) ==============================================================

### Time Use - Type of Volunteer Work ============================================================================================

### Time Use - How Often Volunteered (Over 12 Mos) ===============================================================================

### Self-Rating (Compared to Others) - Listening & Understanding Others ==========================================================

### Self-Rating (Compared to Others) - Teaching & Explaining to Others ===========================================================

### Self-Rating (Compared to Others) - Supervising Others ========================================================================

### Self-Rating (Compared to Others) - Being a Leader ============================================================================

### Self-Rating (Compared to Others) - Logical/Analytic Thinking =================================================================

### Self-Rating (Compared to Others) - Helping Others Solve Their Problems =======================================================

### How Good at Problem Solving ==================================================================================================

### Self-Rating (Compared to Others) - Intelligence ==============================================================================

### Self-Rating (Compared to Others) - Independence ==============================================================================

### Self-Rating (Compared to Others) - Confidence ================================================================================

### Self-Rating (Compared to Others) - Decisiveness ==============================================================================

### Frequency - Snacking Instead of Consuming Regular Meals ======================================================================

### Binge Eating Frequency =======================================================================================================

### Race - Hispanic ==============================================================================================================

### Race Identification ==========================================================================================================

### Daily Cigarette Usage ========================================================================================================

### Body Mass Index (BMI) ========================================================================================================

### Usual Amount of Daily Sleep ==================================================================================================
