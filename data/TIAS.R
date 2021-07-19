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

# Creating a subsetted dataframe including only FTL participants for the 2005 wave 

TIAS2005_FTL <- subset(TIAS2005, CAT == "FTL_05")

# Creating a subsetted dataframe including only IAC participants for the 2005 wave 

TIAS2005_IAC <- subset(TIAS2005, CAT == "IAC_05")

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

### TIAS FTL Wave Count =========================================================================================================

# Creating a variable for the total number of waves that each participant identified as FTL (as opposed to IAC or NA)

TIAS$FTL_COUNT <- as.integer(as.logical(TIAS$CAT_05 == "FTL_05")) + as.integer(as.logical(TIAS$CAT_07 == "FTL_07")) + as.integer(as.logical(TIAS$CAT_09 == "FTL_09")) +
  as.integer(as.logical(TIAS$CAT_11 == "FTL_11")) + as.integer(as.logical(TIAS$CAT_13 == "FTL_13")) + as.integer(as.logical(TIAS$CAT_15 == "FTL_15")) + as.integer(as.logical(TIAS$CAT_17 == "FTL_17"))

# Viewing the distribution of FTL wave counts 

table(TIAS$FTL_COUNT)                        

# Creating a variable distinguishing participants who identified as FTL for at least one wave (greater than or equal to 1; GREQ1) vs. who never identified as FTL

TIAS$GREQ1_FTL <- with(TIAS, ifelse(FTL_COUNT >= 1, "Yes", "No"))      

table(TIAS$GREQ1_FTL)

######################## TIAS-D Analysis - TIAS 2005 ######################## 

### Amphetamine Usage =========================================================================================================== 

####
# H44B. # of Times Took w/o Doc in Past 12mos: "On how many occasions (if any) have you taken/used amphetamine on your own
# w/o a doctor telling you to take them during the last 12 months?" 
# Answers: 1 (1-2); 2 (3-5); 3 (6-9); 4 (10-19); 5 (20-39); 6 (40 or more); 8 (DK); 9 (NA/refused); 0 (Inap: 0 in past 12 mos or never)
#### 

table(TIAS$TA050784)

T05_AMP_FTLW <- TIAS[, c("TA050784", "FTL_COUNT")] %>% group_by(TA050784, FTL_COUNT) %>% summarise(Count = n())

T05_AMP_FTLW <- T05_AMP_FTLW[1:11,]

T05_AMP_CAT <- TIAS2005[, c("TA050784", "CAT")] %>% group_by(TA050784, CAT) %>% summarise(Count = n())

head(T05_AMP_CAT, 7)

ggplot(T05_AMP_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050784)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Amphetamine Usage (Prev. Year)", values = c("darkseagreen2", "darkslategray2", "lightgoldenrod1", "lightsalmon", "lightpink1", "mediumpurple1"), 
                    labels = c("Never", "1-2 times", "3-5 times", "6-9 times", "20-39 times", "40 or more times"))

head(T05_AMP_FTLW, 11)

ggplot(T05_AMP_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050784)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Amphetamine Usage (Prev. Year)", values = c("darkseagreen2", "darkslategray2", "lightgoldenrod1", "lightsalmon", "lightpink1", "mediumpurple1"), 
                    labels = c("Never", "1-2 times", "3-5 times", "6-9 times", "20-39 times", "40 or more times"))

### Barbiturate Usage =========================================================================================================== 

####
# H44E. # of Times Took w/o Doc in Past 12mos: "On how many occasions (if any) have you taken/used barbiturates on your own
# w/o a doctor telling you to take them during the last 12 months?" 
# Answers: 1 (1-2); 2 (3-5); 3 (6-9); 4 (10-19); 5 (20-39); 6 (40 or more); 8 (DK); 9 (NA/refused); 0 (Inap: 0 in past 12 mos or never)
#### 

table(TIAS$TA050808)

T05_BRB_FTLW <- TIAS[, c("TA050808", "FTL_COUNT")] %>% group_by(TA050808, FTL_COUNT) %>% summarise(Count = n())

T05_BRB_FTLW <- T05_BRB_FTLW[1:9, ]

T05_BRB_CAT <- TIAS2005[, c("TA050808", "CAT")] %>% group_by(TA050808, CAT) %>% summarise(Count = n())

head(T05_BRB_CAT, 5)

ggplot(T05_BRB_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050808)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Barbiturate Usage (Prev. Year)", values = c("darkseagreen2", "darkslategray2", "lightgoldenrod1", "lightpink1"), 
                    labels = c("Never", "3-5 times", "10-19 times", "40 or more times"))

head(T05_BRB_FTLW, 9)

ggplot(T05_BRB_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050808)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Barbiturate Usage (Prev. Year)", values = c("darkseagreen2", "darkslategray2", "lightgoldenrod1", "lightpink1"), 
                    labels = c("Never", "3-5 times", "10-19 times", "40 or more times"))

### Marijuana Usage ============================================================================================================= 

####
# H44C. # of Times Took w/o Doc in Past 12mos: "On how many occasions (if any) have you taken/used marijuana on your own
# w/o a doctor telling you to take them during the last 12 months?" 
# Answers: 1 (1-2); 2 (3-5); 3 (6-9); 4 (10-19); 5 (20-39); 6 (40 or more); 8 (DK); 9 (NA/refused); 0 (Inap: 0 in past 12 mos or never)
#### 

table(TIAS$TA050792)

T05_MAR_FTLW <- TIAS[, c("TA050792", "FTL_COUNT")] %>% group_by(TA050792, FTL_COUNT) %>% summarise(Count = n())

T05_MAR_FTLW <- T05_MAR_FTLW[1:7, ]

T05_MAR_CAT <- TIAS2005[, c("TA050792", "CAT")] %>% group_by(TA050792, CAT) %>% summarise(Count = n())

head(T05_MAR_CAT, 3)

ggplot(T05_MAR_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050792)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Marijuana Usage (Prev. Year)", values = c("darkseagreen2", "darkslategray2"), labels = c("Never", "40 or more times"))

head(T05_MAR_FTLW, 7)

ggplot(T05_MAR_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050792)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") +
  scale_fill_manual("Marijuana Usage (Prev. Year)", values = c("darkseagreen2", "darkslategray2"), labels = c("Never", "40 or more times"))

### Diet Pill Usage ============================================================================================================= 

####
# H44A. # of Times Took w/o Doc in Past 12mos: "On how many occasions (if any) have you taken/used diet pills on your own
# w/o a doctor telling you to take them during the last 12 months?" 
# Answers: 1 (1-2); 2 (3-5); 3 (6-9); 4 (10-19); 5 (20-39); 6 (40 or more); 8 (DK); 9 (NA/refused); 0 (Inap: 0 in past 12 mos or never)
#### 

table(TIAS$TA050776)

T05_DIP_FTLW <- TIAS[, c("TA050776", "FTL_COUNT")] %>% group_by(TA050776, FTL_COUNT) %>% summarise(Count = n())

T05_DIP_FTLW <- T05_DIP_FTLW[1:7, ]

T05_DIP_CAT <- TIAS2005[, c("TA050776", "CAT")] %>% group_by(TA050776, CAT) %>% summarise(Count = n())

head(T05_DIP_CAT, 3)

ggplot(T05_DIP_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050776)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Diet Pill Usage (Prev. Year)", values = c("darkseagreen2", "darkslategray2"), labels = c("Never", "1-2 times")) 

head(T05_DIP_FTLW, 7)

ggplot(T05_DIP_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050776)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") +
  scale_fill_manual("Diet Pill Usage (Prev. Year)", values = c("darkseagreen2", "darkslategray2"), labels = c("Never", "1-2 times")) 
  
### Steroid Usage =============================================================================================================== 

####
# H44G. # of Times Took w/o Doc in Past 12mos: "On how many occasions (if any) have you taken/used steroids on your own
# w/o a doctor telling you to take them during the last 12 months?" 
# Answers: 1 (1-2); 2 (3-5); 3 (6-9); 4 (10-19); 5 (20-39); 6 (40 or more); 8 (DK); 9 (NA/refused); 0 (Inap: 0 in past 12 mos or never)
#### 

table(TIAS$TA050824)

T05_STR_FTLW <- TIAS[, c("TA050824", "FTL_COUNT")] %>% group_by(TA050824, FTL_COUNT) %>% summarise(Count = n())

T05_STR_FTLW <- T05_STR_FTLW[1:7, ]

T05_STR_CAT <- TIAS2005[, c("TA050824", "CAT")] %>% group_by(TA050824, CAT) %>% summarise(Count = n())

head(T05_STR_CAT, 3)

ggplot(T05_STR_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050824)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Steroid Usage (Prev. Year)", values = c("darkseagreen2", "darkslategray2"), labels = c("Never", "20-39 times")) 

head(T05_STR_FTLW, 7)

ggplot(T05_STR_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050824)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  scale_fill_manual("Steroid Usage (Prev. Year)", values = c("darkseagreen2", "darkslategray2"), labels = c("Never", "20-39 times")) +
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") 

### Tranquilizer Usage ========================================================================================================== 

####
# H44F. # of Times Took w/o Doc in Past 12mos: "On how many occasions (if any) have you taken/used tranquilizers on your own
# w/o a doctor telling you to take them during the last 12 months?" 
# Answers: 1 (1-2); 2 (3-5); 3 (6-9); 4 (10-19); 5 (20-39); 6 (40 or more); 8 (DK); 9 (NA/refused); 0 (Inap: 0 in past 12 mos or never)
#### 

table(TIAS$TA050816)

table(TIAS$TA050816, TIAS$FTL_COUNT)

T05_TRQ_FTLW <- TIAS[, c("TA050816", "FTL_COUNT")] %>% group_by(TA050816, FTL_COUNT) %>% summarise(Count = n())

T05_TRQ_FTLW <- T05_TRQ_FTLW[1:11, ]

T05_TRQ_CAT <- TIAS2005[, c("TA050816", "CAT")] %>% group_by(TA050816, CAT) %>% summarise(Count = n())

T05_TRQ_FTLCAT <- TIAS2005_FTL[, c("TA050816", "CAT")] %>% group_by(TA050816, CAT) %>% summarise(Count = n())

T05_TRQ_IACCAT <- TIAS2005_IAC[, c("TA050816", "CAT")] %>% group_by(TA050816, CAT) %>% summarise(Count = n())

head(T05_TRQ_CAT, 7)

ggplot(T05_TRQ_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050816)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2005", x = "Category", y = "Count") +
  scale_fill_manual("Tranquilizer Usage (Prev. Year)", values = c("darkseagreen2", "darkslategray2", "lightgoldenrod1", "lightsalmon", "lightpink1"), 
                    labels = c("Never", "1-2 times", "3-5 times", "6-9 times", "10-19 times"))

head(T05_TRQ_FTLW, 11)

ggplot(T05_TRQ_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050816)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Tranquilizer Usage (Prev. Year)", values = c("darkseagreen2", "darkslategray2", "lightgoldenrod1", "lightsalmon", "lightpink1"), 
                    labels = c("Never", "1-2 times", "3-5 times", "6-9 times", "10-19 times"))

prop.table(table(TIAS2005_FTL$TA050816))

trq.pie.ftl.05 <- ggplot(data = T05_TRQ_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050816))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0)  + 
  scale_fill_manual("Tranquilizer Usage (Prev. Year)", values = c("darkseagreen2", "lightgoldenrod1"), labels = c("Never", "3-5 times")) + 
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050816))

trq.pie.iac.05 <- ggplot(data = T05_TRQ_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050816))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("Tranquilizer Usage (Prev. Year)", values = c("darkseagreen2", "darkslategray2", "lightgoldenrod1", "lightsalmon", "lightpink1"), 
                    labels = c("Never", "1-2 times", "3-5 times", "6-9 times", "10-19 times"))                    

ggarrange(trq.pie.ftl.05, trq.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### Cocaine Usage =============================================================================================================== 

####
# H42D_B. # of Occasions in Past 12mos: "On how many occasions (if any) have you used cocaine in the past 12 months?"
# Answers: 1 (1-2); 2 (3-5); 3 (6-9); 4 (10-19); 5 (20-39); 6 (40 or more); 8 (DK); 9 (NA/refused); 0 (Inap: 0 in past 12 mos or never)
#### 

table(TIAS$TA050797)

T05_CCN_FTLW <- TIAS[, c("TA050797", "FTL_COUNT")] %>% group_by(TA050797, FTL_COUNT) %>% summarise(Count = n())

T05_CCN_FTLW <- T05_CCN_FTLW[1:15, ]

T05_CCN_CAT <- TIAS2005[, c("TA050797", "CAT")] %>% group_by(TA050797, CAT) %>% summarise(Count = n())

T05_CCN_FTLCAT <- TIAS2005_FTL[, c("TA050797", "CAT")] %>% group_by(TA050797, CAT) %>% summarise(Count = n())

T05_CCN_IACCAT <- TIAS2005_IAC[, c("TA050797", "CAT")] %>% group_by(TA050797, CAT) %>% summarise(Count = n())

table(TIAS2005$TA050797, TIAS2005$CAT)

head(T05_CCN_CAT, 9)

ggplot(T05_CCN_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050797))) + 
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Cocaine Usage (Prev. Year)", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1", "hotpink"), 
                    labels = c("Never", "1-2 times", "3-5 times", "6-9 times", "10-19 times", "20-39 times", "40 or more times"))

head(T05_CCN_FTLW, 15)

ggplot(T05_CCN_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050797)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Cocaine Usage (Prev. Year)", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1", "hotpink"), 
                    labels = c("Never", "1-2 times", "3-5 times", "6-9 times", "10-19 times", "20-39 times", "40 or more times"))

prop.table(table(TIAS2005_FTL$TA050797))

ccn.pie.ftl.05 <- ggplot(data = T05_CCN_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050797))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_manual("Cocaine Usage (Prev. Year)", values = c("lightcoral", "gold"), labels = c("Never", "1-2 times")) +
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050797))

ccn.pie.iac.05 <- ggplot(data = T05_CCN_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050797))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_manual("Cocaine Usage (Prev. Year)", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1", "hotpink"), 
  labels = c("Never", "1-2 times", "3-5 times", "6-9 times", "10-19 times", "20-39 times", "40 or more times")) + 
  theme_void() 

ggarrange(ccn.pie.ftl.05, ccn.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))      

### Average Alcohol Consumption Frequency Over Past Year ======================================================================== 

#### 
# H37. How Often Have Drinks-HD: “In the last year, on average, how often did you have any alcohol to drink? Would you say: 
# less than once a month, about once a month, several times a month, about once a week, several times a week, or every day?”
# Answers: 1 (Less than once a month); 2 (About once a month); 3 (Several times a month); 4 (About once a week); 5 (Several times a week); 
# 6 (Every day); 8 (DK); 9 (NA/refused); 0 (Inap: Does not drink alcohol)
####

table(TIAS$TA050767)

T05_AAC_FTLW <- TIAS[, c("TA050767", "FTL_COUNT")] %>% group_by(TA050767, FTL_COUNT) %>% summarise(Count = n())

T05_AAC_FTLW <- T05_AAC_FTLW[1:23, ]

T05_AAC_CAT <- TIAS2005[, c("TA050767", "CAT")] %>% group_by(TA050767, CAT) %>% summarise(Count = n())

T05_AAC_FTLCAT <- TIAS2005_FTL[, c("TA050767", "CAT")] %>% group_by(TA050767, CAT) %>% summarise(Count = n())

T05_AAC_IACCAT <- TIAS2005_IAC[, c("TA050767", "CAT")] %>% group_by(TA050767, CAT) %>% summarise(Count = n())

head(T05_AAC_CAT, 12)

ggplot(T05_AAC_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050767))) + 
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Avg. Alcohol Consumption (Prev. Year)", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1", "hotpink"), 
                    labels = c("Never", "Less than once a month", "About once a month", "Several times a month", "About once a week", "Several times a week", "Every day"))

head(T05_AAC_FTLW, 23)

ggplot(T05_AAC_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050767)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Avg. Alcohol Consumption (Prev. Year)", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1", "hotpink"), 
                    labels = c("Never", "Less than once a month", "About once a month", "Several times a month", "About once a week", "Several times a week", "Every day"))

prop.table(table(TIAS2005_FTL$TA050767))

aac.pie.ftl.05 <- ggplot(data = T05_AAC_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050767))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Avg. Alcohol Consumption (Prev. Year)", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue"), 
  labels = c("Never", "Less than once a month", "About once a month", "Several times a month", "About once a week")) +
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050767))

aac.pie.iac.05 <- ggplot(data = T05_AAC_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050767))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_manual("Avg. Alcohol Consumption (Prev. Year)", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1", "hotpink"), 
  labels = c("Never", "Less than once a month", "About once a month", "Several times a month", "About once a week", "Several times a week", "Every day")) +
  theme_void() 

ggarrange(aac.pie.ftl.05, aac.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))        

### Average Daily Alcohol Consumption =========================================================================================== 

#### 
# H38. # Alcoholic Drinks Per Day: "In the last year, on the days you drank, about how many drinks did you have?”
# Answers: 1 (One drink or fewer); 2-50 (Actual number of drinks); 98 (DK); 99 (NA/refused); 0 (Inap.: Does not drink alcohol or drinks alcohol but frequency of drinking is DK or NA)
####

table(TIAS$TA050768)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050768 = 98)) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050768 = 98)) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050768 = 98)) 

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050768 = 98))  

T05_DAC_FTLW <- TIAS[, c("TA050768", "FTL_COUNT")] %>% group_by(TA050768, FTL_COUNT) %>% summarise(Count = n())

T05_DAC_FTLW <- T05_DAC_FTLW[1:32, ]

T05_DAC_CAT <- TIAS2005[, c("TA050768", "CAT")] %>% group_by(TA050768, CAT) %>% summarise(Count = n())

T05_DAC_CAT <- T05_DAC_CAT[1:20, ]

T05_DAC_FTLCAT <- TIAS2005_FTL[, c("TA050768", "CAT")] %>% group_by(TA050768, CAT) %>% summarise(Count = n())

T05_DAC_FTLCAT <- T05_DAC_FTLCAT[1:5, ]

T05_DAC_IACCAT <- TIAS2005_IAC[, c("TA050768", "CAT")] %>% group_by(TA050768, CAT) %>% summarise(Count = n())

T05_DAC_IACCAT <- T05_DAC_IACCAT[1:15, ]

head(T05_DAC_CAT, 20)

ggplot(T05_DAC_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050768))) + 
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Avg. Daily Alcohol Consumption", values =  c("#E69F00", "#56B4E9", "#009E73", "gold", "#0072B2", "#D55E00", "#CC79A7", "lightcoral", "wheat", "green3", "mediumturquoise", 
  "deepskyblue", "mediumpurple1", "hotpink", "khaki1"), labels = c("Never", "1 or fewer drinks", "2 drinks", "3 drinks", "4 drinks", "5 drinks", "6 drinks", "7 drinks", "8 drinks", 
  "9 drinks", "10 drinks", "11 drinks", "12 drinks", "15 drinks", "20 drinks"))

head(T05_DAC_FTLW, 32)

ggplot(T05_DAC_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050768)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Avg. Daily Alcohol Consumption", values =  c("#E69F00", "#56B4E9", "#009E73", "gold", "#0072B2", "#D55E00", "#CC79A7", "lightcoral", "wheat", "green3", "mediumturquoise", 
  "deepskyblue", "mediumpurple1", "hotpink", "khaki1"), labels = c("Never", "1 or fewer drinks", "2 drinks", "3 drinks", "4 drinks", "5 drinks", "6 drinks", "7 drinks", "8 drinks", 
  "9 drinks", "10 drinks", "11 drinks", "12 drinks", "15 drinks", "20 drinks"))

prop.table(table(TIAS2005_FTL$TA050768))

dac.pie.ftl.05 <- ggplot(data = T05_DAC_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050768))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Avg. Daily Alcohol Consumption", values =  c("#E69F00", "#56B4E9", "#009E73", "gold", "#D55E00"), labels = c("Never", "1 or fewer drinks", "2 drinks", "3 drinks", "5 drinks")) +
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050768))

dac.pie.iac.05 <- ggplot(data = T05_DAC_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050768))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_manual("Avg. Daily Alcohol Consumption", values =  c("#E69F00", "#56B4E9", "#009E73", "gold", "#0072B2", "#D55E00", "#CC79A7", "lightcoral", "wheat", "green3", "mediumturquoise", 
  "deepskyblue", "mediumpurple1", "hotpink", "khaki1"), labels = c("Never", "1 or fewer drinks", "2 drinks", "3 drinks", "4 drinks", "5 drinks", "6 drinks", "7 drinks", "8 drinks", 
  "9 drinks", "10 drinks", "11 drinks", "12 drinks", "15 drinks", "20 drinks")) +
  theme_void() 

ggarrange(dac.pie.ftl.05, dac.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))          

### Degree to Which Condition Limits Normal Daily Activities ==================================================================== 

####
# H13B. How much limits normal activities: "How much does this condition limit your normal daily activities? Would you say: A lot, somewhat, just a little, or not at all?”
# Answers: 1 (A lot); 3 (Somewhat); 5 (Just a little); 7 (Not at all); 8 (DK); 9 (NA; refused); 0 (Inap: Never diagnosed with any serious chronic condition)
####

table(TIAS$TA050723)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050723 = 9)) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050723 = 9)) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050723 = 9)) 

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050723 = 9)) 

T05_DCN_FTLW <- TIAS[, c("TA050723", "FTL_COUNT")] %>% group_by(TA050723, FTL_COUNT) %>% summarise(Count = n())

T05_DCN_FTLW <- T05_DCN_FTLW[1:14, ]

T05_DCN_CAT <- TIAS2005[, c("TA050723", "CAT")] %>% group_by(TA050723, CAT) %>% summarise(Count = n())

T05_DCN_CAT <- T05_DCN_CAT[1:8, ]

T05_DCN_FTLCAT <- TIAS2005_FTL[, c("TA050723", "CAT")] %>% group_by(TA050723, CAT) %>% summarise(Count = n())

T05_DCN_IACCAT <- TIAS2005_IAC[, c("TA050723", "CAT")] %>% group_by(TA050723, CAT) %>% summarise(Count = n())

T05_DCN_IACCAT <- T05_DCN_IACCAT[1:5, ]

head(T05_DCN_CAT, 8)

ggplot(T05_DCN_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050723)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Condition Limits Daily Activities", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue"), 
                    labels = c("Inap: No chronic condition", "A lot", "Somewhat", "Just a little", "Not at all"))

head(T05_DCN_FTLW, 14)

ggplot(T05_DCN_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050723)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Condition Limits Daily Activities", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue"), 
                    labels = c("Inap: No chronic condition", "A lot", "Somewhat", "Just a little", "Not at all"))

prop.table(table(TIAS2005_FTL$TA050723))

dcn.pie.ftl.05 <- ggplot(data = T05_DCN_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050723))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Condition Limits Daily Activities", values = c("lightcoral", "mediumturquoise", "deepskyblue"), 
  labels = c("Inap: No chronic condition", "Just a little", "Not at all")) +
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050723))

dcn.pie.iac.05 <- ggplot(data = T05_DCN_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050723))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Condition Limits Daily Activities", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue"), 
  labels = c("Inap: No chronic condition", "A lot", "Somewhat", "Just a little", "Not at all")) +
  theme_void() 

ggarrange(dcn.pie.ftl.05, dcn.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### Depression Over Past Year =================================================================================================== 

####
# H15. WTR>2 Wks Depressed In Past 12mos: “Now I want to ask you about periods of feeling sad, empty, or depressed. 
# In the past 12 months, have you had two weeks or longer when nearly every day you felt sad, empty, or depressed for most of the day?”
# Answers: 1 (Yes); 5 (No); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050733)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050733 = 9)) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050733 = 9)) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050733 = 9)) 

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050733 = 9)) 

T05_DEP_FTLW <- TIAS[, c("TA050733", "FTL_COUNT")] %>% group_by(TA050733, FTL_COUNT) %>% summarise(Count = n())

T05_DEP_FTLW <- T05_DEP_FTLW[1:10, ]

T05_DEP_CAT <- TIAS2005[, c("TA050733", "CAT")] %>% group_by(TA050733, CAT) %>% summarise(Count = n())

T05_DEP_CAT <- T05_DEP_CAT[1:4, ]

T05_DEP_FTLCAT <- TIAS2005_FTL[, c("TA050733", "CAT")] %>% group_by(TA050733, CAT) %>% summarise(Count = n())

T05_DEP_IACCAT <- TIAS2005_IAC[, c("TA050733", "CAT")] %>% group_by(TA050733, CAT) %>% summarise(Count = n())

T05_DEP_IACCAT <- T05_DEP_IACCAT[1:2, ]

head(T05_DEP_CAT, 4)

ggplot(T05_DEP_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050733)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2005", x = "Category", y = "Count") +
  scale_fill_manual("Depressed >2 Weeks", values = c("aquamarine3", "cadetblue2"), labels = c("Yes", "No"))

head(T05_DEP_FTLW, 10)

ggplot(T05_DEP_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050733)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") +
  scale_fill_manual("Depressed >2 Weeks", values = c("aquamarine3", "cadetblue2"), labels = c("Yes", "No"))

prop.table(table(TIAS2005_FTL$TA050733))

dep.pie.ftl.05 <- ggplot(data = T05_DEP_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050733))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Depressed >2 Weeks", values = c("aquamarine3", "cadetblue2"), labels = c("Yes", "No")) + 
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050733))

dep.pie.iac.05 <- ggplot(data = T05_DEP_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050733))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Depressed >2 Weeks", values = c("aquamarine3", "cadetblue2"), labels = c("Yes", "No")) + 
  theme_void() 

ggarrange(dep.pie.ftl.05, dep.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### Depression - Anhedonia ======================================================================================================= 

####
# H16. WTR>2 Wks No Interest in Life: “In the past 12 months, have you had two weeks or longer when you lost interest in most things 
# like work, hobbies, and other things you usually enjoyed?”
# Answers: 1 (Yes); 5 (No); 8 (DK); 9 (NA/refused)

table(TIAS$TA050734)

T05_ANH_FTLW <- TIAS[, c("TA050734", "FTL_COUNT")] %>% group_by(TA050734, FTL_COUNT) %>% summarise(Count = n())

T05_ANH_FTLW <- T05_ANH_FTLW[1:10,]

T05_ANH_CAT <- TIAS2005[, c("TA050734", "CAT")] %>% group_by(TA050734, CAT) %>% summarise(Count = n())

T05_ANH_FTLCAT <- TIAS2005_FTL[, c("TA050734", "CAT")] %>% group_by(TA050734, CAT) %>% summarise(Count = n())

T05_ANH_IACCAT <- TIAS2005_IAC[, c("TA050734", "CAT")] %>% group_by(TA050734, CAT) %>% summarise(Count = n())

table(TIAS2005$TA050734, TIAS2005$CAT)

ggplot(T05_ANH_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050734)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") +
  scale_fill_manual("Anhedonia >2 Weeks", values = c("cadetblue1", "lightsalmon"), labels = c("Yes", "No"))

prop.table(table(TIAS2005_FTL$TA050734))

anh.pie.ftl.05 <- ggplot(data = T05_ANH_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050734))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Anhedonia >2 Weeks", values = c("cadetblue1", "lightsalmon"), labels = c("Yes", "No")) + 
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050734))

anh.pie.iac.05 <- ggplot(data = T05_ANH_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050734))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Anhedonia >2 Weeks", values = c("cadetblue1", "lightsalmon"), labels = c("Yes", "No")) + 
  theme_void() 

ggarrange(anh.pie.ftl.05, anh.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### Depression Diagnosis ========================================================================================================= 

####
# H12B. WTR Depression: "What was the diagnosis? What is the emotional or psychiatric disorder?--DEPRESSION"
# Answers: 1 (Diagnosed); 8 (DK); 9 (NA/refused); 0 (Not Diagnosed)
####

table(TIAS$TA050710)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050710 = c(8, 9))) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050710 = c(8, 9))) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050710 = c(8, 9)))  

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050710 = c(8, 9))) 

T05_DPD_FTLW <- TIAS[, c("TA050710", "FTL_COUNT")] %>% group_by(TA050710, FTL_COUNT) %>% summarise(Count = n())

T05_DPD_FTLW <- T05_DPD_FTLW[1:9, ]

T05_DPD_CAT <- TIAS2005[, c("TA050710", "CAT")] %>% group_by(TA050710, CAT) %>% summarise(Count = n())

T05_DPD_CAT <- T05_DPD_CAT[1:3, ]

T05_DPD_FTLCAT <- TIAS2005_FTL[, c("TA050710", "CAT")] %>% group_by(TA050710, CAT) %>% summarise(Count = n())

T05_DPD_IACCAT <- TIAS2005_IAC[, c("TA050710", "CAT")] %>% group_by(TA050710, CAT) %>% summarise(Count = n())

T05_DPD_IACCAT <- T05_DPD_IACCAT[1:2, ]
  
head(T05_DPD_CAT, 3)

ggplot(T05_DPD_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050710)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") +
  scale_fill_manual("Depression Diagnosis", values = c("cadetblue1", "lightsalmon"), labels = c("Not Diagnosed", "Diagnosed"))

head(T05_DPD_FTLW, 9)

ggplot(T05_DPD_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050710)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Depression Diagnosis", values = c("cadetblue1", "lightsalmon"), labels = c("Not Diagnosed", "Diagnosed"))

### Bipolar Disorder Diagnosis =================================================================================================== 

####
# H12B. WTR Bipolar: "What was the diagnosis? What is the emotional or psychiatric disorder?--BIPOLAR DISORDER"
# Answers: 1 (Diagnosed); 8 (DK); 9 (NA/refused); 0 (Not Diagnosed)
####

table(TIAS$TA050711)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050711 = c(8, 9))) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050711 = c(8, 9))) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050711 = c(8, 9)))  

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050711 = c(8, 9))) 

T05_BIP_FTLW <- TIAS[, c("TA050711", "FTL_COUNT")] %>% group_by(TA050711, FTL_COUNT) %>% summarise(Count = n())

T05_BIP_FTLW <- T05_BIP_FTLW[1:7, ]

T05_BIP_CAT <- TIAS2005[, c("TA050711", "CAT")] %>% group_by(TA050711, CAT) %>% summarise(Count = n())

T05_BIP_CAT <- T05_BIP_CAT[1:3, ]

T05_BIP_FTLCAT <- TIAS2005_FTL[, c("TA050711", "CAT")] %>% group_by(TA050711, CAT) %>% summarise(Count = n())

T05_BIP_IACCAT <- TIAS2005_IAC[, c("TA050711", "CAT")] %>% group_by(TA050711, CAT) %>% summarise(Count = n())

T05_BIP_IACCAT <- T05_BIP_IACCAT[1:2, ]

head(T05_BIP_CAT, 3)

ggplot(T05_BIP_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050711)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") +
  scale_fill_manual("Bipolar Disorder Diagnosis", values = c("cadetblue1", "lightsalmon"), labels = c("Not Diagnosed", "Diagnosed"))

head(T05_BIP_FTLW, 7)

ggplot(T05_BIP_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050711)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Bipolar Disorder Diagnosis", values = c("cadetblue1", "lightsalmon"), labels = c("Not Diagnosed", "Diagnosed"))

### Phobia Diagnosis =============================================================================================================

####
# H12B. WTR Phobia: "What was the diagnosis? What is the emotional or psychiatric disorder?--PHOBIAS"
# Answers: 1 (Diagnosed); 8 (DK); 9 (NA/refused); 0 (Not Diagnosed)
####

table(TIAS$TA050714)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050714 = c(8, 9))) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050714 = c(8, 9))) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050714 = c(8, 9)))  

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050714 = c(8, 9))) 

T05_PHB_FTLW <- TIAS[, c("TA050714", "FTL_COUNT")] %>% group_by(TA050714, FTL_COUNT) %>% summarise(Count = n())

T05_PHB_FTLW <- T05_PHB_FTLW[1:7, ]

T05_PHB_CAT <- TIAS2005[, c("TA050714", "CAT")] %>% group_by(TA050714, CAT) %>% summarise(Count = n())

T05_PHB_CAT <- T05_PHB_CAT[1:3, ]

T05_PHB_FTLCAT <- TIAS2005_FTL[, c("TA050714", "CAT")] %>% group_by(TA050714, CAT) %>% summarise(Count = n())

T05_PHB_IACCAT <- TIAS2005_IAC[, c("TA050714", "CAT")] %>% group_by(TA050714, CAT) %>% summarise(Count = n())

T05_PHB_IACCAT <- T05_PHB_IACCAT[1:2, ]

head(T05_PHB_CAT, 3)

ggplot(T05_PHB_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050714)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") +
  scale_fill_manual("Phobia Diagnosis", values = c("cadetblue1", "lightsalmon"), labels = c("Not Diagnosed", "Diagnosed"))

head(T05_PHB_FTLW, 7)

ggplot(T05_PHB_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050714)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Phobia Diagnosis", values = c("cadetblue1", "lightsalmon"), labels = c("Not Diagnosed", "Diagnosed"))

### Anxiety Disorders Diagnosis ==================================================================================================

#### 
# H12B. What was the diagnosis? What is the emotional or psychiatric disorder? -- Anxiety 
# Answers: 1 (Diagnosed w/ Anxiety); 8 (DK); 9 (NA/refused); 
# 0 (Inap. Never Diagnosed w/ Anxiety or Never Diagnosed with Emotional/Nervous/Psychiatric Problems)
####

table(TIAS$TA050713)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050713 = c(8, 9))) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050713 = c(8, 9))) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050713 = c(8, 9)))  

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050713 = c(8, 9))) 

T05_ANX_FTLW <- TIAS[, c("TA050713", "FTL_COUNT")] %>% group_by(TA050713, FTL_COUNT) %>% summarise(Count = n())

T05_ANX_FTLW <- T05_ANX_FTLW[1:8, ]

T05_ANX_CAT <- TIAS2005[, c("TA050713", "CAT")] %>% group_by(TA050713, CAT) %>% summarise(Count = n())

T05_ANX_CAT <- T05_ANX_CAT[1:4, ]

T05_ANX_FTLCAT <- TIAS2005_FTL[, c("TA050713", "CAT")] %>% group_by(TA050713, CAT) %>% summarise(Count = n())

T05_ANX_IACCAT <- TIAS2005_IAC[, c("TA050713", "CAT")] %>% group_by(TA050713, CAT) %>% summarise(Count = n())

T05_ANX_IACCAT <- T05_ANX_IACCAT[1:2, ]

head(T05_ANX_CAT, 4)

ggplot(T05_ANX_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050713)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") +
  scale_fill_manual("Anxiety Disorder Diagnosis", values = c("cadetblue1", "lightsalmon"), labels = c("Not Diagnosed", "Diagnosed"))

head(T05_ANX_FTLW, 8)

ggplot(T05_ANX_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050713)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Anxiety Disorder Diagnosis", values = c("cadetblue1", "lightsalmon"), labels = c("Not Diagnosed", "Diagnosed"))

prop.table(table(TIAS2005_FTL$TA050713))

anx.pie.ftl.05 <- ggplot(data = T05_ANX_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050713))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Anxiety Disorder Diagnosis", values = c("cadetblue1", "lightsalmon"), labels = c("Not Diagnosed", "Diagnosed")) + 
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050713))

anx.pie.iac.05 <- ggplot(data = T05_ANX_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050713))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Anxiety Disorder Diagnosis", values = c("cadetblue1", "lightsalmon"), labels = c("Not Diagnosed", "Diagnosed")) + 
  theme_void() 

ggarrange(anx.pie.ftl.05, anx.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### OCD Diagnosis ================================================================================================================

#### 
# H12B. What was the diagnosis? What is the emotional or psychiatric disorder? -- Obsessive Compulsive Disorder 
# Answers: 1 (Diagnosed w/ OCD); 8 (DK); 9 (NA/refused); 
# 0 (Inap. Never Diagnosed w/ Anxiety or Never Diagnosed with Emotional/Nervous/Psychiatric Problems)
####

table(TIAS$TA050717)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050717 = c(8, 9))) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050717 = c(8, 9))) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050717 = c(8, 9)))  

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050717 = c(8, 9))) 

T05_OCD_FTLW <- TIAS[, c("TA050717", "FTL_COUNT")] %>% group_by(TA050717, FTL_COUNT) %>% summarise(Count = n())

T05_OCD_FTLW <- T05_OCD_FTLW[1:7, ]

T05_OCD_CAT <- TIAS2005[, c("TA050717", "CAT")] %>% group_by(TA050717, CAT) %>% summarise(Count = n())

T05_OCD_CAT <- T05_OCD_CAT[1:3, ]

T05_OCD_FTLCAT <- TIAS2005_FTL[, c("TA050717", "CAT")] %>% group_by(TA050717, CAT) %>% summarise(Count = n())

T05_OCD_IACCAT <- TIAS2005_IAC[, c("TA050717", "CAT")] %>% group_by(TA050717, CAT) %>% summarise(Count = n())

T05_OCD_IACCAT <- T05_OCD_IACCAT[1:2, ]

head(T05_OCD_CAT, 3)

ggplot(T05_OCD_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050717)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") +
  scale_fill_manual("OCD Diagnosis", values = c("cadetblue1", "lightsalmon"), labels = c("Not Diagnosed", "Diagnosed"))

head(T05_OCD_FTLW, 7)

ggplot(T05_OCD_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050717)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("OCD Diagnosis", values = c("cadetblue1", "lightsalmon"), labels = c("Not Diagnosed", "Diagnosed"))

### Mental Health: Non-Spec Psych Distress ======================================================================================= 

####
# Cumulative score from answers to NSPD scale questions: 
# (how often felt nervous/hopeless/restless/everything as an effort/sad/worthless in past mo.)
# Possible scores: 0-24; 99 (all items are DK/NA/refused)
####

table(TIAS$TA050938)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050938 = 99)) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050938 = 99)) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050938 = 99)) 

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050938 = 99)) 

T05_NPD_FTLW <- TIAS[, c("TA050938", "FTL_COUNT")] %>% group_by(TA050938, FTL_COUNT) %>% summarise(Count = n())

T05_NPD_FTLW <- T05_NPD_FTLW[1:55, ]

T05_NPD_CAT <- TIAS2005[, c("TA050938", "CAT")] %>% group_by(TA050938, CAT) %>% summarise(Count = n())

T05_NPD_CAT <- T05_NPD_CAT[1:36, ]

ggplot(T05_NPD_FTLW, aes(x = FTL_COUNT, y = TA050938, group = FTL_COUNT, fill = as.factor(FTL_COUNT))) +
  geom_boxplot() + 
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 1)) + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "NSPD Scale Score") + 
  guides(fill = guide_legend(title = "# of FTL Waves"))

ggplot(T05_NPD_CAT, aes(x = CAT, y = TA050938, group = CAT, fill = as.factor(CAT))) +
  geom_boxplot() +
  labs(title = "TIAS 2005", x = "Category", y = "NSPD Scale Score") + 
  guides(fill = guide_legend(title = "Category"))

### Mental Health: Social Anxiety ================================================================================================ 

####
# Cumulative score from answers to SA scale questions: 
# (how often nervous meeting others/feel shy/feel self-conscious/feel nervous performing)
# Possible scores: 1-7; 9 (all items are DK/NA/refused)
####

table(TIAS$TA050933)

T05_SOA_FTLW <- TIAS[, c("TA050933", "FTL_COUNT")] %>% group_by(TA050933, FTL_COUNT) %>% summarise(Count = n())

T05_SOA_FTLW <- T05_SOA_FTLW[1:24, ]

T05_SOA_CAT <- TIAS2005[, c("TA050933", "CAT")] %>% group_by(TA050933, CAT) %>% summarise(Count = n())

ggplot(T05_SOA_FTLW, aes(x = FTL_COUNT, y = TA050933, group = FTL_COUNT, fill = as.factor(FTL_COUNT))) +
  geom_boxplot() +
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "SA Scale Score") + 
  scale_y_continuous(limits = c(1, 7), breaks = seq(1, 7, by = 1)) + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) +
  guides(fill = guide_legend(title = "# of FTL Waves"))

ggplot(T05_SOA_CAT, aes(x = CAT, y = TA050933, group = CAT, fill = as.factor(CAT))) +
  geom_boxplot() +
  labs(title = "TIAS 2005", x = "Category", y = "SA Scale Score") + 
  scale_y_continuous(limits = c(1, 7), breaks = seq(1, 7, by = 1)) +
  guides(fill = guide_legend(title = "Category"))

### Mental Health: Worry =========================================================================================================

####
# Cumulative score from answers to 'Mental Health: Worry' section questions:
# (how often worry about money/future job/feel discouraged about future)
# Possible scores: 1-7; 9 (all items are DK/NA/refused)
####

table(TIAS$TA050932)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050932 = 9)) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050932 = 9)) 

T05_WRY_FTLW <- TIAS[, c("TA050932", "FTL_COUNT")] %>% group_by(TA050932, FTL_COUNT) %>% summarise(Count = n())

T05_WRY_FTLW <- T05_WRY_FTLW[1:26, ]

T05_WRY_CAT <- TIAS2005[, c("TA050932", "CAT")] %>% group_by(TA050932, CAT) %>% summarise(Count = n())

T05_WRY_CAT <- T05_WRY_CAT[1:14, ]

ggplot(T05_WRY_FTLW, aes(x = FTL_COUNT, y = TA050932, group = FTL_COUNT, fill = as.factor(FTL_COUNT))) +
  geom_boxplot() +
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "MH-Worry Scale Score") + 
  scale_y_continuous(limits = c(1, 7), breaks = seq(1, 7, by = 1)) + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) +
  guides(fill = guide_legend(title = "# of FTL Waves"))

ggplot(T05_WRY_CAT, aes(x = CAT, y = TA050932, group = CAT, fill = as.factor(CAT))) +
  geom_boxplot() +
  labs(title = "TIAS 2005", x = "Category", y = "MH-Worry Scale Score") + 
  scale_y_continuous(limits = c(1, 7), breaks = seq(1, 7, by = 1)) +
  guides(fill = guide_legend(title = "Category"))

### Paid Employment Since Jan 1 of Prior Year ====================================================================================

#### 
# E3A. WTR Worked Since Jan 1 Of Prior Year: “Have you done any work for money since January 1, 2003? Please include any type of work, no matter how small”
# Answers: 1 (Yes); 5 (No); 8 (DK); 9 (NA/refused); 0 (Inap: Working Now)
####

table(TIAS$TA050131)

T05_EPJ_FTLW <- TIAS[, c("TA050131", "FTL_COUNT")] %>% group_by(TA050131, FTL_COUNT) %>% summarise(Count = n())

T05_EPJ_FTLW <- T05_EPJ_FTLW[1:13, ]

T05_EPJ_CAT <- TIAS2005[, c("TA050131", "CAT")] %>% group_by(TA050131, CAT) %>% summarise(Count = n())

T05_EPJ_FTLCAT <- TIAS2005_FTL[, c("TA050131", "CAT")] %>% group_by(TA050131, CAT) %>% summarise(Count = n())

T05_EPJ_IACCAT <- TIAS2005_IAC[, c("TA050131", "CAT")] %>% group_by(TA050131, CAT) %>% summarise(Count = n())

head(T05_EPJ_CAT, 6)

ggplot(T05_EPJ_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050131)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") +
  scale_fill_manual("Paid Employment Since Prior Year", values = c("lightblue1", "lightgoldenrod1", "lightpink"), labels = c("Yes (Working Now)", "Yes (Not Now)", "No"))

head(T05_EPJ_FTLW, 13)

ggplot(T05_EPJ_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050131)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Paid Employment Since Prior Year", values = c("lightblue1", "lightgoldenrod1", "lightpink"), labels = c("Yes (Working Now)", "Yes (Not Now)", "No"))

prop.table(table(TIAS2005_FTL$TA050131))

epj.pie.ftl.05 <- ggplot(data = T05_EPJ_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050131))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Paid Employment Since Prior Year", values = c("lightblue1", "lightgoldenrod1", "lightpink"), labels = c("Yes (Working Now)", "Yes (Not Now)", "No")) +
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050131))

epj.pie.iac.05 <- ggplot(data = T05_EPJ_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050131))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Paid Employment Since Prior Year", values = c("lightblue1", "lightgoldenrod1", "lightpink"), labels = c("Yes (Working Now)", "Yes (Not Now)", "No")) +
  theme_void() 

ggarrange(epj.pie.ftl.05, epj.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### Paid Employment History ====================================================================================================== 

####
# E62. WTR Ever Worked: “Have you ever done any work for money?”
# Answers: 1 (Yes); 5 (No); 8 (DK); 9 (NA/refused); 0 (Inap: Has worked since 01/01/2003)
####

table(TIAS$TA050368)

T05_EPH_FTLW <- TIAS[, c("TA050368", "FTL_COUNT")] %>% group_by(TA050368, FTL_COUNT) %>% summarise(Count = n())

T05_EPH_FTLW <- T05_EPH_FTLW[1:13, ]

T05_EPH_CAT <- TIAS2005[, c("TA050368", "CAT")] %>% group_by(TA050368, CAT) %>% summarise(Count = n())

T05_EPH_FTLCAT <- TIAS2005_FTL[, c("TA050368", "CAT")] %>% group_by(TA050368, CAT) %>% summarise(Count = n())

T05_EPH_IACCAT <- TIAS2005_IAC[, c("TA050368", "CAT")] %>% group_by(TA050368, CAT) %>% summarise(Count = n())

head(T05_EPH_CAT, 6)

ggplot(T05_EPH_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050368)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") +
  scale_fill_manual("Paid Employment History", values = c("darkturquoise", "darkviolet", "deeppink"), labels = c("Yes (Employed Since 2003 or Before)", "Yes (Not Now)", "No"))

head(T05_EPH_FTLW, 13)

ggplot(T05_EPH_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050368)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Paid Employment History", values = c("darkturquoise", "darkviolet", "deeppink"), labels = c("Yes (Employed Since 2003 or Before)", "Yes (Not Now)", "No"))

prop.table(table(TIAS2005_FTL$TA050368))

eph.pie.ftl.05 <- ggplot(data = T05_EPH_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050368))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Paid Employment History", values = c("darkturquoise", "darkviolet", "deeppink"), labels = c("Yes (Employed Since 2003 or Before)", "Yes (Not Now)", "No")) +
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050368))

eph.pie.iac.05 <- ggplot(data = T05_EPH_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050368))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Paid Employment History", values = c("darkturquoise", "darkviolet", "deeppink"), labels = c("Yes (Employed Since 2003 or Before)", "Yes (Not Now)", "No")) +
  theme_void() 

ggarrange(eph.pie.ftl.05, eph.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### Current Marital Status =======================================================================================================

####
# D1 Current Marital Status: Are you married, have you never been married, or are you widowed, divorced, or separated?
# Answers: 1 (Married); 2 (Never Married); 3 (Widowed); 4 (Divorced); 5 (Separated); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050069)

T05_CMS_FTLW <- TIAS[, c("TA050069", "FTL_COUNT")] %>% group_by(TA050069, FTL_COUNT) %>% summarise(Count = n())

T05_CMS_FTLW <- T05_CMS_FTLW[1:10, ]

T05_CMS_CAT <- TIAS2005[, c("TA050069", "CAT")] %>% group_by(TA050069, CAT) %>% summarise(Count = n())

T05_CMS_FTLCAT <- TIAS2005_FTL[, c("TA050069", "CAT")] %>% group_by(TA050069, CAT) %>% summarise(Count = n())

T05_CMS_IACCAT <- TIAS2005_IAC[, c("TA050069", "CAT")] %>% group_by(TA050069, CAT) %>% summarise(Count = n())

head(T05_CMS_CAT, 5)

ggplot(T05_CMS_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050069)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") +
  scale_fill_manual("Current Marital Status", values = c("lightcoral", "khaki1", "mediumaquamarine", "lightskyblue"), labels = c("Married", "Never Married", "Divorced", "Separated"))

head(T05_CMS_FTLW, 10)

ggplot(T05_CMS_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050069)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Current Marital Status", values = c("lightcoral", "khaki1", "mediumaquamarine", "lightskyblue"), labels = c("Married", "Never Married", "Divorced", "Separated"))

### Romantic Relationships ======================================================================================================= 

####
# D8. WTR Romantic Relationship Now: “Are you currently involved in a romantic relationship?”
# Answers: 1 (Yes); 5 (No); 8 (DK); 9 (NA/refused); 0 (Inap: Married)
####

table(TIAS$TA050078)

T05_RRN_FTLW <- TIAS[, c("TA050078", "FTL_COUNT")] %>% group_by(TA050078, FTL_COUNT) %>% summarise(Count = n())

T05_RRN_FTLW <- T05_RRN_FTLW[1:11, ]

T05_RRN_CAT <- TIAS2005[, c("TA050078", "CAT")] %>% group_by(TA050078, CAT) %>% summarise(Count = n())

T05_RRN_FTLCAT <- TIAS2005_FTL[, c("TA050078", "CAT")] %>% group_by(TA050078, CAT) %>% summarise(Count = n())

T05_RRN_IACCAT <- TIAS2005_IAC[, c("TA050078", "CAT")] %>% group_by(TA050078, CAT) %>% summarise(Count = n())

head(T05_RRN_CAT, 6)

ggplot(T05_RRN_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050078)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") +
  scale_fill_manual("Involved in Romantic Rel.", values = c("darkturquoise", "darkviolet", "deeppink"), labels = c("Inap: Married", "Yes", "No"))

head(T05_RRN_FTLW, 11)

ggplot(T05_RRN_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050078)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Involved in Romantic Rel.", values = c("darkturquoise", "darkviolet", "deeppink"), labels = c("Inap: Married", "Yes", "No"))

prop.table(table(TIAS2005_FTL$TA050078))

rrn.pie.ftl.05 <- ggplot(data = T05_RRN_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050078))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Involved in Romantic Rel.", values = c("darkturquoise", "darkviolet", "deeppink"), labels = c("Inap: Married", "Yes", "No")) +
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050078))

rrn.pie.iac.05 <- ggplot(data = T05_RRN_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050078))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Involved in Romantic Rel.", values = c("darkturquoise", "darkviolet", "deeppink"), labels = c("Inap: Married", "Yes", "No")) +
  theme_void() 

ggarrange(rrn.pie.ftl.05, rrn.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### Number of Children ===========================================================================================================

####
# D28A. Number of Children: “How many (biological,) adopted, or step- children do you have?”
# Answers: 0-20 (# of Children); 98 (DK); 99 (NA/refused)
####

table(TIAS$TA050091)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050091 = 99)) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050091 = 99)) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050091 = 99)) 

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050091 = 99)) 

T05_CHL_FTLW <- TIAS[, c("TA050091", "FTL_COUNT")] %>% group_by(TA050091, FTL_COUNT) %>% summarise(Count = n())

T05_CHL_FTLW <- T05_CHL_FTLW[1:14, ]

T05_CHL_CAT <- TIAS2005[, c("TA050091", "CAT")] %>% group_by(TA050091, CAT) %>% summarise(Count = n())

T05_CHL_CAT <- T05_CHL_CAT[1:8, ]

ggplot(T05_CHL_FTLW, aes(x = FTL_COUNT, y = TA050091, group = FTL_COUNT, fill = as.factor(FTL_COUNT))) +
  geom_boxplot() + 
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "Number of Children") + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  guides(fill = guide_legend(title = "# of FTL Waves"))

ggplot(T05_CHL_CAT, aes(x = CAT, y = TA050091, group = CAT, fill = as.factor(CAT))) +
  geom_boxplot() +
  labs(title = "TIAS 2005", x = "Category", y = "Number of Children") + 
  guides(fill = guide_legend(title = "Category"))

### High School Education ======================================================================================================== 

####
# G1. WTR Graduated High School: “Now I would like to talk about the education you have received. Did you graduate from high school, get a GED, or neither?”
# Answers: 1 (Graduated from high school); 2 (Got a GED); 3 (Neither); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050573)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050573 = 9)) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050573 = 9)) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050573 = 9)) 

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050573 = 9)) 

T05_HSG_FTLW <- TIAS[, c("TA050573", "FTL_COUNT")] %>% group_by(TA050573, FTL_COUNT) %>% summarise(Count = n())

T05_HSG_FTLW <- T05_HSG_FTLW[1:13, ]

T05_HSG_CAT <- TIAS2005[, c("TA050573", "CAT")] %>% group_by(TA050573, CAT) %>% summarise(Count = n())

T05_HSG_CAT <- T05_HSG_CAT[1:6, ]

T05_HSG_FTLCAT <- TIAS2005_FTL[, c("TA050573", "CAT")] %>% group_by(TA050573, CAT) %>% summarise(Count = n())

T05_HSG_IACCAT <- TIAS2005_IAC[, c("TA050573", "CAT")] %>% group_by(TA050573, CAT) %>% summarise(Count = n())

T05_HSG_IACCAT <- T05_HSG_IACCAT[1:3, ]

head(T05_HSG_CAT, 6)

ggplot(T05_HSG_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050573)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") +
  scale_fill_manual("High School Education", values = c("lightblue1", "lightgoldenrod1", "lightpink"), labels = c("HS Diploma", "GED", "Neither"))

head(T05_HSG_FTLW, 13)

ggplot(T05_HSG_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050573)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("High School Education", values = c("lightblue1", "lightgoldenrod1", "lightpink"), labels = c("HS Diploma", "GED", "Neither"))

prop.table(table(TIAS2005_FTL$TA050573))

hsg.pie.ftl.05 <- ggplot(data = T05_HSG_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050573))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("High School Education", values = c("lightblue1", "lightgoldenrod1", "lightpink"), labels = c("HS Diploma", "GED", "Neither")) +
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050573))

hsg.pie.iac.05 <- ggplot(data = T05_HSG_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050573))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("High School Education", values = c("lightblue1", "lightgoldenrod1", "lightpink"), labels = c("HS Diploma", "GED", "Neither")) +
  theme_void() 

ggarrange(hsg.pie.ftl.05, hsg.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### College Education ============================================================================================================ 

####
# G10. WTR Ever Attended College: “Have you ever attended college?” 
# Answers: 1 (Yes); 5 (No); 8 (DK); 9 (NA/refused); 0 (Inap: did not complete high school or receive a GED)
####

table(TIAS$TA050594)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050594 = 9)) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050594 = 9)) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050594 = 9)) 

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050594 = 9)) 

T05_CLG_FTLW <- TIAS[, c("TA050594", "FTL_COUNT")] %>% group_by(TA050594, FTL_COUNT) %>% summarise(Count = n())

T05_CLG_FTLW <- T05_CLG_FTLW[1:14, ]

T05_CLG_CAT <- TIAS2005[, c("TA050594", "CAT")] %>% group_by(TA050594, CAT) %>% summarise(Count = n())

T05_CLG_CAT <- T05_CLG_CAT[1:6, ]

T05_CLG_FTLCAT <- TIAS2005_FTL[, c("TA050594", "CAT")] %>% group_by(TA050594, CAT) %>% summarise(Count = n())

T05_CLG_IACCAT <- TIAS2005_IAC[, c("TA050594", "CAT")] %>% group_by(TA050594, CAT) %>% summarise(Count = n())

T05_CLG_IACCAT <- T05_CLG_IACCAT[1:3, ]

head(T05_CLG_CAT, 6)

ggplot(T05_CLG_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050594)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") +
  scale_fill_manual("Higher Education", values = c("lightblue1", "lightgoldenrod1", "lightpink"), labels = c("Attended College", "Graduated HS", "No HS Diploma or GED"))

head(T05_CLG_FTLW, 13)

ggplot(T05_CLG_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050594)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Higher Education", values = c("lightblue1", "lightgoldenrod1", "lightpink"), labels = c("Attended College", "Graduated HS", "No HS Diploma or GED"))

prop.table(table(TIAS2005_FTL$TA050594))

clg.pie.ftl.05 <- ggplot(data = T05_CLG_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050594))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Higher Education", values = c("lightblue1", "lightgoldenrod1", "lightpink"), labels = c("Attended College", "Graduated HS", "No HS Diploma or GED")) +
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050594))

clg.pie.iac.05 <- ggplot(data = T05_CLG_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050594))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Higher Education", values = c("lightblue1", "lightgoldenrod1", "lightpink"), labels = c("Attended College", "Graduated HS", "No HS Diploma or GED")) +
  theme_void() 

ggarrange(clg.pie.ftl.05, clg.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### Highest Education Level ====================================================================================================== 

####
# Enrollment Status 
# Answers: 1 (No high school diploma and no GED); 2 (no high school diploma but has GED); 3 (Has high school diploma); 4 (Not enrolled, some college);
# 5 (Not enrolled, 2-yr college graduate); 6 (Not enrolled, 4-yr college graduate); 7 (Not enrolled, graduate degree); 9 (Enrolled, has no prior degree);
# 10 (Enrolled, has a prior degree); 11 (Enrolled, graduate program); 99 (NA/DK)
####

table(TIAS$TA050946)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050946 = 99)) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050946 = 99)) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050946 = 99)) 

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050946 = 99))  

T05_HEL_FTLW <- TIAS[, c("TA050946", "FTL_COUNT")] %>% group_by(TA050946, FTL_COUNT) %>% summarise(Count = n())

T05_HEL_FTLW <- T05_HEL_FTLW[1:24, ]

T05_HEL_CAT <- TIAS2005[, c("TA050946", "CAT")] %>% group_by(TA050946, CAT) %>% summarise(Count = n())

T05_HEL_CAT <- T05_HEL_CAT[1:12, ]

T05_HEL_FTLCAT <- TIAS2005_FTL[, c("TA050946", "CAT")] %>% group_by(TA050946, CAT) %>% summarise(Count = n())

T05_HEL_FTLCAT <- T05_HEL_FTLCAT[1:4, ]

T05_HEL_IACCAT <- TIAS2005_IAC[, c("TA050946", "CAT")] %>% group_by(TA050946, CAT) %>% summarise(Count = n())

T05_HEL_IACCAT <- T05_HEL_IACCAT[1:8, ]

head(T05_HEL_CAT, 12)

ggplot(T05_HEL_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050946))) + 
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Highest Education Level", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumspringgreen", "mediumslateblue"), 
  labels = c("No HS Diploma, No GED", "No HS Diploma, GED", "HS Diploma", "Not Enrolled, Some College", "Not Enrolled, 2-yr College Graduate", "Enrolled in College, No Prior Degree", 
             "Enrolled in College, Has Prior Degree", "Enrolled in Graduate Program"))

head(T05_HEL_FTLW, 24)

ggplot(T05_HEL_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050946)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Highest Education Level", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumspringgreen", "mediumslateblue"), 
  labels = c("No HS Diploma, No GED", "No HS Diploma, GED", "HS Diploma", "Not Enrolled, Some College", "Not Enrolled, 2-yr College Graduate", "Enrolled in College, No Prior Degree", 
            "Enrolled in College, Has Prior Degree", "Enrolled in Graduate Program"))

prop.table(table(TIAS2005_FTL$TA050946))

hel.pie.ftl.05 <- ggplot(data = T05_HEL_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050946))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("Highest Education Level", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumspringgreen", "mediumslateblue"), 
  labels = c("No HS Diploma, No GED", "No HS Diploma, GED", "HS Diploma", "Not Enrolled, Some College", "Not Enrolled, 2-yr College Graduate", "Enrolled in College, No Prior Degree", 
            "Enrolled in College, Has Prior Degree", "Enrolled in Graduate Program"))

prop.table(table(TIAS2005_IAC$TA050946))

hel.pie.iac.05 <- ggplot(data = T05_HEL_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050946))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("Highest Education Level", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumspringgreen", "mediumslateblue"), 
  labels = c("No HS Diploma, No GED", "No HS Diploma, GED", "HS Diploma", "Not Enrolled, Some College", "Not Enrolled, 2-yr College Graduate", "Enrolled in College, No Prior Degree", 
            "Enrolled in College, Has Prior Degree", "Enrolled in Graduate Program"))

ggarrange(hel.pie.ftl.05, hel.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### Responsibility - Earning Own Living ==========================================================================================

####
# B5A. How Much Responsibility Earning Own Living: “As people get older they begin to take more responsibility for themselves. How much responsibility do you currently take for earning your own living?”
# Answers: 1 (Somebody else does this for me all of the time); 2 (Somebody else does this most of the time); 3 (I do this half of the time); 4 (I do this most of the time); 
# 5 (I am completely responsible for this all the time); 8 (DK); 9 (NA; refused)
####

table(TIAS$TA050044)

T05_REO_FTLW <- TIAS[, c("TA050044", "FTL_COUNT")] %>% group_by(TA050044, FTL_COUNT) %>% summarise(Count = n())

T05_REO_FTLW <- T05_REO_FTLW[1:19, ]

T05_REO_CAT <- TIAS2005[, c("TA050044", "CAT")] %>% group_by(TA050044, CAT) %>% summarise(Count = n())

T05_REO_FTLCAT <- TIAS2005_FTL[, c("TA050044", "CAT")] %>% group_by(TA050044, CAT) %>% summarise(Count = n())

T05_REO_IACCAT <- TIAS2005_IAC[, c("TA050044", "CAT")] %>% group_by(TA050044, CAT) %>% summarise(Count = n())

head(T05_REO_CAT, 10)

ggplot(T05_REO_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050044)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Responsibility - Earning Own Living", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue"), 
                    labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time"))

head(T05_REO_FTLW, 19)

ggplot(T05_REO_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050044)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Responsibility - Earning Own Living", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue"), 
                    labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time"))

prop.table(table(TIAS2005_FTL$TA050044))

reo.pie.ftl.05 <- ggplot(data = T05_REO_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050044))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Responsibility - Earning Own Living", values = c("lightcoral", "mediumturquoise", "deepskyblue", "gold", "green3"),
  labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time")) +
  theme_void()

prop.table(table(TIAS2005_IAC$TA050044))

reo.pie.iac.05 <- ggplot(data = T05_REO_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050044))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Responsibility - Earning Own Living", values = c("lightcoral", "mediumturquoise", "deepskyblue", "gold", "green3"),
  labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time")) + 
  theme_void()

ggarrange(reo.pie.ftl.05, reo.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### Responsibility - Paying Own Rent =============================================================================================

####
# B5B. How Much Responsibility Paying Own Rent: “How much responsibility do you currently take for paying your rent or mortgage?"  
# Answers: 1 (Somebody else does this for me all of the time); 2 (Somebody else does this most of the time); 3 (I do this half of the time); 
# 4 (I do this most of the time); 5 (I am completely responsible for this all the time); 6 (No rent or mortgage to pay); 8 (DK); 9 (NA; refused)
####

table(TIAS$TA050045)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050045 = 8)) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050045 = 8)) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050045 = 8)) 

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050045 = 8)) 

T05_POR_FTLW <- TIAS[, c("TA050045", "FTL_COUNT")] %>% group_by(TA050045, FTL_COUNT) %>% summarise(Count = n())

T05_POR_FTLW <- T05_POR_FTLW[1:23, ]

T05_POR_CAT <- TIAS2005[, c("TA050045", "CAT")] %>% group_by(TA050045, CAT) %>% summarise(Count = n())

T05_POR_CAT <- T05_POR_CAT[1:12, ]

T05_POR_FTLCAT <- TIAS2005_FTL[, c("TA050045", "CAT")] %>% group_by(TA050045, CAT) %>% summarise(Count = n())

T05_POR_IACCAT <- TIAS2005_IAC[, c("TA050045", "CAT")] %>% group_by(TA050045, CAT) %>% summarise(Count = n())

T05_POR_IACCAT <- T05_POR_IACCAT[1:6, ]

head(T05_POR_CAT, 12)

ggplot(T05_POR_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050045)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Responsibility - Paying Own Rent", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "blueviolet"), 
                    labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time", "No payments"))

head(T05_POR_FTLW, 23)

ggplot(T05_POR_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050045)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Responsibility - Paying Own Rent", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "blueviolet"), 
                    labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time", "No payments"))

prop.table(table(TIAS2005_FTL$TA050045))

por.pie.ftl.05 <- ggplot(data = T05_POR_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050045))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Responsibility - Paying Own Rent", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "blueviolet"), 
                    labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time", "No payments"))  + theme_void()

prop.table(table(TIAS2005_IAC$TA050045))

por.pie.iac.05 <- ggplot(data = T05_POR_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050045))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Responsibility - Paying Own Rent", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "blueviolet"), 
                    labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time", "No payments"))  + theme_void()

ggarrange(por.pie.ftl.05, por.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### Responsibility - Paying Own Bills ============================================================================================

####
# B5C. How Much Responsibility for Own Bills: How much responsibility do you currently take for paying your bills?  
# Answers: 1 (Somebody else does this for me all of the time); 2 (Somebody else does this most of the time); 3 (I do this half of the time); 4 (I do this most of the time); 
# 5 (I am completely responsible for this all the time); 6 (No bills; 8 (DK); 9 (NA; refused)
####

table(TIAS$TA050046)

T05_POB_FTLW <- TIAS[, c("TA050046", "FTL_COUNT")] %>% group_by(TA050046, FTL_COUNT) %>% summarise(Count = n())

T05_POB_FTLW <- T05_POB_FTLW[1:24, ]

T05_POB_CAT <- TIAS2005[, c("TA050046", "CAT")] %>% group_by(TA050046, CAT) %>% summarise(Count = n())

T05_POB_FTLCAT <- TIAS2005_FTL[, c("TA050046", "CAT")] %>% group_by(TA050046, CAT) %>% summarise(Count = n())

T05_POB_IACCAT <- TIAS2005_IAC[, c("TA050046", "CAT")] %>% group_by(TA050046, CAT) %>% summarise(Count = n())

head(T05_POB_CAT, 12)

ggplot(T05_POB_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050046)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Responsibility - Paying Own Bills", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "blueviolet"), 
                    labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time", "No bills"))

head(T05_POB_FTLW, 24)

ggplot(T05_POB_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050046)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Responsibility - Paying Own Bills", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "blueviolet"), 
                    labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time", "No bills"))

prop.table(table(TIAS2005_FTL$TA050046))

pob.pie.ftl.05 <- ggplot(data = T05_POB_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050046))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("Responsibility - Paying Own Bills", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "blueviolet"), 
                    labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time", "No bills"))  

prop.table(table(TIAS2005_IAC$TA050046))

pob.pie.iac.05 <- ggplot(data = T05_POB_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050046))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("Responsibility - Paying Own Bills", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "blueviolet"), 
                    labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time", "No bills"))  

ggarrange(pob.pie.ftl.05, pob.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### Responsibility - Managing Money ============================================================================================== 

####
# B5D. How Much Responsibility Managing Money: "How much responsibility do you currently take for managing your money?"
# Answers: 1 (Somebody else does this for me all of the time); 2 (Somebody else does this most of the time); 3 (I do this half of the time); 4 (I do this most of the time); 
# 5 (I am completely responsible for this all the time); 8 (DK); 9 (NA; refused)
####

table(TIAS$TA050047)

T05_RMM_FTLW <- TIAS[, c("TA050047", "FTL_COUNT")] %>% group_by(TA050047, FTL_COUNT) %>% summarise(Count = n())

T05_RMM_FTLW <- T05_RMM_FTLW[1:20, ]

T05_RMM_CAT <- TIAS2005[, c("TA050047", "CAT")] %>% group_by(TA050047, CAT) %>% summarise(Count = n())

T05_RMM_FTLCAT <- TIAS2005_FTL[, c("TA050047", "CAT")] %>% group_by(TA050047, CAT) %>% summarise(Count = n())

T05_RMM_IACCAT <- TIAS2005_IAC[, c("TA050047", "CAT")] %>% group_by(TA050047, CAT) %>% summarise(Count = n())

head(T05_RMM_CAT, 10)

ggplot(T05_RMM_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050047)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Responsibility - Managing Money", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue"), 
                    labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time"))

head(T05_RMM_FTLW, 20)

ggplot(T05_RMM_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050047)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Responsibility - Managing Money", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue"), 
                    labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time"))

prop.table(table(TIAS2005_FTL$TA050047))

rmm.pie.ftl.05 <- ggplot(data = T05_RMM_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050047))) +  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Responsibility - Managing Money", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue"),
  labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time")) +
  theme_void()

prop.table(table(TIAS2005_IAC$TA050047))

rmm.pie.iac.05 <- ggplot(data = T05_RMM_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050047))) + geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Responsibility - Managing Money", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue"),
  labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time")) + 
  theme_void()

ggarrange(rmm.pie.ftl.05, rmm.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### How Good at Taking Responsibility for Actions ================================================================================

####
# B6A. On a scale of 1 to 7, where 1 means "Not At All Well" and 7 means "Extremely Well", how good are you at taking responsibility for your actions?
# Answers: 1-7 (Values range from 1 to 7; 1 represents "not at all well" and 7 represents "extremely well"); 8 (DK); 9 (NA; refused)
####

table(TIAS$TA050048)

T05_ROA_FTLW <- TIAS[, c("TA050048", "FTL_COUNT")] %>% group_by(TA050048, FTL_COUNT) %>% summarise(Count = n())

T05_ROA_FTLW <- T05_ROA_FTLW[1:21, ]

T05_ROA_CAT <- TIAS2005[, c("TA050048", "CAT")] %>% group_by(TA050048, CAT) %>% summarise(Count = n())

ggplot(T05_ROA_FTLW, aes(x = FTL_COUNT, y = TA050048, group = FTL_COUNT, fill = as.factor(FTL_COUNT))) +
  geom_boxplot() +
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "Taking Responsibility for Actions - Self-Rating (1 = Not at all well; 7 = Extremely Well)") + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) +
  scale_y_continuous(breaks = seq(1, 7, by = 1)) +
  guides(fill = guide_legend(title = "# of FTL Waves"))

ggplot(T05_ROA_CAT, aes(x = CAT, y = TA050048, group = CAT, fill = as.factor(CAT))) +
  geom_boxplot() +
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "Taking Responsibility for Actions - Self-Rating (1 = Not at all well; 7 = Extremely Well)") + 
  scale_y_continuous(breaks = seq(1, 7, by = 1)) +
  guides(fill = guide_legend(title = "Category"))

### How Good at Money Management =================================================================================================

####
# B6C. On a scale of 1 to 7, where 1 means "Not At All Well" and 7 means "Extremely Well", how good are you at managing money?
# Answers: 1-7 (Values range from 1 to 7; 1 represents "not at all well" and 7 represents "extremely well"); 8 (DK); 9 (NA; refused)
####

table(TIAS$TA050050)

T05_GMM_FTLW <- TIAS[, c("TA050050", "FTL_COUNT")] %>% group_by(TA050050, FTL_COUNT) %>% summarise(Count = n())

T05_GMM_FTLW <- T05_GMM_FTLW[1:22, ]

T05_GMM_CAT <- TIAS2005[, c("TA050050", "CAT")] %>% group_by(TA050050, CAT) %>% summarise(Count = n())

ggplot(T05_GMM_FTLW, aes(x = FTL_COUNT, y = TA050050, group = FTL_COUNT, fill = as.factor(FTL_COUNT))) +
  geom_boxplot() +
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "How Good at Money Management - Self-Rating (1 = Not at all well; 7 = Extremely Well)") + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) +
  scale_y_continuous(breaks = seq(1, 7, by = 1)) +
  guides(fill = guide_legend(title = "# of FTL Waves"))

ggplot(T05_GMM_CAT, aes(x = CAT, y = TA050050, group = CAT, fill = as.factor(CAT))) +
  geom_boxplot() +
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "How Good at Money Management - Self-Rating (1 = Not at all well; 7 = Extremely Well)") + 
  scale_y_continuous(breaks = seq(1, 7, by = 1)) +
  guides(fill = guide_legend(title = "Category"))

### How Good at Paying Off Credit Card Balances ==================================================================================

####
# B6D. On a scale of 1 to 7, where 1 means "Not At All Well" and 7 means "Extremely Well", how good are you at paying off credit card balances each month?
# Answers: 0 (does not have a credit card); 1-7 (Values range from 1 to 7; 1 represents "not at all well" and 7 represents "extremely well"); 8 (DK); 9 (NA; refused)
####

table(TIAS$TA050051)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050051 = 8)) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050051 = 8)) 

T05_CCB_FTLW <- TIAS[, c("TA050051", "FTL_COUNT")] %>% group_by(TA050051, FTL_COUNT) %>% summarise(Count = n())

T05_CCB_FTLW <- T05_CCB_FTLW[1:26, ]

T05_CCB_CAT <- TIAS2005[, c("TA050051", "CAT")] %>% group_by(TA050051, CAT) %>% summarise(Count = n())

T05_CCB_CAT <- T05_CCB_CAT[1:16, ]

ggplot(T05_CCB_FTLW, aes(x = FTL_COUNT, y = TA050051, group = FTL_COUNT, fill = as.factor(FTL_COUNT))) +
  geom_boxplot() +
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "Paying off Credit Card Balances - Self-Rating (1 = Not at all well; 7 = Extremely Well)") + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) +
  scale_y_continuous(breaks = seq(1, 7, by = 1)) +
  guides(fill = guide_legend(title = "# of FTL Waves"))

ggplot(T05_CCB_CAT, aes(x = CAT, y = TA050051, group = CAT, fill = as.factor(CAT))) +
  geom_boxplot() +
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "How Good at Money Management - Self-Rating (1 = Not at all well; 7 = Extremely Well)") + 
  scale_y_continuous(breaks = seq(1, 7, by = 1)) +
  guides(fill = guide_legend(title = "Category"))

### MIDUS M1 - Happiness =========================================================================================================

####
# M1. Frequency of Happiness in the Last Month: "These last questions are about how you have been feeling in the last month. In the last month, how often did you feel happy?"
# Answers: 1 (Never); 2 (Once or twice); 3 (About once a week); 4 (Two or three times a week); 5 (Almost every day); 6 (Every day); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050888)

T05_M01_FTLW <- TIAS[, c("TA050888", "FTL_COUNT")] %>% group_by(TA050888, FTL_COUNT) %>% summarise(Count = n())

T05_M01_FTLW <- T05_M01_FTLW[1:20, ]

T05_M01_CAT <- TIAS2005[, c("TA050888", "CAT")] %>% group_by(TA050888, CAT) %>% summarise(Count = n())

T05_M01_FTLCAT <- TIAS2005_FTL[, c("TA050888", "CAT")] %>% group_by(TA050888, CAT) %>% summarise(Count = n())

T05_M01_IACCAT <- TIAS2005_IAC[, c("TA050888", "CAT")] %>% group_by(TA050888, CAT) %>% summarise(Count = n())

head(T05_M01_CAT, 10)

ggplot(T05_M01_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050888)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Happiness in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day"))

head(T05_M01_FTLW, 20)

ggplot(T05_M01_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050888)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Happiness in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day"))

prop.table(table(TIAS2005_FTL$TA050888))

m01.pie.ftl.05 <- ggplot(data = T05_M01_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050888))) +  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Happiness in Last Mo.", values = c("green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("About once a week", "Two or three times a week", "Almost every day", "Every day")) +
  theme_void()

prop.table(table(TIAS2005_IAC$TA050888))

m01.pie.iac.05 <- ggplot(data = T05_M01_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050888))) + geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Happiness in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day")) +
  theme_void()

ggarrange(m01.pie.ftl.05, m01.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### MIDUS M2 - Interest in Life ==================================================================================================

####
# M2. Freq of Interest in Life in the Last Month: "In the last month, how often did you feel interested in life?"
# Answers: 1 (Never); 2 (Once or twice); 3 (About once a week); 4 (Two or three times a week); 5 (Almost every day); 6 (Every day); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050889)

T05_M02_FTLW <- TIAS[, c("TA050889", "FTL_COUNT")] %>% group_by(TA050889, FTL_COUNT) %>% summarise(Count = n())

T05_M02_FTLW <- T05_M02_FTLW[1:21, ]

T05_M02_CAT <- TIAS2005[, c("TA050889", "CAT")] %>% group_by(TA050889, CAT) %>% summarise(Count = n())

T05_M02_FTLCAT <- TIAS2005_FTL[, c("TA050889", "CAT")] %>% group_by(TA050889, CAT) %>% summarise(Count = n())

T05_M02_IACCAT <- TIAS2005_IAC[, c("TA050889", "CAT")] %>% group_by(TA050889, CAT) %>% summarise(Count = n())

head(T05_M02_CAT, 12)

ggplot(T05_M02_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050889)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Interest in Life in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day"))

head(T05_M02_FTLW, 21)

ggplot(T05_M02_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050889)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Interest in Life in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day"))

prop.table(table(TIAS2005_FTL$TA050889))

m02.pie.ftl.05 <- ggplot(data = T05_M02_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050889))) +  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Interest in Life in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day")) +
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050889))

m02.pie.iac.05 <- ggplot(data = T05_M02_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050889))) + geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Interest in Life in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day")) +
  theme_void()

ggarrange(m02.pie.ftl.05, m02.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### MIDUS M3 - Satisfaction ======================================================================================================

####
# M3. Freq of Feeling Satisfied in Last Month: "In the last month, how often did you feel satisfied?"
# Answers: 1 (Never); 2 (Once or twice); 3 (About once a week); 4 (Two or three times a week); 5 (Almost every day); 6 (Every day); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050890)

T05_M03_FTLW <- TIAS[, c("TA050890", "FTL_COUNT")] %>% group_by(TA050890, FTL_COUNT) %>% summarise(Count = n())

T05_M03_FTLW <- T05_M03_FTLW[1:22, ]

T05_M03_CAT <- TIAS2005[, c("TA050890", "CAT")] %>% group_by(TA050890, CAT) %>% summarise(Count = n())

T05_M03_FTLCAT <- TIAS2005_FTL[, c("TA050890", "CAT")] %>% group_by(TA050890, CAT) %>% summarise(Count = n())

T05_M03_IACCAT <- TIAS2005_IAC[, c("TA050890", "CAT")] %>% group_by(TA050890, CAT) %>% summarise(Count = n())

head(T05_M03_CAT, 12)

ggplot(T05_M03_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050890)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Satisfaction in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day"))

head(T05_M03_FTLW, 22)

ggplot(T05_M03_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050890)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Satisfaction in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day"))

prop.table(table(TIAS2005_FTL$TA050890))

m03.pie.ftl.05 <- ggplot(data = T05_M03_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050890))) +  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Satisfaction in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day")) +
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050890))

m03.pie.iac.05 <- ggplot(data = T05_M03_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050890))) + geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Interest in Life in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day")) +
  theme_void()

ggarrange(m03.pie.ftl.05, m03.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### MIDUS M4 - Contribution of Something Important to Society ====================================================================

####
# M4. Freq of Feeling Contributed to Society: “In the last month, how often did you feel that you had something important to contribute to society?"
# Answers: 1 (Never); 2 (Once or twice); 3 (About once a week); 4 (Two or three times a week); 5 (Almost every day); 6 (Every day); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050891)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050891 = 8)) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050891 = 8)) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050891 = 8)) 

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050891 = 8)) 

T05_M04_FTLW <- TIAS[, c("TA050891", "FTL_COUNT")] %>% group_by(TA050891, FTL_COUNT) %>% summarise(Count = n())

T05_M04_FTLW <- T05_M04_FTLW[1:23, ]

T05_M04_CAT <- TIAS2005[, c("TA050891", "CAT")] %>% group_by(TA050891, CAT) %>% summarise(Count = n())

T05_M04_CAT <- T05_M04_CAT[1:12, ]

T05_M04_FTLCAT <- TIAS2005_FTL[, c("TA050891", "CAT")] %>% group_by(TA050891, CAT) %>% summarise(Count = n())

T05_M04_FTLCAT <- T05_M04_FTLCAT[1:6, ]

T05_M04_IACCAT <- TIAS2005_IAC[, c("TA050891", "CAT")] %>% group_by(TA050891, CAT) %>% summarise(Count = n())

head(T05_M04_CAT, 12)

ggplot(T05_M04_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050891)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Feeling Contributed to Society in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
                    labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day"))

head(T05_M04_FTLW, 23)

ggplot(T05_M04_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050891)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Feeling Contributed to Society in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
                    labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day"))

prop.table(table(TIAS2005_FTL$TA050891))

m04.pie.ftl.05 <- ggplot(data = T05_M04_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050891))) + geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Feeling Contributed to Society in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day")) +
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050891))

m04.pie.iac.05 <- ggplot(data = T05_M04_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050891))) + geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Feeling Contributed to Society in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day")) +
  theme_void()

ggarrange(m04.pie.ftl.05, m04.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### MIDUS M5 - Belonging to Community ============================================================================================

####
# M5. Freq of Feeling Belonging to Community: "In the last month, how often did you feel that you belonged to a community like a social group, your school, or your neighborhood?”
# Answers: 1 (Never); 2 (Once or twice); 3 (About once a week); 4 (Two or three times a week); 5 (Almost every day); 6 (Every day); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050892)

T05_M05_FTLW <- TIAS[, c("TA050892", "FTL_COUNT")] %>% group_by(TA050892, FTL_COUNT) %>% summarise(Count = n())

T05_M05_FTLW <- T05_M05_FTLW[1:21, ]

T05_M05_CAT <- TIAS2005[, c("TA050892", "CAT")] %>% group_by(TA050892, CAT) %>% summarise(Count = n())

T05_M05_FTLCAT <- TIAS2005_FTL[, c("TA050892", "CAT")] %>% group_by(TA050892, CAT) %>% summarise(Count = n())

T05_M05_IACCAT <- TIAS2005_IAC[, c("TA050892", "CAT")] %>% group_by(TA050892, CAT) %>% summarise(Count = n())

head(T05_M05_CAT, 12)

ggplot(T05_M05_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050892)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Feeling Belonging to Society in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
                    labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day"))

head(T05_M05_FTLW, 21)

ggplot(T05_M05_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050892)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Feeling Belonging to Society in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
                    labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day"))

prop.table(table(TIAS2005_FTL$TA050892))

m05.pie.ftl.05 <- ggplot(data = T05_M05_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050892))) + geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Feeling Belonging to Society in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day")) +
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050892))

m05.pie.iac.05 <- ggplot(data = T05_M05_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050892))) + geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Feeling Belonging to Society in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day")) +
  theme_void()

ggarrange(m05.pie.ftl.05, m05.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### MIDUS M6 - Society Getting Better ============================================================================================

####
# M6. Freq of Feeling Society Getting Better: "In the last month, how often did you feel that our society is becoming a better place?”
# Answers: 1 (Never); 2 (Once or twice); 3 (About once a week); 4 (Two or three times a week); 5 (Almost every day); 6 (Every day); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050893)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050893 = 8)) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050893 = 8)) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050893 = 8)) 

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050893 = 8)) 

T05_M06_FTLW <- TIAS[, c("TA050893", "FTL_COUNT")] %>% group_by(TA050893, FTL_COUNT) %>% summarise(Count = n())

T05_M06_FTLW <- T05_M06_FTLW[1:24, ]

T05_M06_CAT <- TIAS2005[, c("TA050893", "CAT")] %>% group_by(TA050893, CAT) %>% summarise(Count = n())

T05_M06_CAT <- T05_M06_CAT[1:12, ]

T05_M06_FTLCAT <- TIAS2005_FTL[, c("TA050893", "CAT")] %>% group_by(TA050893, CAT) %>% summarise(Count = n())

T05_M06_FTLCAT <- T05_M06_FTLCAT[1:6, ]

T05_M06_IACCAT <- TIAS2005_IAC[, c("TA050893", "CAT")] %>% group_by(TA050893, CAT) %>% summarise(Count = n())

T05_M06_IACCAT <- T05_M06_IACCAT[1:6, ]

head(T05_M06_CAT, 12)

ggplot(T05_M06_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050893)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Feeling Society Getting Better in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day"))

head(T05_M06_FTLW, 24)

ggplot(T05_M06_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050893)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Feeling Society Getting Better in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day"))

prop.table(table(TIAS2005_FTL$TA050893))

m06.pie.ftl.05 <- ggplot(data = T05_M06_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050893))) + geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Feeling Society Getting Better in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day")) +
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050893))

m06.pie.iac.05 <- ggplot(data = T05_M06_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050893))) + geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Feeling Society Getting Better in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day")) +
  theme_void()

ggarrange(m06.pie.ftl.05, m06.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### MIDUS M7 - People Basically Good =============================================================================================

####
# M7. Freq of Feeling People Basically Good: "In the last month, how often did you feel that people are basically good?”
# Answers: 1 (Never); 2 (Once or twice); 3 (About once a week); 4 (Two or three times a week); 5 (Almost every day); 6 (Every day); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050894)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050894 = 8)) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050894 = 8)) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050894 = 8)) 

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050894 = 8)) 

T05_M07_FTLW <- TIAS[, c("TA050894", "FTL_COUNT")] %>% group_by(TA050894, FTL_COUNT) %>% summarise(Count = n())

T05_M07_FTLW <- T05_M07_FTLW[1:24, ]

T05_M07_CAT <- TIAS2005[, c("TA050894", "CAT")] %>% group_by(TA050894, CAT) %>% summarise(Count = n())

T05_M07_CAT <- T05_M07_CAT[1:12, ]

T05_M07_FTLCAT <- TIAS2005_FTL[, c("TA050894", "CAT")] %>% group_by(TA050894, CAT) %>% summarise(Count = n())

T05_M07_FTLCAT <- T05_M07_FTLCAT[1:6, ]

T05_M07_IACCAT <- TIAS2005_IAC[, c("TA050894", "CAT")] %>% group_by(TA050894, CAT) %>% summarise(Count = n())

T05_M07_IACCAT <- T05_M07_IACCAT[1:6, ]

head(T05_M07_CAT, 12)

ggplot(T05_M07_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050894)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Feeling People are Good in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day"))

head(T05_M07_FTLW, 24)

ggplot(T05_M07_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050894)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Feeling People are Good in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day"))

prop.table(table(TIAS2005_FTL$TA050894))

m07.pie.ftl.05 <- ggplot(data = T05_M07_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050894))) + geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Feeling People are Good in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day")) + 
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050894))

m07.pie.iac.05 <- ggplot(data = T05_M07_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050894))) + geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Feeling People are Good in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day")) +
  theme_void()

ggarrange(m07.pie.ftl.05, m07.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### MIDUS M8 - Way Society Makes Sense ===========================================================================================

####
# M8. Freq Feeling Way Soc Works Makes Sense: "In the last month, how often did you feel that the way our society works made sense to you?”
# Answers: 1 (Never); 2 (Once or twice); 3 (About once a week); 4 (Two or three times a week); 5 (Almost every day); 6 (Every day); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050895)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050895 = 8)) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050895 = 8)) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050895 = 8)) 

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050895 = 8)) 

T05_M08_FTLW <- TIAS[, c("TA050895", "FTL_COUNT")] %>% group_by(TA050895, FTL_COUNT) %>% summarise(Count = n())

T05_M08_FTLW <- T05_M08_FTLW[1:23, ]

T05_M08_CAT <- TIAS2005[, c("TA050895", "CAT")] %>% group_by(TA050895, CAT) %>% summarise(Count = n())

T05_M08_CAT <- T05_M08_CAT[1:12, ]

T05_M08_FTLCAT <- TIAS2005_FTL[, c("TA050895", "CAT")] %>% group_by(TA050895, CAT) %>% summarise(Count = n())

T05_M08_FTLCAT <- T05_M08_FTLCAT[1:6, ]

T05_M08_IACCAT <- TIAS2005_IAC[, c("TA050895", "CAT")] %>% group_by(TA050895, CAT) %>% summarise(Count = n())

T05_M08_IACCAT <- T05_M08_IACCAT[1:6, ]

head(T05_M08_CAT, 12)

ggplot(T05_M08_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050895)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Feeling Soc. Makes Sense in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day"))

head(T05_M08_FTLW, 23)

ggplot(T05_M08_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050895)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Feeling Soc. Makes Sense in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day"))

prop.table(table(TIAS2005_FTL$TA050895))

m08.pie.ftl.05 <- ggplot(data = T05_M08_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050895))) + geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Feeling Soc. Makes Sense in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day")) +
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050895))

m08.pie.iac.05 <- ggplot(data = T05_M08_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050895))) + geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Feeling Soc. Makes Sense in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day")) +
  theme_void()

ggarrange(m08.pie.ftl.05, m08.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### MIDUS M9 - Managing Daily Responsibility =====================================================================================

####
# M9. Freq Feel Managing Responsibility: "In the last month, how often did you feel good at managing the responsibilities of your daily life?”
# Answers: 1 (Never); 2 (Once or twice); 3 (About once a week); 4 (Two or three times a week); 5 (Almost every day); 6 (Every day); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050896)

T05_M09_FTLW <- TIAS[, c("TA050896", "FTL_COUNT")] %>% group_by(TA050896, FTL_COUNT) %>% summarise(Count = n())

T05_M09_FTLW <- T05_M09_FTLW[1:21, ]

T05_M09_CAT <- TIAS2005[, c("TA050896", "CAT")] %>% group_by(TA050896, CAT) %>% summarise(Count = n())

T05_M09_FTLCAT <- TIAS2005_FTL[, c("TA050896", "CAT")] %>% group_by(TA050896, CAT) %>% summarise(Count = n())

T05_M09_IACCAT <- TIAS2005_IAC[, c("TA050896", "CAT")] %>% group_by(TA050896, CAT) %>% summarise(Count = n())

head(T05_M09_CAT, 12)

ggplot(T05_M09_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050896)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Feeling Managing Resp. in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day"))

head(T05_M09_FTLW, 21)

ggplot(T05_M09_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050896)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Feeling Managing Resp. in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day"))

prop.table(table(TIAS2005_FTL$TA050896))

m09.pie.ftl.05 <- ggplot(data = T05_M09_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050896))) + geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Feeling Managing Resp. in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day")) +
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050896))

m09.pie.iac.05 <- ggplot(data = T05_M09_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050896))) + geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Feeling Managing Resp. in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day")) +
  theme_void()

ggarrange(m09.pie.ftl.05, m09.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### MIDUS M10 - Trusting Relationships w/ Others =================================================================================

####
# M10. Freq Feeling Trusting Rels w/ Others: "In the last month, how often did you feel that you have warm and trusting relationships with other people?”
# Answers: 1 (Never); 2 (Once or twice); 3 (About once a week); 4 (Two or three times a week); 5 (Almost every day); 6 (Every day); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050897)

T05_M10_FTLW <- TIAS[, c("TA050897", "FTL_COUNT")] %>% group_by(TA050897, FTL_COUNT) %>% summarise(Count = n())

T05_M10_FTLW <- T05_M10_FTLW[1:22, ]

T05_M10_CAT <- TIAS2005[, c("TA050897", "CAT")] %>% group_by(TA050897, CAT) %>% summarise(Count = n())

T05_M10_FTLCAT <- TIAS2005_FTL[, c("TA050897", "CAT")] %>% group_by(TA050897, CAT) %>% summarise(Count = n())

T05_M10_IACCAT <- TIAS2005_IAC[, c("TA050897", "CAT")] %>% group_by(TA050897, CAT) %>% summarise(Count = n())

head(T05_M10_CAT, 12)

ggplot(T05_M10_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050897)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Feeling Trusting Rels w/ Others in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day"))

head(T05_M10_FTLW, 22)

ggplot(T05_M10_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050897)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Feeling Trusting Rels w/ Others in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day"))

prop.table(table(TIAS2005_FTL$TA050897))

m10.pie.ftl.05 <- ggplot(data = T05_M10_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050897))) + geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Feeling Trusting Rels w/ Others in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day")) +
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050897))

m10.pie.iac.05 <- ggplot(data = T05_M10_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050897))) + geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Feeling Trusting Rels w/ Others in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day")) + 
  theme_void()

ggarrange(m10.pie.ftl.05, m10.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### MIDUS M11 - Challenged to Grow ===============================================================================================

####
# M11. Freq Feeling Challenged to Grow: "In the last month, how often did you feel that you have experiences that challenged you to grow or become a better person?”
# Answers: 1 (Never); 2 (Once or twice); 3 (About once a week); 4 (Two or three times a week); 5 (Almost every day); 6 (Every day); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050898)

T05_M11_FTLW <- TIAS[, c("TA050898", "FTL_COUNT")] %>% group_by(TA050898, FTL_COUNT) %>% summarise(Count = n())

T05_M11_FTLW <- T05_M11_FTLW[1:21, ]

T05_M11_CAT <- TIAS2005[, c("TA050898", "CAT")] %>% group_by(TA050898, CAT) %>% summarise(Count = n())

T05_M11_FTLCAT <- TIAS2005_FTL[, c("TA050898", "CAT")] %>% group_by(TA050898, CAT) %>% summarise(Count = n())

T05_M11_IACCAT <- TIAS2005_IAC[, c("TA050898", "CAT")] %>% group_by(TA050898, CAT) %>% summarise(Count = n())

head(T05_M11_CAT, 11)

ggplot(T05_M11_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050898)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Feeling Challenged to Grow in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day"))

head(T05_M11_FTLW, 21)

ggplot(T05_M11_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050898)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Feeling Challenged to Grow in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day"))

prop.table(table(TIAS2005_FTL$TA050898))

m11.pie.ftl.05 <- ggplot(data = T05_M11_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050898))) + geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Feeling Challenged to Grow in Last Mo.", values = c("lightcoral", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "About once a week", "Two or three times a week", "Almost every day", "Every day")) +
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050898))

m11.pie.iac.05 <- ggplot(data = T05_M11_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050898))) + geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Feeling Challenged to Grow in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day")) + 
  theme_void()

ggarrange(m11.pie.ftl.05, m11.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### MIDUS M12 - Confident of Own Ideas ===========================================================================================

####
# M12. Freq Feeling Confident of Own Ideas: "In the last month, how often did you feel confident to think or express your own ideas and opinions?”
# Answers: 1 (Never); 2 (Once or twice); 3 (About once a week); 4 (Two or three times a week); 5 (Almost every day); 6 (Every day); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050899)

T05_M12_FTLW <- TIAS[, c("TA050899", "FTL_COUNT")] %>% group_by(TA050899, FTL_COUNT) %>% summarise(Count = n())

T05_M12_FTLW <- T05_M12_FTLW[1:21, ]

T05_M12_CAT <- TIAS2005[, c("TA050899", "CAT")] %>% group_by(TA050899, CAT) %>% summarise(Count = n())

T05_M12_FTLCAT <- TIAS2005_FTL[, c("TA050899", "CAT")] %>% group_by(TA050899, CAT) %>% summarise(Count = n())

T05_M12_IACCAT <- TIAS2005_IAC[, c("TA050899", "CAT")] %>% group_by(TA050899, CAT) %>% summarise(Count = n())

head(T05_M12_CAT, 12)

ggplot(T05_M12_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050899)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Feeling Confident of Own Ideas in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day"))

head(T05_M12_FTLW, 21)

ggplot(T05_M12_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050899)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Feeling Confident of Own Ideas in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day"))

prop.table(table(TIAS2005_FTL$TA050899))

m12.pie.ftl.05 <- ggplot(data = T05_M12_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050899))) + geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Feeling Confident of Own Ideas in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day")) +
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050899))

m12.pie.iac.05 <- ggplot(data = T05_M12_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050899))) + geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Feeling Confident of Own Ideas in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day")) +
  theme_void()

ggarrange(m12.pie.ftl.05, m12.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### MIDUS M13 - Liked Personality ================================================================================================

####
# M13. Freq Feeling Liked Personality: "In the last month, how often did you feel that you liked your personality?”
# Answers: 1 (Never); 2 (Once or twice); 3 (About once a week); 4 (Two or three times a week); 5 (Almost every day); 6 (Every day); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050900)

T05_M13_FTLW <- TIAS[, c("TA050900", "FTL_COUNT")] %>% group_by(TA050900, FTL_COUNT) %>% summarise(Count = n())

T05_M13_FTLW <- T05_M13_FTLW[1:20, ]

T05_M13_CAT <- TIAS2005[, c("TA050900", "CAT")] %>% group_by(TA050900, CAT) %>% summarise(Count = n())

T05_M13_FTLCAT <- TIAS2005_FTL[, c("TA050900", "CAT")] %>% group_by(TA050900, CAT) %>% summarise(Count = n())

T05_M13_IACCAT <- TIAS2005_IAC[, c("TA050900", "CAT")] %>% group_by(TA050900, CAT) %>% summarise(Count = n())

head(T05_M13_CAT, 10)

ggplot(T05_M13_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050900)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Feeling Liked Own Personality in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day"))

head(T05_M13_FTLW, 20)

ggplot(T05_M13_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050900)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Feeling Liked Own Personality in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day"))

prop.table(table(TIAS2005_FTL$TA050900))

m13.pie.ftl.05 <- ggplot(data = T05_M13_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050900))) + geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Feeling Liked Own Personality in Last Mo.", values = c("green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("About once a week", "Two or three times a week", "Almost every day", "Every day")) +
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050900))

m13.pie.iac.05 <- ggplot(data = T05_M13_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050900))) + geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Feeling Liked Own Personality in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day")) +
  theme_void()

ggarrange(m13.pie.ftl.05, m13.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### MIDUS M14 - Life Had Direction ===============================================================================================

####
# M14. Freq Feeling Life Had Direction: "In the last month, how often did you feel that your life had a direction or purpose?”
# Answers: 1 (Never); 2 (Once or twice); 3 (About once a week); 4 (Two or three times a week); 5 (Almost every day); 6 (Every day); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050901)

T05_M14_FTLW <- TIAS[, c("TA050901", "FTL_COUNT")] %>% group_by(TA050901, FTL_COUNT) %>% summarise(Count = n())

T05_M14_FTLW <- T05_M14_FTLW[1:21, ]

T05_M14_CAT <- TIAS2005[, c("TA050901", "CAT")] %>% group_by(TA050901, CAT) %>% summarise(Count = n())

T05_M14_FTLCAT <- TIAS2005_FTL[, c("TA050901", "CAT")] %>% group_by(TA050901, CAT) %>% summarise(Count = n())

T05_M14_IACCAT <- TIAS2005_IAC[, c("TA050901", "CAT")] %>% group_by(TA050901, CAT) %>% summarise(Count = n())

head(T05_M14_CAT, 12)

ggplot(T05_M14_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050901)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Feeling Life Had Direction in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day"))

head(T05_M14_FTLW, 21)

ggplot(T05_M14_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050901)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Feeling Life Had Direction in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day"))

prop.table(table(TIAS2005_FTL$TA050901))

m14.pie.ftl.05 <- ggplot(data = T05_M14_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050901))) + geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Feeling Life Had Direction in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day")) +
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050901))

m14.pie.iac.05 <- ggplot(data = T05_M14_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050901))) + geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Feeling Life Had Direction in Last Mo.", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1"), 
  labels = c("Never", "Once or twice", "About once a week", "Two or three times a week", "Almost every day", "Every day")) +
  theme_void()

ggarrange(m14.pie.ftl.05, m14.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### MIDUS Subscale - Emotional ===================================================================================================

####
# MIDUS Subscale - Emotional (M1-M3): Total Average for Frequency of (1) Happiness in Last Month; (2) Interest in Life in Last Month; (3) Feeling Satisfied in Last Month
# Possible Score: 1-6 (Actual value); 9 (At least one component is DK/NA/refused)
####

table(TIAS$TA050935)

T05_MSE_FTLW <- TIAS[, c("TA050935", "FTL_COUNT")] %>% group_by(TA050935, FTL_COUNT) %>% summarise(Count = n())

T05_MSE_FTLW <- T05_MSE_FTLW[1:19, ]

T05_MSE_CAT <- TIAS2005[, c("TA050935", "CAT")] %>% group_by(TA050935, CAT) %>% summarise(Count = n())

ggplot(T05_MSE_FTLW, aes(x = FTL_COUNT, y = TA050935, group = FTL_COUNT, fill = as.factor(FTL_COUNT))) +
  geom_boxplot() + 
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "MIDUS Emotional Subscale Score") + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  scale_y_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + 
  guides(fill = guide_legend(title = "# of FTL Waves"))

ggplot(T05_MSE_CAT, aes(x = CAT, y = TA050935, group = CAT, fill = as.factor(CAT))) +
  geom_boxplot() +
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "MIDUS Emotional Subscale Score") + 
  scale_y_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + 
  guides(fill = guide_legend(title = "Category"))

### MIDUS Subscale - Psychological ===============================================================================================

####
# MIDUS Subscale - Psychological (M9-M14): Total Average for Frequency of (1) Feeling Good at Managing Daily Responsibility; (2) Feeling Has Trusting Relationships with Others; 
# (3) Feeling Challenged to Grow; (4) Feeling Confident of Own Ideas; (5) Feeling Liked Own Personality; (6) Feeling Life Had Direction 
# Possible Score: 1-6 (Actual value); 9 (At least one component is DK/NA/refused)
####

table(TIAS$TA050937)

T05_MSP_FTLW <- TIAS[, c("TA050937", "FTL_COUNT")] %>% group_by(TA050937, FTL_COUNT) %>% summarise(Count = n())

T05_MSP_FTLW <- T05_MSP_FTLW[1:18, ]

T05_MSP_CAT <- TIAS2005[, c("TA050937", "CAT")] %>% group_by(TA050937, CAT) %>% summarise(Count = n())

ggplot(T05_MSP_FTLW, aes(x = FTL_COUNT, y = TA050937, group = FTL_COUNT, fill = as.factor(FTL_COUNT))) +
  geom_boxplot() + 
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "MIDUS Psychological Subscale Score") + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  scale_y_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + 
  guides(fill = guide_legend(title = "# of FTL Waves"))

ggplot(T05_MSP_CAT, aes(x = CAT, y = TA050937, group = CAT, fill = as.factor(CAT))) +
  geom_boxplot() +
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "MIDUS Psychological Subscale Score") + 
  scale_y_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + 
  guides(fill = guide_legend(title = "Category"))

### Time Use - Involved in Arts ==================================================================================================

####
# A1. WTR Involved in Arts: “I would like to ask you about things you like to do in your free time. In the last 12 months, have you been involved/participated in any organized activities related to art, music, or the theater?”
# Answers: 1 (Yes); 5 (No); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050015)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050015 = 9)) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050015 = 9)) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050015 = 9)) 

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050015 = 9)) 

T05_TIA_FTLW <- TIAS[, c("TA050015", "FTL_COUNT")] %>% group_by(TA050015, FTL_COUNT) %>% summarise(Count = n())

T05_TIA_FTLW <- T05_TIA_FTLW[1:9, ]

T05_TIA_CAT <- TIAS2005[, c("TA050015", "CAT")] %>% group_by(TA050015, CAT) %>% summarise(Count = n())

T05_TIA_CAT <- T05_TIA_CAT[1:4, ]

T05_TIA_FTLCAT <- TIAS2005_FTL[, c("TA050015", "CAT")] %>% group_by(TA050015, CAT) %>% summarise(Count = n())

T05_TIA_IACCAT <- TIAS2005_IAC[, c("TA050015", "CAT")] %>% group_by(TA050015, CAT) %>% summarise(Count = n())

T05_TIA_IACCAT <- T05_TIA_IACCAT[1:2, ]

head(T05_TIA_CAT, 4)

ggplot(T05_TIA_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050015))) + 
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("WTR Involved in Arts", values =  c("lightgreen", "lightpink"), labels = c("Yes", "No"))

head(T05_TIA_FTLW, 9)

ggplot(T05_TIA_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050015)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("WTR Involved in Arts", values =  c("lightgreen", "lightpink"), labels = c("Yes", "No"))

prop.table(table(TIAS2005_FTL$TA050015))

tia.pie.ftl.05 <- ggplot(data = T05_TIA_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050015))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() + 
  scale_fill_manual("WTR Involved in Arts", values =  c("lightgreen", "lightpink"), labels = c("Yes", "No"))

prop.table(table(TIAS2005_IAC$TA050015)) 

tia.pie.iac.05 <- ggplot(data = T05_TIA_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050015))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("WTR Involved in Arts", values =  c("lightgreen", "lightpink"), labels = c("Yes", "No"))

ggarrange(tia.pie.ftl.05, tia.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### Time Use - Type of Art (Involvement) =========================================================================================

####
# A2 Type of Arts: “Follow up question: What Activities Are Those?”
# Answers: 6132 (Band/ choir/orchestra - after school); 6133 (Drama/art club - after school); 8420 (Needle-work, including classes (knitting, crocheting,beading, embroidery, cross-stitch, weaving, quilting, macrame)
# 8510 (Arts, arts and crafts, arts unspecified); 8511 (Pottery, ceramics); 8512 (Painting); 8513 (Drawing, coloring); 8514 (Sculpture); 8520 (Literature, literature unspecified); 8521 (Writing [not letters; not homework])
# 8522 (Poetry); 8523 (Writing in a diary); 8610 (Playing a musical instrument [include practicing], whistling--NA which activity); 8620 (Singing for fun, karaoke, special event, or competition)
# 8630 (Acting in/rehearsing for a play); 8640 (Non-social dancing; ballet modern dance, body movement); 8810 (Lessons in dance); 8870 (Music lessons, unspecified); 88,871 (Voice lessons)
# 8872 (Lessons in musical instruments); 9997 (Other activity); 9998 (DK); 9999 (NA; refused); 0 (Inap.: not involved in organized activities related to art, music, or theater in the last 12 months) 
####

table(TIAS$TA050016)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050016 = 9999)) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050016 = 9999)) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050016 = 9999)) 

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050016 = 9999)) 

T05_IIA_FTLW <- TIAS[, c("TA050016", "FTL_COUNT")] %>% group_by(TA050016, FTL_COUNT) %>% summarise(Count = n())

T05_IIA_FTLW <- T05_IIA_FTLW[1:35, ]

T05_IIA_CAT <- TIAS2005[, c("TA050016", "CAT")] %>% group_by(TA050016, CAT) %>% summarise(Count = n())

T05_IIA_CAT <- T05_IIA_CAT[1:24, ]

T05_IIA_FTLCAT <- TIAS2005_FTL[, c("TA050016", "CAT")] %>% group_by(TA050016, CAT) %>% summarise(Count = n())

T05_IIA_FTLCAT <- T05_IIA_FTLCAT[1:5, ]

T05_IIA_IACCAT <- TIAS2005_IAC[, c("TA050016", "CAT")] %>% group_by(TA050016, CAT) %>% summarise(Count = n())

T05_IIA_IACCAT <- T05_IIA_IACCAT[1:19, ]

head(T05_IIA_CAT, 24)

ggplot(T05_IIA_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050016))) + 
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Type of Art Involvement", values =  c("skyblue", "hotpink", "mediumslateblue", "mediumspringgreen", "mediumturquoise", "mediumvioletred", "midnightblue", "violet", "olivedrab1", "orange", 
  "royalblue1", "seagreen1", "yellow", "aquamarine", "cornflowerblue", "coral1", "forestgreen", "wheat", "lawngreen"),  labels = c("Not Involved in Arts/Music/Theater", "After-School Band/Choir/Orchestra", 
  "After-School Drama/Art Club", "Arts & Crafts", "Pottery/Ceramics", "Painting", "Drawing/Coloring", "Literature", "Writing", "Poetry", "Musical Instrument", "Singing", "Play Rehearsal/Acting", "Non-social Dancing/Ballet/Body Movement",
  "Dance Lessons", "Music Lessons - Unspecified", "Voice Lessons", "Musical Instrument Lessons", "Other Activity"))                                                                                                                                       

head(T05_IIA_FTLW, 35)

ggplot(T05_IIA_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050016)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Type of Art Involvement", values =  c("skyblue", "hotpink", "mediumslateblue", "mediumspringgreen", "mediumturquoise", "mediumvioletred", "midnightblue", "violet", "olivedrab1", "orange", 
  "royalblue1", "seagreen1", "yellow", "aquamarine", "cornflowerblue", "coral1", "forestgreen", "wheat", "lawngreen"),  labels = c("Not Involved in Arts/Music/Theater", "After-School Band/Choir/Orchestra", 
  "After-School Drama/Art Club", "Arts & Crafts", "Pottery/Ceramics", "Painting", "Drawing/Coloring", "Literature", "Writing", "Poetry", "Musical Instrument", "Singing", "Play Rehearsal/Acting", "Non-social Dancing/Ballet/Body Movement",
  "Dance Lessons", "Music Lessons - Unspecified", "Voice Lessons", "Musical Instrument Lessons", "Other Activity"))                                                                                                                                       

prop.table(table(TIAS2005_FTL$TA050016)) 

iia.pie.ftl.05 <- ggplot(data = T05_IIA_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050016))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() + 
  scale_fill_manual("Type of Art Involvement", values =  c("skyblue", "olivedrab1", "seagreen1", "yellow", "aquamarine"),  labels = c("Not Involved in Arts/Music/Theater", "Writing", "Singing", "Play Rehearsal/Acting", "Non-social Dancing/Ballet/Body Movement"))
                                                                                                                                                                          
prop.table(table(TIAS2005_IAC$TA050016)) 

iia.pie.iac.05 <- ggplot(data = T05_IIA_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050016))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("Type of Art Involvement", values =  c("skyblue", "hotpink", "mediumslateblue", "mediumspringgreen", "mediumturquoise", "mediumvioletred", "midnightblue", "violet", "olivedrab1", "orange", 
  "royalblue1", "seagreen1", "yellow", "aquamarine", "cornflowerblue", "coral1", "forestgreen", "wheat", "lawngreen"),  labels = c("Not Involved in Arts/Music/Theater", "After-School Band/Choir/Orchestra", 
  "After-School Drama/Art Club", "Arts & Crafts", "Pottery/Ceramics", "Painting", "Drawing/Coloring", "Literature", "Writing", "Poetry", "Musical Instrument", "Singing", "Play Rehearsal/Acting", "Non-social Dancing/Ballet/Body Movement",
  "Dance Lessons", "Music Lessons - Unspecified", "Voice Lessons", "Musical Instrument Lessons", "Other Activity"))                                                                                                                                       

ggarrange(iia.pie.ftl.05, iia.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### Time Use - Art Involvement Frequency (Over 12 Mos) ===========================================================================

####
# A3. How Often Participated In Arts: “During the last 12 months, about how often did you participate in any activity related to art, music, or the theater?"
# Answers: 1 (Less than once a month); 2 (At least once a month); 3 (Once a week); 4 (Several times a week); 5 (Almost every day); 
# 6 (Every day); 7 (Never); 8 (DK); 9 (NA/refused); 0  (Inap.: not involved in organized activities related to art, music, or theater in the last 12 months)
####

table(TIAS$TA050017)

T05_AIF_FTLW <- TIAS[, c("TA050017", "FTL_COUNT")] %>% group_by(TA050017, FTL_COUNT) %>% summarise(Count = n())

T05_AIF_FTLW <- T05_AIF_FTLW[1:22, ]

T05_AIF_CAT <- TIAS2005[, c("TA050017", "CAT")] %>% group_by(TA050017, CAT) %>% summarise(Count = n())

T05_AIF_FTLCAT <- TIAS2005_FTL[, c("TA050017", "CAT")] %>% group_by(TA050017, CAT) %>% summarise(Count = n())

T05_AIF_IACCAT <- TIAS2005_IAC[, c("TA050017", "CAT")] %>% group_by(TA050017, CAT) %>% summarise(Count = n())

head(T05_AIF_CAT, 12)

ggplot(T05_AIF_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050017))) + 
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Time Use - Art Involvement Frequency", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise"), 
                    labels = c("Not Involved in Arts/Music/Theater", "Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day"))

head(T05_AIF_FTLW, 22)

ggplot(T05_AIF_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050017)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Time Use - Art Involvement Frequency", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise"), 
                    labels = c("Not Involved in Arts/Music/Theater", "Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day"))

prop.table(table(TIAS2005_FTL$TA050017)) 

aif.pie.ftl.05 <- ggplot(data = T05_AIF_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050017))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() + 
  scale_fill_manual("Time Use - Art Involvement Frequency", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "#0072B2"), 
                    labels = c("Not Involved in Arts/Music/Theater", "Less than once a month", "At least once a month", "Once a week", "Almost every day"))

prop.table(table(TIAS2005_IAC$TA050017)) 

aif.pie.iac.05 <- ggplot(data = T05_AIF_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050017))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("Time Use - Art Involvement Frequency", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise"), 
                    labels = c("Not Involved in Arts/Music/Theater", "Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day"))

ggarrange(aif.pie.ftl.05, aif.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### WTR Member of Sports Team (Over 12 Mos) ======================================================================================

####
# A4. WTR Member of Sports Team: “Were you a member of any athletic or sports teams in the last 12 months?”
# Answers: 1 (Yes); 5 (No); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050018)

T05_SPO_FTLW <- TIAS[, c("TA050018", "FTL_COUNT")] %>% group_by(TA050018, FTL_COUNT) %>% summarise(Count = n())

T05_SPO_FTLW <- T05_SPO_FTLW[1:10, ]

T05_SPO_CAT <- TIAS2005[, c("TA050018", "CAT")] %>% group_by(TA050018, CAT) %>% summarise(Count = n())

T05_SPO_FTLCAT <- TIAS2005_FTL[, c("TA050018", "CAT")] %>% group_by(TA050018, CAT) %>% summarise(Count = n())

T05_SPO_IACCAT <- TIAS2005_IAC[, c("TA050018", "CAT")] %>% group_by(TA050018, CAT) %>% summarise(Count = n())

head(T05_SPO_CAT, 4)

ggplot(T05_SPO_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050018))) + 
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("WTR Member of Sports Team", values =  c("lightgreen", "lightpink"), labels = c("Yes", "No"))

head(T05_SPO_FTLW, 10)

ggplot(T05_SPO_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050018)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("WTR Member of Sports Team", values =  c("lightgreen", "lightpink"), labels = c("Yes", "No"))

prop.table(table(TIAS2005_FTL$TA050019))

spo.pie.ftl.05 <- ggplot(data = T05_SPO_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050018))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() + 
  scale_fill_manual("WTR Member of Sports Team", values =  c("lightgreen", "lightpink"), labels = c("Yes", "No"))

prop.table(table(TIAS2005_IAC$TA050019)) 

spo.pie.iac.05 <- ggplot(data = T05_SPO_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050018))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("WTR Member of Sports Team", values =  c("lightgreen", "lightpink"), labels = c("Yes", "No"))

ggarrange(spo.pie.ftl.05, spo.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### Time Use - Involved in Sports Frequency  =====================================================================================

####
# A5. How Often Participated in Sports: “During the last 12 months, how often did you spend time on athletic or sports teams?"
# Answers: 1 (Less than once a month); 2 (At least once a month); 3 (Once a week); 4 (Several times a week); 5 (Almost every day); 
# 6 (Every day); 7 (Never); 8 (DK); 9 (NA/refused); 0 (Inap: not a member of athletic or sports teams)
####

table(TIAS$TA050019)

T05_IIS_FTLW <- TIAS[, c("TA050019", "FTL_COUNT")] %>% group_by(TA050019, FTL_COUNT) %>% summarise(Count = n())

T05_IIS_FTLW <- T05_IIS_FTLW[1:21, ]

T05_IIS_CAT <- TIAS2005[, c("TA050019", "CAT")] %>% group_by(TA050019, CAT) %>% summarise(Count = n())

T05_IIS_FTLCAT <- TIAS2005_FTL[, c("TA050019", "CAT")] %>% group_by(TA050019, CAT) %>% summarise(Count = n())

T05_IIS_IACCAT <- TIAS2005_IAC[, c("TA050019", "CAT")] %>% group_by(TA050019, CAT) %>% summarise(Count = n())

head(T05_IIS_CAT, 13)

ggplot(T05_IIS_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050019))) + 
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Time Use - Involved in Sports", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise"), 
                    labels = c("Not Involved in Sports", "Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day"))

head(T05_IIS_FTLW, 21)

ggplot(T05_IIS_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050019)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Time Use - Involved in Sports", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise"), 
                    labels = c("Not Involved in Sports", "Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day"))

prop.table(table(TIAS2005_FTL$TA050019))

iis.pie.ftl.05 <- ggplot(data = T05_IIS_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050019))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() + 
  scale_fill_manual("Time Use - Involved in Sports", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "gold", "turquoise"), 
                    labels = c("Not Involved in Sports", "Less than once a month", "At least once a month", "Once a week", "Several times a week", "Every day"))

prop.table(table(TIAS2005_IAC$TA050019)) 

iis.pie.iac.05 <- ggplot(data = T05_IIS_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050019))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("Time Use - Involved in Sports", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise"), 
                    labels = c("Not Involved in Sports", "Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day"))

ggarrange(iis.pie.ftl.05, iis.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### Time Use - Watching/Reading News (Over 12 Mos) ===============================================================================

####
# A6. How Often Watch/Read News (TV or Magazine): “During the last 12 months, about how often did you watch or read news?"
# Answers: 1 (Less than once a month); 2 (At least once a month); 3 (Once a week); 4 (Several times a week); 5 (Almost every day); 
# 6 (Every day); 7 (Never); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050020)

T05_WRN_FTLW <- TIAS[, c("TA050020", "FTL_COUNT")] %>% group_by(TA050020, FTL_COUNT) %>% summarise(Count = n())

T05_WRN_FTLW <- T05_WRN_FTLW[1:23, ]

T05_WRN_CAT <- TIAS2005[, c("TA050020", "CAT")] %>% group_by(TA050020, CAT) %>% summarise(Count = n())

T05_WRN_FTLCAT <- TIAS2005_FTL[, c("TA050020", "CAT")] %>% group_by(TA050020, CAT) %>% summarise(Count = n())

T05_WRN_IACCAT <- TIAS2005_IAC[, c("TA050020", "CAT")] %>% group_by(TA050020, CAT) %>% summarise(Count = n())

head(T05_WRN_CAT, 13)

ggplot(T05_WRN_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050020))) + 
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Time Use - Watching/Reading News", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise"), 
                    labels = c("Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day", "Never watch or read news"))

head(T05_WRN_FTLW, 23)

ggplot(T05_WRN_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050020)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Time Use - Watching/Reading News", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise"), 
                    labels = c("Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day", "Never watch or read news"))

prop.table(table(TIAS2005_FTL$TA050020))

wrn.pie.ftl.05 <- ggplot(data = T05_WRN_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050020))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() + 
  scale_fill_manual("Time Use - Watching/Reading News", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2"), 
                    labels = c("Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day"))

prop.table(table(TIAS2005_IAC$TA050020)) 

wrn.pie.iac.05 <- ggplot(data = T05_WRN_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050020))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("Time Use - Watching/Reading News", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise"), 
                    labels = c("Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day", "Never watch or read news"))

ggarrange(wrn.pie.ftl.05, wrn.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### Time Use - Reading for Pleasure (Over 12 Mos) ================================================================================

####
# A7. How Often Read for Pleasure: “During the last 12 months, about how often did you read for pleasure?"
# Answers: 1 (Less than once a month); 2 (At least once a month); 3 (Once a week); 4 (Several times a week); 5 (Almost every day); 
# 6 (Every day); 7 (Never); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050021)

T05_RFP_FTLW <- TIAS[, c("TA050021", "FTL_COUNT")] %>% group_by(TA050021, FTL_COUNT) %>% summarise(Count = n())

T05_RFP_FTLW <- T05_RFP_FTLW[1:26, ]

T05_RFP_CAT <- TIAS2005[, c("TA050021", "CAT")] %>% group_by(TA050021, CAT) %>% summarise(Count = n())

T05_RFP_FTLCAT <- TIAS2005_FTL[, c("TA050021", "CAT")] %>% group_by(TA050021, CAT) %>% summarise(Count = n())

T05_RFP_IACCAT <- TIAS2005_IAC[, c("TA050021", "CAT")] %>% group_by(TA050021, CAT) %>% summarise(Count = n())

head(T05_RFP_CAT, 14)

ggplot(T05_RFP_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050021))) + 
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Time Use - Reading for Pleasure", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise"), 
                  labels = c("Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day", "Never read for pleasure"))

head(T05_RFP_FTLW, 26)

ggplot(T05_RFP_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050021)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Time Use - Reading for Pleasure", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise"), 
                    labels = c("Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day", "Never read for pleasure"))

prop.table(table(TIAS2005_FTL$TA050021))

rfp.pie.ftl.05 <- ggplot(data = T05_RFP_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050021))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() + 
  scale_fill_manual("Time Use - Reading for Pleasure", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise"), 
                    labels = c("Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day", "Never read for pleasure"))

prop.table(table(TIAS2005_IAC$TA050021)) 

rfp.pie.iac.05 <- ggplot(data = T05_RFP_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050021))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("Time Use - Reading for Pleasure", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise"), 
                    labels = c("Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day", "Never read for pleasure"))

ggarrange(rfp.pie.ftl.05, rfp.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### Time Use - Watching Non-News TV Shows (Over 12 Mos) ==========================================================================

####
# A8. How Often Watch Non-News TV Shows: “During the last 12 months, about how often did you watch non-news TV shows?"
# Answers: 1 (Less than once a month); 2 (At least once a month); 3 (Once a week); 4 (Several times a week); 5 (Almost every day); 
# 6 (Every day); 7 (Never); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050022)

T05_NNT_FTLW <- TIAS[, c("TA050022", "FTL_COUNT")] %>% group_by(TA050022, FTL_COUNT) %>% summarise(Count = n())

T05_NNT_FTLW <- T05_NNT_FTLW[1:21, ]

T05_NNT_CAT <- TIAS2005[, c("TA050022", "CAT")] %>% group_by(TA050022, CAT) %>% summarise(Count = n())

T05_NNT_FTLCAT <- TIAS2005_FTL[, c("TA050022", "CAT")] %>% group_by(TA050022, CAT) %>% summarise(Count = n())

T05_NNT_IACCAT <- TIAS2005_IAC[, c("TA050022", "CAT")] %>% group_by(TA050022, CAT) %>% summarise(Count = n())

head(T05_NNT_CAT, 12)

ggplot(T05_NNT_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050022))) + 
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Time Use - Non-News TV Shows", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise"), 
                    labels = c("Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day", "Never watched non-news TV shows"))

head(T05_NNT_FTLW, 21)

ggplot(T05_NNT_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050022)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Time Use - Non-News TV Shows", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise"), 
                    labels = c("Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day", "Never watched non-news TV shows"))

prop.table(table(TIAS2005_FTL$TA050022))

nnt.pie.ftl.05 <- ggplot(data = T05_NNT_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050022))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() + 
  scale_fill_manual("Time Use - Non-News TV Shows", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2"), 
                    labels = c("At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day"))

prop.table(table(TIAS2005_IAC$TA050022)) 

nnt.pie.iac.05 <- ggplot(data = T05_NNT_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050022))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("Time Use - Non-News TV Shows", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise"), 
                    labels = c("Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day", "Never watched non-news TV shows"))

ggarrange(nnt.pie.ftl.05, nnt.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### Time Use - Internet Usage ====================================================================================================

#### 
# A9. WTR Ever Used Internet: “Have you ever used the internet?”
# Answers: 1 (Yes); 5 (No); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050023)

T05_ITU_FTLW <- TIAS[, c("TA050023", "FTL_COUNT")] %>% group_by(TA050023, FTL_COUNT) %>% summarise(Count = n())

T05_ITU_FTLW <- T05_ITU_FTLW[1:9, ]

T05_ITU_CAT <- TIAS2005[, c("TA050023", "CAT")] %>% group_by(TA050023, CAT) %>% summarise(Count = n())

T05_ITU_FTLCAT <- TIAS2005_FTL[, c("TA050023", "CAT")] %>% group_by(TA050023, CAT) %>% summarise(Count = n())

T05_ITU_IACCAT <- TIAS2005_IAC[, c("TA050023", "CAT")] %>% group_by(TA050023, CAT) %>% summarise(Count = n())

head(T05_ITU_CAT, 4)

ggplot(T05_ITU_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050023))) + 
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("WTR Ever Used Internet", values =  c("lightgreen", "lightpink"), labels = c("Yes", "No"))

head(T05_ITU_FTLW, 9)

ggplot(T05_ITU_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050023)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("WTR Ever Used Internet", values =  c("lightgreen", "lightpink"), labels = c("Yes", "No"))

prop.table(table(TIAS2005_FTL$TA050023))

itu.pie.ftl.05 <- ggplot(data = T05_ITU_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050023))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() + 
  scale_fill_manual("WTR Ever Used Internet", values =  c("lightgreen", "lightpink"), labels = c("Yes", "No"))

prop.table(table(TIAS2005_IAC$TA050023)) 

itu.pie.iac.05 <- ggplot(data = T05_ITU_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050023))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("WTR Ever Used Internet", values =  c("lightgreen", "lightpink"), labels = c("Yes", "No"))

ggarrange(itu.pie.ftl.05, itu.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### Time Use - Internet Usage for Email/Instant Messenger (Over 12 Mos) ==========================================================

####
# A10A. WTR Used Internet for email/instant messenger: “In the last 12 months, how often did you use the Internet for email/instant messenger?"
# Answers: 1 (Less than once a month); 2 (At least once a month); 3 (Once a week); 4 (Several times a week); 5 (Almost every day); 
# 6 (Every day); 7 (Never); 8 (DK); 9 (NA/refused); 0 (Inap: never used the Internet)
####

table(TIAS$TA050024)

T05_IPE_FTLW <- TIAS[, c("TA050024", "FTL_COUNT")] %>% group_by(TA050024, FTL_COUNT) %>% summarise(Count = n())

T05_IPE_FTLW <- T05_IPE_FTLW[1:29, ]

T05_IPE_CAT <- TIAS2005[, c("TA050024", "CAT")] %>% group_by(TA050024, CAT) %>% summarise(Count = n())

T05_IPE_FTLCAT <- TIAS2005_FTL[, c("TA050024", "CAT")] %>% group_by(TA050024, CAT) %>% summarise(Count = n())

T05_IPE_IACCAT <- TIAS2005_IAC[, c("TA050024", "CAT")] %>% group_by(TA050024, CAT) %>% summarise(Count = n())

head(T05_IPE_CAT, 16)

ggplot(T05_IPE_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050024))) + 
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Internet Usage for Email/Instant Messenger", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumpurple1"), 
                    labels = c("Never used the internet", "Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day", "Never used the internet for email/instant messenger"))

head(T05_IPE_FTLW, 29)

ggplot(T05_IPE_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050024)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Internet Usage for Email/Instant Messenger", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumpurple1"), 
                    labels = c("Never used the internet", "Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day", "Never used the internet for email/instant messenger"))

prop.table(table(TIAS2005_FTL$TA050024))

ipe.pie.ftl.05 <- ggplot(data = T05_IPE_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050024))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() + 
  scale_fill_manual("Internet Usage for Email/Instant Messenger", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumpurple1"), 
                    labels = c("Never used the internet", "Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day", "Never used the internet for email/instant messenger"))

prop.table(table(TIAS2005_IAC$TA050024)) 

ipe.pie.iac.05 <- ggplot(data = T05_IPE_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050024))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("Internet Usage for Email/Instant Messenger", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumpurple1"), 
                    labels = c("Never used the internet", "Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day", "Never used the internet for email/instant messenger"))

ggarrange(ipe.pie.ftl.05, ipe.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### Time Use - Internet Usage for School/Research (Over 12 Mos) ==================================================================

####
# A10B. WTR Used Internet for school/research for school-related papers and projects: “In the last 12 months, how often did you use 
# the Internet for school-related papers and projects?"
# Answers: 1 (Less than once a month); 2 (At least once a month); 3 (Once a week); 4 (Several times a week); 5 (Almost every day); 
# 6 (Every day); 7 (Never); 8 (DK); 9 (NA/refused); 0 (Inap: never used the Internet)
####

table(TIAS$TA050025)

T05_IPR_FTLW <- TIAS[, c("TA050025", "FTL_COUNT")] %>% group_by(TA050025, FTL_COUNT) %>% summarise(Count = n())

T05_IPR_FTLW <- T05_IPR_FTLW[1:29, ]

T05_IPR_CAT <- TIAS2005[, c("TA050025", "CAT")] %>% group_by(TA050025, CAT) %>% summarise(Count = n())

T05_IPR_FTLCAT <- TIAS2005_FTL[, c("TA050025", "CAT")] %>% group_by(TA050025, CAT) %>% summarise(Count = n())

T05_IPR_IACCAT <- TIAS2005_IAC[, c("TA050025", "CAT")] %>% group_by(TA050025, CAT) %>% summarise(Count = n())

head(T05_IPR_CAT, 15)

ggplot(T05_IPR_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050025))) + 
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Internet Usage for School/Research", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumpurple1"), 
                    labels = c("Never used the internet", "Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day", "Never used the internet for school/research"))

head(T05_IPR_FTLW, 29)

ggplot(T05_IPR_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050025)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Internet Usage for School/Research", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumpurple1"), 
                    labels = c("Never used the internet", "Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day", "Never used the internet for school/research"))

prop.table(table(TIAS2005_FTL$TA050025))

ipr.pie.ftl.05 <- ggplot(data = T05_IPR_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050025))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() + 
  scale_fill_manual("Internet Usage for School/Research", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "mediumpurple1"), 
                    labels = c("Never used the internet", "Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Never used the internet for school/research"))

prop.table(table(TIAS2005_IAC$TA050025))

ipr.pie.iac.05 <- ggplot(data = T05_IPR_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050025))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("Internet Usage for School/Research", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumpurple1"), 
                    labels = c("Never used the internet", "Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day", "Never used the internet for school/research"))

ggarrange(ipr.pie.ftl.05, ipr.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### Time Use - Internet Usage for Shopping (Over 12 Mos) =========================================================================

####
# A10C. WTR Used Internet for shopping: “In the last 12 months, how often did you use the Internet for shopping?"
# Answers: 1 (Less than once a month); 2 (At least once a month); 3 (Once a week); 4 (Several times a week); 5 (Almost every day); 
# 6 (Every day); 7 (Never); 8 (DK); 9 (NA/refused); 0 (Inap: never used the Internet)
####

table(TIAS$TA050026)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050026 = 8)) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050026 = 8)) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050026 = 8)) 

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050026 = 8)) 

T05_IPS_FTLW <- TIAS[, c("TA050026", "FTL_COUNT")] %>% group_by(TA050026, FTL_COUNT) %>% summarise(Count = n())

T05_IPS_FTLW <- T05_IPS_FTLW[1:26, ]

T05_IPS_CAT <- TIAS2005[, c("TA050026", "CAT")] %>% group_by(TA050026, CAT) %>% summarise(Count = n())

T05_IPS_CAT <- T05_IPS_CAT[1:15, ]

T05_IPS_FTLCAT <- TIAS2005_FTL[, c("TA050026", "CAT")] %>% group_by(TA050026, CAT) %>% summarise(Count = n())

T05_IPS_IACCAT <- TIAS2005_IAC[, c("TA050026", "CAT")] %>% group_by(TA050026, CAT) %>% summarise(Count = n())

T05_IPS_IACCAT <- T05_IPS_IACCAT[1:8, ]

head(T05_IPS_CAT, 15)

ggplot(T05_IPS_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050026))) + 
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Internet Usage for Shopping", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumpurple1"), 
                    labels = c("Never used the internet", "Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day", "Never used the internet for shopping"))

head(T05_IPS_FTLW, 26)

ggplot(T05_IPS_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050026)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Internet Usage for Shopping", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumpurple1"), 
                    labels = c("Never used the internet", "Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day", "Never used the internet for shopping"))

prop.table(table(TIAS2005_FTL$TA050026))

ips.pie.ftl.05 <- ggplot(data = T05_IPS_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050026))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() + 
  scale_fill_manual("Internet Usage for Shopping", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "gold", "turquoise", "mediumpurple1"), 
                    labels = c("Never used the internet", "Less than once a month", "At least once a month", "Once a week", "Several times a week", "Every day", "Never used the internet for shopping"))

prop.table(table(TIAS2005_IAC$TA050026))

ips.pie.iac.05 <- ggplot(data = T05_IPS_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050026))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("Internet Usage for Shopping", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumpurple1"), 
                    labels = c("Never used the internet", "Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day", "Never used the internet for shopping"))

ggarrange(ips.pie.ftl.05, ips.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### Time Use - Internet Usage for Playing Games (Over 12 Mos) ====================================================================

####
# A10D. WTR Used Internet for playing games: “In the last 12 months, how often did you use the Internet for playing games?"
# Answers: 1 (Less than once a month); 2 (At least once a month); 3 (Once a week); 4 (Several times a week); 5 (Almost every day); 
# 6 (Every day); 7 (Never); 8 (DK); 9 (NA/refused); 0 (Inap: never used the Internet)
####

table(TIAS$TA050027)

T05_IPG_FTLW <- TIAS[, c("TA050027", "FTL_COUNT")] %>% group_by(TA050027, FTL_COUNT) %>% summarise(Count = n())

T05_IPG_FTLW <- T05_IPG_FTLW[1:28, ]

T05_IPG_CAT <- TIAS2005[, c("TA050027", "CAT")] %>% group_by(TA050027, CAT) %>% summarise(Count = n())

T05_IPG_FTLCAT <- TIAS2005_FTL[, c("TA050027", "CAT")] %>% group_by(TA050027, CAT) %>% summarise(Count = n())

T05_IPG_IACCAT <- TIAS2005_IAC[, c("TA050027", "CAT")] %>% group_by(TA050027, CAT) %>% summarise(Count = n())

head(T05_IPG_CAT, 16)

ggplot(T05_IPG_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050027))) + 
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Internet Usage for Playing Games", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumpurple1"), 
                    labels = c("Never used the internet", "Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day", "Never used the internet for gaming"))

head(T05_IPG_FTLW, 28)

ggplot(T05_IPG_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050027)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Internet Usage for Playing Games", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumpurple1"), 
                    labels = c("Never used the internet", "Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day", "Never used the internet for gaming"))

prop.table(table(TIAS2005_FTL$TA050027))

ipg.pie.ftl.05 <- ggplot(data = T05_IPG_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050027))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() + 
  scale_fill_manual("Internet Usage for Playing Games", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumpurple1"), 
                    labels = c("Never used the internet", "Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day", "Never used the internet for gaming"))

prop.table(table(TIAS2005_IAC$TA050027))

ipg.pie.iac.05 <- ggplot(data = T05_IPG_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050027))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("Internet Usage for Playing Games", values =  c("darkolivegreen1", "lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumpurple1"), 
                    labels = c("Never used the internet", "Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day", "Never used the internet for gaming"))

ggarrange(ipg.pie.ftl.05, ipg.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### Time Use - Voted in Last Presidential Election ===============================================================================

####
# A11. WTR Voted in 2004: “Did you vote in the national election for President last November, in 2004?”
# Answers: 1 (Yes); 5 (No); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050028)

T05_VLP_FTLW <- TIAS[, c("TA050028", "FTL_COUNT")] %>% group_by(TA050028, FTL_COUNT) %>% summarise(Count = n())

T05_VLP_FTLW <- T05_VLP_FTLW[1:10, ]

T05_VLP_CAT <- TIAS2005[, c("TA050028", "CAT")] %>% group_by(TA050028, CAT) %>% summarise(Count = n())

head(T05_VLP_CAT, 4)

ggplot(T05_VLP_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050028))) + 
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Voted in 2004 Presidential Election", values =  c("lightgreen", "lightpink"), labels = c("Yes", "No"))

head(T05_VLP_FTLW, 10)

ggplot(T05_VLP_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050028)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Voted in 2004 Presidential Election", values =  c("lightgreen", "lightpink"), labels = c("Yes", "No"))

### Time Use - Involvement in Social Action Groups ===============================================================================

####
# A11B. WTR In Social Action Groups: “During the last 12 months, were you involved in any political groups, solidarity or 
# ethnic-support groups, such as NAACP, or social-action groups?”
# Answers: 1 (Yes); 5 (No); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050029)

T05_ISA_FTLW <- TIAS[, c("TA050029", "FTL_COUNT")] %>% group_by(TA050029, FTL_COUNT) %>% summarise(Count = n())

T05_ISA_FTLW <- T05_ISA_FTLW[1:8, ]

T05_ISA_CAT <- TIAS2005[, c("TA050029", "CAT")] %>% group_by(TA050029, CAT) %>% summarise(Count = n())

head(T05_ISA_CAT, 3)

ggplot(T05_ISA_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050029))) + 
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Social Action Group Involvement", values =  c("lightskyblue1", "pink"), labels = c("Yes", "No"))

head(T05_ISA_FTLW, 8)

ggplot(T05_ISA_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050029)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Social Action Group Involvement", values =  c("lightskyblue1", "pink"), labels = c("Yes", "No"))

### Time Use - Type of Social Action Group (If Involved) =========================================================================

####
# A11C. Type of Social Action Groups: “Which political groups were you involved in?”
# Answers: 1 (Young Democrats/Democratic Party); 2 (Young Republicans/Republican Party); 3 (NAACP); 4 (Amnesty International/social justice groups)
# 7 (Other); 8 (DK); 9 (NA/refused); 0 (Inap.: not involved in political, solidarity, or ethnic-support groups in the last 12 months)
####

table(TIAS$TA050030)

T05_TSA_FTLW <- TIAS[, c("TA050030", "FTL_COUNT")] %>% group_by(TA050030, FTL_COUNT) %>% summarise(Count = n())

T05_TSA_FTLW <- T05_TSA_FTLW[1:12, ]

T05_TSA_CAT <- TIAS2005[, c("TA050030", "CAT")] %>% group_by(TA050030, CAT) %>% summarise(Count = n())

head(T05_TSA_CAT, 7)

ggplot(T05_TSA_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050030))) + 
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Social Action Group Type", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise"), 
                    labels = c("Not Involved", "Young Democrats", "Young Republicans", "NAACP", "Amnesty International/Social Justice", "Other"))

head(T05_TSA_FTLW, 13)

ggplot(T05_TSA_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050030)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Social Action Group Type", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise"), 
                    labels = c("Not Involved", "Young Democrats", "Young Republicans", "NAACP", "Amnesty International/Social Justice", "Other"))

### Time Use - Social Action Group Involvement (Over 12 Mos) =====================================================================

####
# A11D. How Often Did Social Action Groups: “During the last 12 months, about how often were you involved in social action groups?" 
# Answers: 1 (Less than once a month); 2 (At least once a month); 3 (Once a week); 4 (Several times a week)
# 5 (Almost every day); 6 (Every day); 8 (DK); 9 (NA/refused); 0 (Inap.: not involved in political, solidarity or ethnic-support groups)
####

table(TIAS$TA050031)

T05_SAG_FTLW <- TIAS[, c("TA050031", "FTL_COUNT")] %>% group_by(TA050031, FTL_COUNT) %>% summarise(Count = n())

T05_SAG_FTLW <- T05_SAG_FTLW[1:13, ]

T05_SAG_CAT <- TIAS2005[, c("TA050031", "CAT")] %>% group_by(TA050031, CAT) %>% summarise(Count = n())

head(T05_SAG_CAT, 8)

ggplot(T05_SAG_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050031))) + 
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Social Action Group Involvement", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumpurple1"), 
                    labels = c("Not Involved", "Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day"))

head(T05_SAG_FTLW, 13)

ggplot(T05_SAG_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050031)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Social Action Group Involvement", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumpurple1"), 
                    labels = c("Not Involved", "Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day"))

### Time Use - School Club/Student Gov Involvement ===============================================================================

####
# A12. WTR Involved With School Clubs: “In the last 12 months, were you involved with any school clubs or student government?
# Answers: 1 (Yes); 5 (No); 6 (Not in school); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050032)

T05_CGI_FTLW <- TIAS[, c("TA050032", "FTL_COUNT")] %>% group_by(TA050032, FTL_COUNT) %>% summarise(Count = n())

T05_CGI_FTLW <- T05_CGI_FTLW[1:10, ]

T05_CGI_CAT <- TIAS2005[, c("TA050032", "CAT")] %>% group_by(TA050032, CAT) %>% summarise(Count = n())

T05_CGI_FTLCAT <- TIAS2005_FTL[, c("TA050032", "CAT")] %>% group_by(TA050032, CAT) %>% summarise(Count = n())

T05_CGI_IACCAT <- TIAS2005_IAC[, c("TA050032", "CAT")] %>% group_by(TA050032, CAT) %>% summarise(Count = n())

head(T05_CGI_CAT, 5)

ggplot(T05_CGI_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050032))) + 
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("School Club/Student Gov Involvement", values =  c("darkseagreen1", "lightskyblue1", "pink"), labels = c("Yes", "No", "Not in school"))

head(T05_CGI_FTLW, 10)

ggplot(T05_CGI_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050032)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("School Club/Student Gov Involvement", values =  c("darkseagreen1", "lightskyblue1", "pink"), labels = c("Yes", "No", "Not in school"))

prop.table(table(TIAS2005_FTL$TA050033))

cgi.pie.ftl.05 <- ggplot(data = T05_CGI_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050032))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() + 
  scale_fill_manual("School Club/Student Gov Involvement", values =  c("darkseagreen1", "lightskyblue1"), labels = c("Yes", "No"))

prop.table(table(TIAS2005_IAC$TA050033))

cgi.pie.iac.05 <- ggplot(data = T05_CGI_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050032))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("School Club/Student Gov Involvement", values =  c("darkseagreen1", "lightskyblue1", "pink"), labels = c("Yes", "No", "Not in school"))

ggarrange(cgi.pie.ftl.05, cgi.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### Time Use - School Club/Student Gov Involvement Frequency (Over 12 Mos) =======================================================

####
# A12B. How Often Did School Clubs: “During the last 12 months, about how often were you involved in school clubs or student government? (Would you say: 
# less than once a month, at least once a month, once a week, several times a week, almost every day, or every day?)”
# Answers: 1 (Less than once a month); 2 (At least once a month); 3 (Once a week); 4 (Several times a week)
# 5 (Almost every day); 6 (Every day); 8 (DK); 9 (NA/refused); 0 (Inap.: not involved with school clubs or student government)
####

table(TIAS$TA050033)

T05_SCG_FTLW <- TIAS[, c("TA050033", "FTL_COUNT")] %>% group_by(TA050033, FTL_COUNT) %>% summarise(Count = n())

T05_SCG_FTLW <- T05_SCG_FTLW[1:18, ]

T05_SCG_CAT <- TIAS2005[, c("TA050033", "CAT")] %>% group_by(TA050033, CAT) %>% summarise(Count = n())

T05_SCG_FTLCAT <- TIAS2005_FTL[, c("TA050033", "CAT")] %>% group_by(TA050033, CAT) %>% summarise(Count = n())

T05_SCG_IACCAT <- TIAS2005_IAC[, c("TA050033", "CAT")] %>% group_by(TA050033, CAT) %>% summarise(Count = n())

head(T05_SCG_CAT, 9)

ggplot(T05_SCG_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050033))) + 
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("School Club/Student Gov Involvement (Over 12mos)", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumpurple1"), 
                    labels = c("Not Involved", "Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day"))

head(T05_SCG_FTLW, 18)

ggplot(T05_SCG_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050033)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("School Club/Student Gov Involvement (Over 12mos)", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumpurple1"), 
                    labels = c("Not Involved", "Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day"))

prop.table(table(TIAS2005_FTL$TA050033))

scg.pie.ftl.05 <- ggplot(data = T05_SCG_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050033))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() + 
  scale_fill_manual("School Club/Student Gov Involvement (Over 12mos)", values =  c("lightcoral", "gold"), 
                    labels = c("Not Involved", "Once a week"))

prop.table(table(TIAS2005_IAC$TA050033))

scg.pie.iac.05 <- ggplot(data = T05_SCG_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050033))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("School Club/Student Gov Involvement Frequency", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumpurple1"), 
                    labels = c("Not Involved", "Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day"))

ggarrange(scg.pie.ftl.05, scg.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### Time Use - Unpaid Volunteer/Community Sevice Work (Over 12 Mos) ==============================================================

####
# A13. WTR Did OTR Volunteer Work: “During the last 12 months, did you do any unpaid volunteer or community service work that you have not told me about?”
# Answers: 1 (Yes); 5 (No); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050034)

T05_UVW_FTLW <- TIAS[, c("TA050034", "FTL_COUNT")] %>% group_by(TA050034, FTL_COUNT) %>% summarise(Count = n())

T05_UVW_FTLW <- T05_UVW_FTLW[1:9, ]

T05_UVW_CAT <- TIAS2005[, c("TA050034", "CAT")] %>% group_by(TA050034, CAT) %>% summarise(Count = n())

T05_UVW_FTLCAT <- TIAS2005_FTL[, c("TA050034", "CAT")] %>% group_by(TA050034, CAT) %>% summarise(Count = n())

T05_UVW_IACCAT <- TIAS2005_IAC[, c("TA050034", "CAT")] %>% group_by(TA050034, CAT) %>% summarise(Count = n())

head(T05_UVW_CAT, 4)

ggplot(T05_UVW_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050034))) + 
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Unpaid Volunteer Work (Over 12mos)", values =  c("turquoise", "mediumpurple1"), labels = c("Yes", "No"))
                    
head(T05_UVW_FTLW, 9)

ggplot(T05_UVW_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050034)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Unpaid Volunteer Work (Over 12mos)", values =  c("turquoise", "mediumpurple1"), labels = c("Yes", "No"))

prop.table(table(TIAS2005_FTL$TA050034))

uvw.pie.ftl.05 <- ggplot(data = T05_UVW_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050034))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() + 
  scale_fill_manual("Unpaid Volunteer Work (Over 12mos)", values =  c("turquoise", "mediumpurple1"), labels = c("Yes", "No"))

prop.table(table(TIAS2005_IAC$TA050034))

uvw.pie.iac.05 <- ggplot(data = T05_UVW_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050034))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("Unpaid Volunteer Work (Over 12mos)", values =  c("turquoise", "mediumpurple1"), labels = c("Yes", "No"))

ggarrange(uvw.pie.ftl.05, uvw.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### Time Use - Type of Volunteer Work ============================================================================================

#### 
# A14. Type Volunteer ORG--FIRST MENTION: “Which types of organizations have you been involved with in your volunteer or community 
# service work in the last 12 months?--(FIRST MENTION) [PROBE: Anything else?]”
# Answers: 1 (Organizations for children and youth); 2 (Service organizations, such as Big Brothers-Big Sisters or Junior league); 
# 3 (Organized volunteer groups in hospitals or nursing homes); 4 (Religious groups, not including worship); 
# 5 (Conservation, recycling, or environmental groups, such as the Sierra Club or Nature Conservancy); 6 (Shelters, soup kitchens, 
# Habitat for Humanity, or other organizations helping families in need); 7 (Other); 8 (DK); 9 (NA/refused); 0 (Inap: no unpaid volunteer 
# or service work)
####

table(TIAS$TA050035)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050035 = 8)) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050035 = 8)) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050035 = 8)) 

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050035 = 8)) 

#### 
# A14 Type Volunteer ORG--SECOND MENTION: “Which types of organizations have you been involved with in your volunteer or community 
# service work in the last 12 months?--(SECOND MENTION) [PROBE: Anything else?]”
####

table(TIAS$TA050036)

#### 
# A14 Type Volunteer ORG--THIRD MENTION: “Which types of organizations have you been involved with in your volunteer or community 
# service work in the last 12 months?--(THIRD MENTION) [PROBE: Anything else?]”
####

table(TIAS$TA050037)

#### 
# A14 Type Volunteer ORG--FOURTH MENTION: “Which types of organizations have you been involved with in your volunteer or community 
# service work in the last 12 months?--(FOURTH MENTION) [PROBE: Anything else?]”
####

table(TIAS$TA050038)

#### 
# A14 Type Volunteer ORG--FIFTH MENTION: “Which types of organizations have you been involved with in your volunteer or community 
# service work in the last 12 months?--(FIFTH MENTION) [PROBE: Anything else?]”
####

table(TIAS$TA050039)

#### 
# A14 Type Volunteer ORG--SIXTH MENTION: “Which types of organizations have you been involved with in your volunteer or community 
# service work in the last 12 months?--(SIXTH MENTION) [PROBE: Anything else?]”
####

table(TIAS$TA050040)

####
# Creating A14 Heatmaps 
####

A14_FTL05 <- TIAS2005_FTL %>% select(TA050035, TA050036, TA050037, TA050038, TA050039, TA050040, ID)

cols <- sapply(A14_FTL05, is.logical)
A14_FTL05[,cols] <- lapply(A14_FTL05[,cols], as.numeric)

A14_FTL05_TIDY <- A14_FTL05 %>% tidyr::gather(variable, volunteer_type, 1:6)

A14_FTL05_TIDY$variable <- factor(A14_FTL05_TIDY$variable, levels = c("TA050035", "TA050036", "TA050037", "TA050038", "TA050039", "TA050040"))

A14_FTL05_IDLEV <- dplyr::pull(A14_FTL05, ID)
A14_FTL05_TIDY$ID <- factor(A14_FTL05_TIDY$ID, levels = A14_FTL05_IDLEV)

A14_FTL05_TIDY$volunteer_type <- factor(A14_FTL05_TIDY$volunteer_type)

ggplot(A14_FTL05_TIDY, aes(x=variable, y=ID, fill=volunteer_type)) + geom_tile(color="white", size=0.5) +
  coord_equal() +
  labs(title = "TIAS 2005 FTL", x="Order of Mention", y="ID") +
  theme_tufte(base_family="Helvetica") +
  theme(axis.ticks=element_blank()) + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1)) + 
  scale_fill_manual("Volunteer Type", values = c("slategray3", "coral1", "darkorange", "khaki1", "lightgreen", "mediumturquoise"), labels = c("Did Not Mention", "Youth Organization", "Hospital/Nursing Home", "Environmental Organization", "Shelters/Soup Kitchen/Habitat for Humanity", "Other")) 

A14_IAC05 <- TIAS2005_IAC %>% select(TA050035, TA050036, TA050037, TA050038, TA050039, TA050040, ID)

nrow(A14_IAC05)  

chunk <- 101  

n <- nrow(A14_IAC05)  

r <- rep(1:ceiling(n/chunk), each=chunk)[1:n]  

A14_IAC05_list <- split(A14_IAC05, r)   

length(A14_IAC05_list)  

tidy.vars <- function(x){
  x %>% tidyr::gather(variable, volunteer_type, 1:6)
}

A14_IAC05_tidylist <- lapply(A14_IAC05_list, tidy.vars)

for(i in 1:7) {
  A14_IAC05_tidylist[[i]]$variable <- factor(A14_IAC05_tidylist[[i]]$variable, levels = c("TA050035", "TA050036", "TA050037", "TA050038", "TA050039", "TA050040"))
}

set.ID.levels <- function(x){
  dplyr::pull(x, ID)
}

A14_IAC05_levlist <- lapply(A14_IAC05_list, set.ID.levels)

for(i in 1:7) {
  A14_IAC05_tidylist[[i]]$ID <- factor(A14_IAC05_tidylist[[i]]$ID, levels = A14_IAC05_levlist[[i]])
}

for(i in 1:7) {
  A14_IAC05_tidylist[[i]]$volunteer_type <- factor(A14_IAC05_tidylist[[i]]$volunteer_type)
}

create.A14.IAC05.heatmap <- function(x){
  ggplot(x, aes(x=variable, y=ID, fill=volunteer_type)) + geom_tile(color="white", size=0.5) +
    coord_equal() +
    labs(x="Order of Mention", y="TIAS 2005 IAC ID") +
    theme_tufte(base_family="Helvetica") +
    theme(axis.ticks=element_blank()) + 
    theme(axis.text.x=element_text(angle = 45, hjust = 1)) + 
    scale_fill_manual("Volunteer Type", values = c("slategray3", "coral1", "darkorange", "khaki1", "lightgreen", "mediumturquoise", "steelblue1", "mediumpurple1"), 
    labels = c("Did Not Mention", "Youth Organization", "Service Organization", "Hospital/Nursing Home", "Religious Organization", "Environmental Organization", "Shelters/Soup Kitchen/Habitat for Humanity", "Other")) 
}

A14.IAC05.heatmaps <- function(x){
  create.heatmap(A14_IAC05_tidylist[[x]])
}

# save or view in a horizontally wide frame to view all graphs properly

ggarrange(A14.IAC05.heatmaps(1) + rremove("legend"), A14.IAC05.heatmaps(2) + rremove("legend") + rremove("y.title"), A14.IAC05.heatmaps(3) + rremove("legend") + rremove("y.title"), A14.IAC05.heatmaps(4) + rremove("y.title"), ncol = 4, nrow = 1) 

ggarrange(A14.IAC05.heatmaps(5) + rremove("legend"), A14.IAC05.heatmaps(6) + rremove("legend") + rremove("y.title"), A14.IAC05.heatmaps(7) + rremove("y.title"), ncol = 3, nrow = 1) 

### Time Use - How Often Volunteered (Over 12 Mos) ===============================================================================

####
# A14B. How Often Volunteered: “During the last 12 months, about how often did you participate in volunteer or community service work? 
# Would you say: less than once a month, at least once a month, once a week, several times a week, almost every day, or every day?”
# Answers: 1 (Less than once a month); 2 (At least once a month); 3 (Once a week); 4 (Several times a week); 5 (Almost every day); 
# 6 (Every day); 8 (DK); 9 (NA/refused); 0 (Inap: no unpaid volunteer or community service work)
####

table(TIAS$TA050041)

T05_TUH_FTLW <- TIAS[, c("TA050041", "FTL_COUNT")] %>% group_by(TA050041, FTL_COUNT) %>% summarise(Count = n())

T05_TUH_FTLW <- T05_TUH_FTLW[1:19, ]

T05_TUH_CAT <- TIAS2005[, c("TA050041", "CAT")] %>% group_by(TA050041, CAT) %>% summarise(Count = n())

T05_TUH_FTLCAT <- TIAS2005_FTL[, c("TA050041", "CAT")] %>% group_by(TA050041, CAT) %>% summarise(Count = n())

T05_TUH_IACCAT <- TIAS2005_IAC[, c("TA050041", "CAT")] %>% group_by(TA050041, CAT) %>% summarise(Count = n())

head(T05_TUH_CAT, 11)

ggplot(T05_TUH_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050041))) + 
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("How Often Volunteered (Over 12mos)", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumpurple1"), 
                    labels = c("Did not volunteer", "Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day"))

head(T05_TUH_FTLW, 19)

ggplot(T05_TUH_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050041)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("How Often Volunteered (Over 12mos)", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumpurple1"), 
                    labels = c("Did not volunteer", "Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day"))

prop.table(table(TIAS2005_FTL$TA050041))

tuh.pie.ftl.05 <- ggplot(data = T05_TUH_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050041))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() + 
  scale_fill_manual("How Often Volunteered (Over 12mos)", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumpurple1"), 
                    labels = c("Did not volunteer", "Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day"))

prop.table(table(TIAS2005_IAC$TA050041))

tuh.pie.iac.05 <- ggplot(data = T05_TUH_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050041))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("How Often Volunteered (Over 12mos)", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumpurple1"), 
                    labels = c("Did not volunteer", "Less than once a month", "At least once a month", "Once a week", "Several times a week", "Almost every day", "Every day"))

ggarrange(tuh.pie.ftl.05, tuh.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### Self-Rating (Compared to Others) - Listening & Understanding Others ==========================================================

####
# C1J. How Well Listen Compared w/ Others: “Compared to other people, how good are you at listening to and understanding others? 
# Answers: 1-7 (1 = a lot worse than others <=> 7 = a lot better than others); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050060)

T05_SRU_FTLW <- TIAS[, c("TA050060", "FTL_COUNT")] %>% group_by(TA050060, FTL_COUNT) %>% summarise(Count = n())

T05_SRU_FTLW <- T05_SRU_FTLW[1:20, ]

T05_SRU_CAT <- TIAS2005[, c("TA050060", "CAT")] %>% group_by(TA050060, CAT) %>% summarise(Count = n())

ggplot(T05_SRU_FTLW, aes(x = FTL_COUNT, y = TA050060, group = FTL_COUNT, fill = as.factor(FTL_COUNT))) +
  geom_boxplot() + 
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "Self-Rating - Listening & Understanding Others (1 = a lot worse than others <=> 7 = a lot better than others)") + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  scale_y_continuous(breaks = seq(1, 7, by = 1)) + 
  guides(fill = guide_legend(title = "# of FTL Waves"))

ggplot(T05_SRU_CAT, aes(x = CAT, y = TA050060, group = CAT, fill = as.factor(CAT))) +
  geom_boxplot() +
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "Self-Rating - Listening & Understanding Others (1 = a lot worse than others <=> 7 = a lot better than others)") + 
  scale_y_continuous(breaks = seq(1, 7, by = 1)) + 
  guides(fill = guide_legend(title = "Category"))

### Self-Rating (Compared to Others) - Teaching & Explaining to Others ===========================================================

####
# C1K. How Good At Teaching Compared w/ Others: “Compared to other people, how good are you at teaching and explaining to others?”
# Answers: 1-7 (1 = a lot worse than others <=> 7 = a lot better than others); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050061)

T05_SRE_FTLW <- TIAS[, c("TA050061", "FTL_COUNT")] %>% group_by(TA050061, FTL_COUNT) %>% summarise(Count = n())

T05_SRE_FTLW <- T05_SRE_FTLW[1:22, ]

T05_SRE_CAT <- TIAS2005[, c("TA050061", "CAT")] %>% group_by(TA050061, CAT) %>% summarise(Count = n())

ggplot(T05_SRE_FTLW, aes(x = FTL_COUNT, y = TA050061, group = FTL_COUNT, fill = as.factor(FTL_COUNT))) +
  geom_boxplot() + 
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "Self-Rating - Teaching & Explaining to Others (1 = a lot worse than others <=> 7 = a lot better than others)") + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  scale_y_continuous(breaks = seq(1, 7, by = 1)) + 
  guides(fill = guide_legend(title = "# of FTL Waves"))

ggplot(T05_SRE_CAT, aes(x = CAT, y = TA050061, group = CAT, fill = as.factor(CAT))) +
  geom_boxplot() +
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "Self-Rating - Teaching & Explaining to Others (1 = a lot worse than others <=> 7 = a lot better than others)") + 
  scale_y_continuous(breaks = seq(1, 7, by = 1)) + 
  guides(fill = guide_legend(title = "Category"))

### Self-Rating (Compared to Others) - Supervising Others ========================================================================

####
# C1A. How Good At Supervising Comp: "Compared to other people, how good are you at supervising others?"
# Answers: 1-7 (1 = a lot worse than others <=> 7 = a lot better than others); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050052)

T05_SRS_FTLW <- TIAS[, c("TA050052", "FTL_COUNT")] %>% group_by(TA050052, FTL_COUNT) %>% summarise(Count = n())

T05_SRS_FTLW <- T05_SRS_FTLW[1:24, ]

T05_SRS_CAT <- TIAS2005[, c("TA050052", "CAT")] %>% group_by(TA050052, CAT) %>% summarise(Count = n())

ggplot(T05_SRS_FTLW, aes(x = FTL_COUNT, y = TA050052, group = FTL_COUNT, fill = as.factor(FTL_COUNT))) +
  geom_boxplot() + 
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "Self-Rating - Supervising Others (1 = a lot worse than others <=> 7 = a lot better than others)") + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  scale_y_continuous(breaks = seq(1, 7, by = 1)) + 
  guides(fill = guide_legend(title = "# of FTL Waves"))

ggplot(T05_SRS_CAT, aes(x = CAT, y = TA050052, group = CAT, fill = as.factor(CAT))) +
  geom_boxplot() +
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "Self-Rating - Supervising Others (1 = a lot worse than others <=> 7 = a lot better than others)") + 
  scale_y_continuous(breaks = seq(1, 7, by = 1)) + 
  guides(fill = guide_legend(title = "Category"))

### Self-Rating (Compared to Others) - Being a Leader ============================================================================

####
# C1B. How Good At Leading Comp w/ Others: “Compared to other people, how good are you at being a leader?”
# Answers: 1-7 (1 = a lot worse than others <=> 7 = a lot better than others); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050053)

T05_SRB_FTLW <- TIAS[, c("TA050053", "FTL_COUNT")] %>% group_by(TA050053, FTL_COUNT) %>% summarise(Count = n())

T05_SRB_FTLW <- T05_SRB_FTLW[1:23, ]

T05_SRB_CAT <- TIAS2005[, c("TA050053", "CAT")] %>% group_by(TA050053, CAT) %>% summarise(Count = n())

ggplot(T05_SRB_FTLW, aes(x = FTL_COUNT, y = TA050053, group = FTL_COUNT, fill = as.factor(FTL_COUNT))) +
  geom_boxplot() + 
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "Self-Rating - Being a Leader (1 = a lot worse than others <=> 7 = a lot better than others)") + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  scale_y_continuous(breaks = seq(1, 7, by = 1)) + 
  guides(fill = guide_legend(title = "# of FTL Waves"))

ggplot(T05_SRB_CAT, aes(x = CAT, y = TA050053, group = CAT, fill = as.factor(CAT))) +
  geom_boxplot() +
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "Self-Rating - Being a Leader (1 = a lot worse than others <=> 7 = a lot better than others)") + 
  scale_y_continuous(breaks = seq(1, 7, by = 1)) + 
  guides(fill = guide_legend(title = "Category"))

### Self-Rating (Compared to Others) - Logical/Analytic Thinking =================================================================

####
# C1C. How Good At Logic Comp w/ Others: “Compared to other people, how good are you at logical, analytical thinking?”
# Answers: 1-7 (1 = a lot worse than others <=> 7 = a lot better than others); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050054)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050054 = c(8, 9))) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050054 = c(8, 9))) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050054 = c(8, 9)))  

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050054 = c(8, 9))) 

T05_SRL_FTLW <- TIAS[, c("TA050054", "FTL_COUNT")] %>% group_by(TA050054, FTL_COUNT) %>% summarise(Count = n())

T05_SRL_FTLW <- T05_SRL_FTLW[1:24, ]

T05_SRL_CAT <- TIAS2005[, c("TA050054", "CAT")] %>% group_by(TA050054, CAT) %>% summarise(Count = n())

T05_SRL_CAT <- T05_SRL_CAT[1:14, ]

ggplot(T05_SRL_FTLW, aes(x = FTL_COUNT, y = TA050054, group = FTL_COUNT, fill = as.factor(FTL_COUNT))) +
  geom_boxplot() + 
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "Self-Rating - Logical/Analytic Thinking (1 = a lot worse than others <=> 7 = a lot better than others)") + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  scale_y_continuous(breaks = seq(1, 7, by = 1)) + 
  guides(fill = guide_legend(title = "# of FTL Waves"))

ggplot(T05_SRL_CAT, aes(x = CAT, y = TA050054, group = CAT, fill = as.factor(CAT))) +
  geom_boxplot() +
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "Self-Rating - Logical/Analytic Thinking (1 = a lot worse than others <=> 7 = a lot better than others)") + 
  scale_y_continuous(breaks = seq(1, 7, by = 1)) + 
  guides(fill = guide_legend(title = "Category"))

### Self-Rating (Compared to Others) - Helping Others Solve Their Problems =======================================================

####
# C1D. How Good At Helping Comp w/ Others: “Compared to other people, how good are you at helping others solve their problems?”
# Answers: 1-7 (1 = a lot worse than others <=> 7 = a lot better than others); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050055)

T05_SRH_FTLW <- TIAS[, c("TA050055", "FTL_COUNT")] %>% group_by(TA050055, FTL_COUNT) %>% summarise(Count = n())

T05_SRH_FTLW <- T05_SRH_FTLW[1:21, ]

T05_SRH_CAT <- TIAS2005[, c("TA050055", "CAT")] %>% group_by(TA050055, CAT) %>% summarise(Count = n())

ggplot(T05_SRH_FTLW, aes(x = FTL_COUNT, y = TA050055, group = FTL_COUNT, fill = as.factor(FTL_COUNT))) +
  geom_boxplot() + 
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "Self-Rating - Helping Others' Problem-Solving (1 = a lot worse than others <=> 7 = a lot better than others)") + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  scale_y_continuous(breaks = seq(1, 7, by = 1)) + 
  guides(fill = guide_legend(title = "# of FTL Waves"))

ggplot(T05_SRH_CAT, aes(x = CAT, y = TA050055, group = CAT, fill = as.factor(CAT))) +
  geom_boxplot() +
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "Self-Rating - Helping Others' Problem-Solving (1 = a lot worse than others <=> 7 = a lot better than others)") + 
  scale_y_continuous(breaks = seq(1, 7, by = 1)) + 
  guides(fill = guide_legend(title = "Category"))

### How Good at Problem Solving ==================================================================================================

####
# B6B. How Good At Problem Solving: “(On a scale of 1 to 7, where 1 means "Not At All Well" and 7 means "Extremely Well",) how good are you at solving problems you encounter?”
# Answers: 1-7 (1 = a lot worse than others <=> 7 = a lot better than others); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050049)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050049 = 8)) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050049 = 8)) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050049 = 8)) 

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050049 = 8)) 

T05_HGP_FTLW <- TIAS[, c("TA050049", "FTL_COUNT")] %>% group_by(TA050049, FTL_COUNT) %>% summarise(Count = n())

T05_HGP_FTLW <- T05_HGP_FTLW[1:20, ]

T05_HGP_CAT <- TIAS2005[, c("TA050049", "CAT")] %>% group_by(TA050049, CAT) %>% summarise(Count = n())

T05_HGP_CAT <- T05_HGP_CAT[1:12, ]

ggplot(T05_HGP_FTLW, aes(x = FTL_COUNT, y = TA050049, group = FTL_COUNT, fill = as.factor(FTL_COUNT))) +
  geom_boxplot() + 
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "Self-Rating - Problem-Solving (1 = a lot worse than others <=> 7 = a lot better than others)") + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  scale_y_continuous(breaks = seq(1, 7, by = 1)) + 
  guides(fill = guide_legend(title = "# of FTL Waves"))

ggplot(T05_HGP_CAT, aes(x = CAT, y = TA050049, group = CAT, fill = as.factor(CAT))) +
  geom_boxplot() +
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "Self-Rating - Problem-Solving (1 = a lot worse than others <=> 7 = a lot better than others)") + 
  scale_y_continuous(breaks = seq(1, 7, by = 1)) + 
  guides(fill = guide_legend(title = "Category"))

### Self-Rating (Compared to Others) - Intelligence ==============================================================================

####
# C1E How Intelligent Compared w/ Others: “Compared to other people, how would you rate your intelligence? (On a scale of 1 to 7, 
# where 1 means ‘A lot worse than other people’ and 7 means ‘A lot better than other people’)”
# Answers: 1-7 (1 = a lot worse than others <=> 7 = a lot better than others); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050056)

T05_SRT_FTLW <- TIAS[, c("TA050056", "FTL_COUNT")] %>% group_by(TA050056, FTL_COUNT) %>% summarise(Count = n())

T05_SRT_FTLW <- T05_SRT_FTLW[1:21, ]

T05_SRT_CAT <- TIAS2005[, c("TA050056", "CAT")] %>% group_by(TA050056, CAT) %>% summarise(Count = n())

ggplot(T05_SRT_FTLW, aes(x = FTL_COUNT, y = TA050056, group = FTL_COUNT, fill = as.factor(FTL_COUNT))) +
  geom_boxplot() + 
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "Self-Rating - Intelligence (1 = a lot worse than others <=> 7 = a lot better than others)") + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  scale_y_continuous(breaks = seq(1, 7, by = 1)) + 
  guides(fill = guide_legend(title = "# of FTL Waves"))

ggplot(T05_SRT_CAT, aes(x = CAT, y = TA050056, group = CAT, fill = as.factor(CAT))) +
  geom_boxplot() +
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "Self-Rating - Intelligence (1 = a lot worse than others <=> 7 = a lot better than others)") + 
  scale_y_continuous(breaks = seq(1, 7, by = 1)) + 
  guides(fill = guide_legend(title = "Category"))

### Self-Rating (Compared to Others) - Independence ==============================================================================

####
# C1F. How Independent Compared w/ Others: “Compared to other people, how would you rate your independence?
# Answers: 1-7 (1 = a lot worse than others <=> 7 = a lot better than others); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050057)

T05_SRI_FTLW <- TIAS[, c("TA050057", "FTL_COUNT")] %>% group_by(TA050057, FTL_COUNT) %>% summarise(Count = n())

T05_SRI_FTLW <- T05_SRI_FTLW[1:23, ]

T05_SRI_CAT <- TIAS2005[, c("TA050057", "CAT")] %>% group_by(TA050057, CAT) %>% summarise(Count = n())

ggplot(T05_SRI_FTLW, aes(x = FTL_COUNT, y = TA050057, group = FTL_COUNT, fill = as.factor(FTL_COUNT))) +
  geom_boxplot() + 
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "Self-Rating - Independence (1 = a lot worse than others <=> 7 = a lot better than others)") + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  scale_y_continuous(breaks = seq(1, 7, by = 1)) + 
  guides(fill = guide_legend(title = "# of FTL Waves"))

ggplot(T05_SRI_CAT, aes(x = CAT, y = TA050057, group = CAT, fill = as.factor(CAT))) +
  geom_boxplot() +
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "Self-Rating - Independence (1 = a lot worse than others <=> 7 = a lot better than others)") + 
  scale_y_continuous(breaks = seq(1, 7, by = 1)) + 
  guides(fill = guide_legend(title = "Category"))

### Self-Rating (Compared to Others) - Confidence ================================================================================

####
# C1G. How Confident Compared w/ Others: “Compared to other people, how would you rate your self-confidence?”
# Answers: 1-7 (1 = a lot worse than others <=> 7 = a lot better than others); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050058)

T05_SRC_FTLW <- TIAS[, c("TA050058", "FTL_COUNT")] %>% group_by(TA050058, FTL_COUNT) %>% summarise(Count = n())

T05_SRC_FTLW <- T05_SRC_FTLW[1:24, ]

T05_SRC_CAT <- TIAS2005[, c("TA050058", "CAT")] %>% group_by(TA050058, CAT) %>% summarise(Count = n())

ggplot(T05_SRC_FTLW, aes(x = FTL_COUNT, y = TA050058, group = FTL_COUNT, fill = as.factor(FTL_COUNT))) +
  geom_boxplot() + 
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "Self-Rating - Confidence (1 = a lot worse than others <=> 7 = a lot better than others)") + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  scale_y_continuous(breaks = seq(1, 7, by = 1)) + 
  guides(fill = guide_legend(title = "# of FTL Waves"))

ggplot(T05_SRC_CAT, aes(x = CAT, y = TA050058, group = CAT, fill = as.factor(CAT))) +
  geom_boxplot() +
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "Self-Rating - Confidence (1 = a lot worse than others <=> 7 = a lot better than others)") + 
  scale_y_continuous(breaks = seq(1, 7, by = 1)) + 
  guides(fill = guide_legend(title = "Category"))

### Self-Rating (Compared to Others) - Decisiveness ==============================================================================

####
# C1H. How Decisive Compared w/ Others: “Compared to other people, how would you rate your decisiveness?”
# Answers: 1-7 (1 = a lot worse than others <=> 7 = a lot better than others); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050059)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050059 = c(8, 9))) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050059 = c(8, 9))) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050059 = c(8, 9)))  

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050059 = c(8, 9))) 

T05_SRD_FTLW <- TIAS[, c("TA050059", "FTL_COUNT")] %>% group_by(TA050059, FTL_COUNT) %>% summarise(Count = n())

T05_SRD_FTLW <- T05_SRD_FTLW[1:23, ]

T05_SRD_CAT <- TIAS2005[, c("TA050059", "CAT")] %>% group_by(TA050059, CAT) %>% summarise(Count = n())

T05_SRD_CAT <- T05_SRD_CAT[1:14, ]

ggplot(T05_SRD_FTLW, aes(x = FTL_COUNT, y = TA050059, group = FTL_COUNT, fill = as.factor(FTL_COUNT))) +
  geom_boxplot() + 
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "Self-Rating - Decisiveness (1 = a lot worse than others <=> 7 = a lot better than others)") + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  scale_y_continuous(breaks = seq(1, 7, by = 1)) + 
  guides(fill = guide_legend(title = "# of FTL Waves"))

ggplot(T05_SRD_CAT, aes(x = CAT, y = TA050059, group = CAT, fill = as.factor(CAT))) +
  geom_boxplot() +
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "Self-Rating - Decisiveness (1 = a lot worse than others <=> 7 = a lot better than others)") + 
  scale_y_continuous(breaks = seq(1, 7, by = 1)) + 
  guides(fill = guide_legend(title = "Category"))

### Frequency - Snacking Instead of Consuming Regular Meals ======================================================================

####
# H28A. Freq of Snack Instead of Regular Meal: “How often do you snack instead of eating regular meals? Would you say: Never, hardly ever, 
# less than once a month, a couple of times a month, more than once a week, or every day?”
# Answers: 1 (Never); 2 (Hardly ever); 3 (Less than once a month); 4 (A couple of times a month); 5 (More than once a week); 6 (Every day); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050755)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050755 = 8)) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050755 = 8)) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050755 = 8)) 

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050755 = 8)) 

T05_FSI_FTLW <- TIAS[, c("TA050755", "FTL_COUNT")] %>% group_by(TA050755, FTL_COUNT) %>% summarise(Count = n())

T05_FSI_FTLW <- T05_FSI_FTLW[1:22, ]

T05_FSI_CAT <- TIAS2005[, c("TA050755", "CAT")] %>% group_by(TA050755, CAT) %>% summarise(Count = n())

T05_FSI_CAT <- T05_FSI_CAT[1:11, ]

T05_FSI_FTLCAT <- TIAS2005_FTL[, c("TA050755", "CAT")] %>% group_by(TA050755, CAT) %>% summarise(Count = n())

T05_FSI_IACCAT <- TIAS2005_IAC[, c("TA050755", "CAT")] %>% group_by(TA050755, CAT) %>% summarise(Count = n())

T05_FSI_IACCAT <- T05_FSI_IACCAT[1:6, ]

head(T05_FSI_CAT, 11)

ggplot(T05_FSI_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050755))) + 
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Snacking Instead of Regular Meals", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise"), 
                    labels = c("Never", "Hardly ever", "Less than once a month", "A couple of times a month", "More than once a week", "Every day"))

head(T05_FSI_FTLW, 22)

ggplot(T05_FSI_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050755)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Snacking Instead of Regular Meals", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise"), 
                    labels = c("Never", "Hardly ever", "Less than once a month", "A couple of times a month", "More than once a week", "Every day"))

prop.table(table(TIAS2005_FTL$TA050755))

fsi.pie.ftl.05 <- ggplot(data = T05_FSI_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050755))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() + 
  scale_fill_manual("Snacking Instead of Regular Meals", values =  c("lightcoral", "lightskyblue", "gold", "#0072B2", "turquoise"), 
                    labels = c("Never", "Hardly ever", "A couple of times a month", "More than once a week", "Every day"))

prop.table(table(TIAS2005_IAC$TA050755))

fsi.pie.iac.05 <- ggplot(data = T05_FSI_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050755))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("Snacking Instead of Regular Meals", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise"), 
                    labels = c("Never", "Hardly ever", "Less than once a month", "A couple of times a month", "More than once a week", "Every day"))

ggarrange(fsi.pie.ftl.05, fsi.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### Binge Eating Frequency =======================================================================================================

####
# H28B. Frequency of Binge Eating: “How often do you eat abnormally large amounts of food within a few hours, that is eat in binges?”
# Answers: 1 (Never); 2 (Hardly ever); 3 (Less than once a month); 4 (A couple of times a month); 5 (More than once a week); 6 (Every day); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050756)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050756 = 8)) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050756 = 8)) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050756 = 8)) 

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050756 = 8)) 

T05_BEF_FTLW <- TIAS[, c("TA050756", "FTL_COUNT")] %>% group_by(TA050756, FTL_COUNT) %>% summarise(Count = n())

T05_BEF_FTLW <- T05_BEF_FTLW[1:22, ]

T05_BEF_CAT <- TIAS2005[, c("TA050756", "CAT")] %>% group_by(TA050756, CAT) %>% summarise(Count = n())

T05_BEF_CAT <- T05_BEF_CAT[1:12, ]

T05_BEF_FTLCAT <- TIAS2005_FTL[, c("TA050756", "CAT")] %>% group_by(TA050756, CAT) %>% summarise(Count = n())

T05_BEF_IACCAT <- TIAS2005_IAC[, c("TA050756", "CAT")] %>% group_by(TA050756, CAT) %>% summarise(Count = n())

T05_BEF_IACCAT <- T05_BEF_IACCAT[1:6, ]

head(T05_BEF_CAT, 12)

ggplot(T05_BEF_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050756))) + 
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Binge Eating Frequency", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise"), 
                    labels = c("Never", "Hardly ever", "Less than once a month", "A couple of times a month", "More than once a week", "Every day"))

head(T05_BEF_FTLW, 22)

ggplot(T05_BEF_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050756)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Binge Eating Frequency", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise"), 
                    labels = c("Never", "Hardly ever", "Less than once a month", "A couple of times a month", "More than once a week", "Every day"))

prop.table(table(TIAS2005_FTL$TA050756))

bef.pie.ftl.05 <- ggplot(data = T05_BEF_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050756))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("Binge Eating Frequency", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise"), 
                    labels = c("Never", "Hardly ever", "Less than once a month", "A couple of times a month", "More than once a week", "Every day"))

prop.table(table(TIAS2005_IAC$TA050756))

bef.pie.iac.05 <- ggplot(data = T05_BEF_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050756))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("Binge Eating Frequency", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise"), 
                    labels = c("Never", "Hardly ever", "Less than once a month", "A couple of times a month", "More than once a week", "Every day"))

ggarrange(bef.pie.ftl.05, bef.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### Race - Hispanic ==============================================================================================================

####
# L6. Hispanicity: “In order to get an idea of the different races and ethnic groups that participate in the study, I would like to ask you 
# about your background. Are you Spanish, Hispanic, or Latino? That is, Mexican, Mexican American, Chicano, Puerto Rican, Cuban, or other Spanish?”
# Answers: 0 (Not Spanish, Hispanic, or Latino); 1 (Mexican); 2 (Mexican-American); 3 (Chicano); 4 (Puerto Rican); 5 (Cuban); 7 (Other Spanish); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050883)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050883 = c(8, 9)))

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050883 = c(8, 9))) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050883 = c(8, 9)))

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050883 = c(8, 9)))

T05_HIS_FTLW <- TIAS[, c("TA050883", "FTL_COUNT")] %>% group_by(TA050883, FTL_COUNT) %>% summarise(Count = n())

T05_HIS_FTLW <- T05_HIS_FTLW[1:18, ]

T05_HIS_CAT <- TIAS2005[, c("TA050883", "CAT")] %>% group_by(TA050883, CAT) %>% summarise(Count = n())

T05_HIS_CAT <- T05_HIS_CAT[1:11, ]

T05_HIS_FTLCAT <- TIAS2005_FTL[, c("TA050883", "CAT")] %>% group_by(TA050883, CAT) %>% summarise(Count = n())

T05_HIS_IACCAT <- TIAS2005_IAC[, c("TA050883", "CAT")] %>% group_by(TA050883, CAT) %>% summarise(Count = n())

T05_HIS_IACCAT <- T05_HIS_IACCAT[1:7, ]

head(T05_HIS_CAT, 11)

ggplot(T05_HIS_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050883))) + 
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Race - Hispanicity", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumpurple1"), 
                    labels = c("Not Spanish, Hispanic, or Latino", "Mexican", "Mexican-American", "Chicano", "Puerto Rican", "Cuban", "Other Spanish"))

head(T05_HIS_FTLW, 18)

ggplot(T05_HIS_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050883)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Race - Hispanicity", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumpurple1"), 
                    labels = c("Not Spanish, Hispanic, or Latino", "Mexican", "Mexican-American", "Chicano", "Puerto Rican", "Cuban", "Other Spanish"))

prop.table(table(TIAS2005_FTL$TA050883))

his.pie.ftl.05 <- ggplot(data = T05_HIS_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050883))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("Race - Hispanicity", values =  c("lightcoral", "lightskyblue", "#009E73", "#0072B2"), 
                    labels = c("Not Spanish, Hispanic, or Latino", "Mexican", "Mexican-American", "Puerto Rican"))

prop.table(table(TIAS2005_IAC$TA050883))

his.pie.iac.05 <- ggplot(data = T05_HIS_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050883))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("Race - Hispanicity", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumpurple1"), 
                    labels = c("Not Spanish, Hispanic, or Latino", "Mexican", "Mexican-American", "Chicano", "Puerto Rican", "Cuban", "Other Spanish"))

ggarrange(his.pie.ftl.05, his.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### Race Identification ========================================================================================================== 

####
# L7. Race Mention #1: “What is your race? Are you white, black, American Indian, Alaska Native, Asian, Native Hawaiian or Other Pacific Islander?--1ST MENTION”
# Answers: 1 (White); 2 (Black, African-American); 3 (American Indian or Alaska Native); 4 (Asian); 5 (Native Hawaiian or Pacific Islander);
# 7 (Some other race); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050884)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050884 = c(8, 9)))

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050884 = c(8, 9)))

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050884 = c(8, 9)))

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050884 = c(8, 9)))

T05_RAC_FTLW <- TIAS[, c("TA050884", "FTL_COUNT")] %>% group_by(TA050884, FTL_COUNT) %>% summarise(Count = n())

T05_RAC_FTLW <- T05_RAC_FTLW[1:17, ]

T05_RAC_CAT <- TIAS2005[, c("TA050884", "CAT")] %>% group_by(TA050884, CAT) %>% summarise(Count = n())

T05_RAC_CAT <- T05_RAC_CAT[1:11, ]

T05_RAC_FTLCAT <- TIAS2005_FTL[, c("TA050884", "CAT")] %>% group_by(TA050884, CAT) %>% summarise(Count = n())

T05_RAC_FTLCAT <- T05_RAC_FTLCAT[1:5, ]

T05_RAC_IACCAT <- TIAS2005_IAC[, c("TA050884", "CAT")] %>% group_by(TA050884, CAT) %>% summarise(Count = n())

T05_RAC_IACCAT <- T05_RAC_IACCAT[1:6, ]

head(T05_RAC_CAT, 11)

ggplot(T05_RAC_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050884))) + 
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2005", x = "Category", y = "Count") + 
  scale_fill_manual("Race", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise"), 
                    labels = c("White", "Black", "American Indian or Alaska Native", "Asian", "Native Hawaiian or Pacific Islander", "Other"))

head(T05_RAC_FTLW, 17)

ggplot(T05_RAC_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050884)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Race", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise"), 
                    labels = c("White", "Black", "American Indian or Alaska Native", "Asian", "Native Hawaiian or Pacific Islander", "Other"))

prop.table(table(TIAS2005_FTL$TA050884))

rac.pie.ftl.05 <- ggplot(data = T05_RAC_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050884))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("Race", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "turquoise"), 
                    labels = c("White", "Black", "American Indian or Alaska Native", "Asian", "Other"))

prop.table(table(TIAS2005_IAC$TA050884))

rac.pie.iac.05 <- ggplot(data = T05_RAC_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050884))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("Race", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise"), 
                    labels = c("White", "Black", "American Indian or Alaska Native", "Asian", "Native Hawaiian or Pacific Islander", "Other"))

ggarrange(rac.pie.ftl.05, rac.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### Daily Cigarette Usage ========================================================================================================

#### 
# H30. # Cigarettes Per Day: “On the average, how many cigarettes per day do you usually smoke?”
# Answers (One cigarette per day or fewer); 2-100 (Actual Number); 998 (DK)
####

table(TIAS$TA050759)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050759 = 998)) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050759 = 998)) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050759 = 998)) 

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050759 = 998)) 

T05_CIG_FTLW <- TIAS[, c("TA050759", "FTL_COUNT")] %>% group_by(TA050759, FTL_COUNT) %>% summarise(Count = n())

T05_CIG_FTLW <- T05_CIG_FTLW[1:37, ]

T05_CIG_CAT <- TIAS2005[, c("TA050759", "CAT")] %>% group_by(TA050759, CAT) %>% summarise(Count = n())

T05_CIG_CAT <- T05_CIG_CAT[1:27, ]

ggplot(T05_CIG_FTLW, aes(x = FTL_COUNT, y = TA050759, group = FTL_COUNT, fill = as.factor(FTL_COUNT))) +
  geom_boxplot() + 
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "Usual # of Cigarettes Per Day") + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  guides(fill = guide_legend(title = "# of FTL Waves"))

ggplot(T05_CIG_CAT, aes(x = CAT, y = TA050759, group = CAT, fill = as.factor(CAT))) +
  geom_boxplot() +
  labs(title = "TIAS 2005", x = "Category", y = "Usual # of Cigarettes Per Day") + 
  guides(fill = guide_legend(title = "Category"))

### Body Mass Index (BMI) ========================================================================================================

####
# Body Mass Index - Calculated as: [( Weight in Pounds) / (Height in inches) x (Height in inches)] x 703
# Answers: 15.0-59.9 (Actual Value); 99.0 (DK/NA/refused)
####

table(TIAS$TA050944)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050944 = 99)) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050944 = 99)) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050944 = 99)) 

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050944 = 99)) 

T05_BMI_FTLW <- TIAS[, c("TA050944", "FTL_COUNT")] %>% group_by(TA050944, FTL_COUNT) %>% summarise(Count = n())

T05_BMI_FTLW <- T05_BMI_FTLW[1:253, ]

T05_BMI_CAT <- TIAS2005[, c("TA050944", "CAT")] %>% group_by(TA050944, CAT) %>% summarise(Count = n())

T05_BMI_CAT <- T05_BMI_CAT[1:213, ]

ggplot(T05_BMI_FTLW, aes(x = FTL_COUNT, y = TA050944, group = FTL_COUNT, fill = as.factor(FTL_COUNT))) +
  geom_boxplot() + 
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "Body Mass Index (BMI)") + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  guides(fill = guide_legend(title = "# of FTL Waves"))

ggplot(T05_BMI_CAT, aes(x = CAT, y = TA050944, group = CAT, fill = as.factor(CAT))) +
  geom_boxplot() +
  labs(title = "TIAS 2005", x = "Category", y = "Body Mass Index (BMI)") + 
  guides(fill = guide_legend(title = "Category"))

### Usual Amount of Daily Sleep ==================================================================================================

####
# Number of Hours Sleep in 24-HR period: “How many hours do you usually sleep in a 24-hour period?”
# Answers: 1-24 (Actual Hours); 98 (DK); 99 (NA/refused)
#### 

table(TIAS$TA050754)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050754 = 98)) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050754 = 98)) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050754 = 98)) 

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050754 = 98)) 

T05_SLP_FTLW <- TIAS[, c("TA050754", "FTL_COUNT")] %>% group_by(TA050754, FTL_COUNT) %>% summarise(Count = n())

T05_SLP_FTLW <- T05_SLP_FTLW[1:36, ]

T05_SLP_CAT <- TIAS2005[, c("TA050754", "CAT")] %>% group_by(TA050754, CAT) %>% summarise(Count = n())

T05_SLP_CAT <- T05_SLP_CAT[1:22, ]

ggplot(T05_SLP_FTLW, aes(x = FTL_COUNT, y = TA050754, group = FTL_COUNT, fill = as.factor(FTL_COUNT))) +
  geom_boxplot() + 
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "Usual Hours of Sleep Per Night") + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  guides(fill = guide_legend(title = "# of FTL Waves"))

ggplot(T05_SLP_CAT, aes(x = CAT, y = TA050754, group = CAT, fill = as.factor(CAT))) +
  geom_boxplot() +
  labs(title = "TIAS 2005", x = "Category", y = "Usual Hours of Sleep Per Night") + 
  guides(fill = guide_legend(title = "Category"))

######################## TIAS-D Analysis - TIAS 2007 ######################## 

### Amphetamine Usage ===========================================================================================================

### Barbiturate Usage ===========================================================================================================

### Marijuana Usage =============================================================================================================

### Diet Pill Usage =============================================================================================================

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

### Diet Pill Usage =============================================================================================================

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

### Diet Pill Usage =============================================================================================================

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

####
# H44B. # of Times Took w/o Doc in Past 12mos: "On how many occasions (if any) have you taken/used amphetamine on your own
# w/o a doctor telling you to take them during the last 12 months?" 
# Answers: 1 (1-2); 2 (3-5); 3 (6-9); 4 (10-19); 5 (20-39); 6 (40 or more); 8 (DK); 9 (NA/refused); 0 (Inap: 0 in past 12 mos or never)
#### 

table(TIAS$TA130963)

T13_AMP_FTLW <- TIAS[, c("TA130963", "FTL_COUNT")] %>% group_by(TA130963, FTL_COUNT) %>% summarise(Count = n())

T13_AMP_FTLW <- T13_AMP_FTLW[1:11,]

T13_AMP_CAT <- TIAS2013[, c("TA130963", "CAT")] %>% group_by(TA130963, CAT) %>% summarise(Count = n())

head(T13_AMP_CAT, 7)

ggplot(T13_AMP_CAT, aes(x = CAT, y = Count, fill = as.factor(TA130963)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2013", x = "Category", y = "Count") + 
  scale_fill_manual("Amphetamine Usage (Prev. Year)", values = c("darkseagreen2", "darkslategray2", "lightgoldenrod1", "lightsalmon", "lightpink1", "mediumpurple1"), 
                    labels = c("Never", "1-2 times", "3-5 times", "6-9 times", "20-39 times", "40 or more times"))

head(T13_AMP_FTLW, 11)

ggplot(T13_AMP_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA130963)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2013", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Amphetamine Usage (Prev. Year)", values = c("darkseagreen2", "darkslategray2", "lightgoldenrod1", "lightsalmon", "lightpink1", "mediumpurple1"), 
                    labels = c("Never", "1-2 times", "3-5 times", "6-9 times", "20-39 times", "40 or more times"))

### Barbiturate Usage =========================================================================================================== 

####
# H44E. # of Times Took w/o Doc in Past 12mos: "On how many occasions (if any) have you taken/used barbiturates on your own
# w/o a doctor telling you to take them during the last 12 months?" 
# Answers: 1 (1-2); 2 (3-5); 3 (6-9); 4 (10-19); 5 (20-39); 6 (40 or more); 8 (DK); 9 (NA/refused); 0 (Inap: 0 in past 12 mos or never)
#### 

table(TIAS$TA130984)

T13_BRB_FTLW <- TIAS[, c("TA130984", "FTL_COUNT")] %>% group_by(TA130984, FTL_COUNT) %>% summarise(Count = n())

T13_BRB_FTLW <- T13_BRB_FTLW[1:9, ]

T13_BRB_CAT <- TIAS2013[, c("TA130984", "CAT")] %>% group_by(TA130984, CAT) %>% summarise(Count = n())

head(T13_BRB_CAT, 5)

ggplot(T13_BRB_CAT, aes(x = CAT, y = Count, fill = as.factor(TA130984)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2013", x = "Category", y = "Count") + 
  scale_fill_manual("Barbiturate Usage (Prev. Year)", values = c("darkseagreen2", "darkslategray2", "lightgoldenrod1", "lightpink1"), 
                    labels = c("Never", "3-5 times", "10-19 times", "40 or more times"))

head(T13_BRB_FTLW, 9)

ggplot(T13_BRB_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA130984)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2013", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Barbiturate Usage (Prev. Year)", values = c("darkseagreen2", "darkslategray2", "lightgoldenrod1", "lightpink1"), 
                    labels = c("Never", "3-5 times", "10-19 times", "40 or more times"))

### Marijuana Usage ============================================================================================================= 

####
# H44C. # of Times Took w/o Doc in Past 12mos: "On how many occasions (if any) have you taken/used marijuana on your own
# w/o a doctor telling you to take them during the last 12 months?" 
# Answers: 1 (1-2); 2 (3-5); 3 (6-9); 4 (10-19); 5 (20-39); 6 (40 or more); 8 (DK); 9 (NA/refused); 0 (Inap: 0 in past 12 mos or never)
#### 

table(TIAS$TA130971)

T13_MAR_FTLW <- TIAS[, c("TA130971", "FTL_COUNT")] %>% group_by(TA130971, FTL_COUNT) %>% summarise(Count = n())

T13_MAR_FTLW <- T13_MAR_FTLW[1:12, ]

T13_MAR_CAT <- TIAS2013[, c("TA130971", "CAT")] %>% group_by(TA130971, CAT) %>% summarise(Count = n())

head(T13_MAR_CAT, 6)

ggplot(T13_MAR_CAT, aes(x = CAT, y = Count, fill = as.factor(TA130971)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2013", x = "Category", y = "Count") + 
  scale_fill_manual("Marijuana Usage (Prev. Year)", values = c("darkseagreen2", "darkslategray2", "lightpink1", "maroon2", "orangered"), labels = c("Never", "40 or more times"))

head(T13_MAR_FTLW, 12)

ggplot(T13_MAR_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA130971)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2013", x = "# of FTL Waves", y = "Count") +
  scale_fill_manual("Marijuana Usage (Prev. Year)", values = c("darkseagreen2", "darkslategray2", "lightpink1", "maroon2", "orangered", "mediumorchid", "rosybrown1"), labels = c("Never", "40 or more times"))

### Diet Pill Usage ============================================================================================================= 

####
# H44A. # of Times Took w/o Doc in Past 12mos: "On how many occasions (if any) have you taken/used diet pills on your own
# w/o a doctor telling you to take them during the last 12 months?" 
# Answers: 1 (1-2); 2 (3-5); 3 (6-9); 4 (10-19); 5 (20-39); 6 (40 or more); 8 (DK); 9 (NA/refused); 0 (Inap: 0 in past 12 mos or never)
#### 

table(TIAS$TA130955)

T13_DIP_FTLW <- TIAS[, c("TA130955", "FTL_COUNT")] %>% group_by(TA130955, FTL_COUNT) %>% summarise(Count = n())

T13_DIP_FTLW <- T13_DIP_FTLW[1:9, ]

T13_DIP_CAT <- TIAS2013[, c("TA130955", "CAT")] %>% group_by(TA130955, CAT) %>% summarise(Count = n())

head(T13_DIP_CAT, 2)

ggplot(T13_DIP_CAT, aes(x = CAT, y = Count, fill = as.factor(TA130955)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2013", x = "Category", y = "Count") + 
  scale_fill_manual("Diet Pill Usage (Prev. Year)", values = c("darkseagreen2", "darkslategray2"), labels = c("Never", "1-2 times")) 

head(T13_DIP_FTLW, 9)

ggplot(T13_DIP_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA130955)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2013", x = "# of FTL Waves", y = "Count") +
  scale_fill_manual("Diet Pill Usage (Prev. Year)", values = c("darkseagreen2", "darkslategray2", "mediumorchid", "rosybrown1"), labels = c("Never", "1-2 times")) 
  
### Steroid Usage =============================================================================================================== 

####
# H44G. # of Times Took w/o Doc in Past 12mos: "On how many occasions (if any) have you taken/used steroids on your own
# w/o a doctor telling you to take them during the last 12 months?" 
# Answers: 1 (1-2); 2 (3-5); 3 (6-9); 4 (10-19); 5 (20-39); 6 (40 or more); 8 (DK); 9 (NA/refused); 0 (Inap: 0 in past 12 mos or never)
#### 

table(TIAS$TA131000)

T13_STR_FTLW <- TIAS[, c("TA131000", "FTL_COUNT")] %>% group_by(TA131000, FTL_COUNT) %>% summarise(Count = n())

T13_STR_FTLW <- T13_STR_FTLW[1:10, ]

T13_STR_CAT <- TIAS2013[, c("TA131000", "CAT")] %>% group_by(TA131000, CAT) %>% summarise(Count = n())

head(T13_STR_CAT, 4)

ggplot(T13_STR_CAT, aes(x = CAT, y = Count, fill = as.factor(TA131000)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2013", x = "Category", y = "Count") + 
  scale_fill_manual("Steroid Usage (Prev. Year)", values = c("darkseagreen2", "darkslategray2", "rosybrown1"), labels = c("Never", "20-39 times")) 

head(T13_STR_FTLW, 10)

ggplot(T13_STR_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA131000)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  scale_fill_manual("Steroid Usage (Prev. Year)", values = c("darkseagreen2", "darkslategray2", "orangered", "mediumorchid", "rosybrown1"), labels = c("Never", "20-39 times")) +
  labs(title = "TIAS 2013", x = "# of FTL Waves", y = "Count") 

### Tranquilizer Usage ========================================================================================================== 

####
# H44F. # of Times Took w/o Doc in Past 12mos: "On how many occasions (if any) have you taken/used tranquilizers on your own
# w/o a doctor telling you to take them during the last 12 months?" 
# Answers: 1 (1-2); 2 (3-5); 3 (6-9); 4 (10-19); 5 (20-39); 6 (40 or more); 8 (DK); 9 (NA/refused); 0 (Inap: 0 in past 12 mos or never)
#### 

table(TIAS$TA130992)

table(TIAS$TA130992, TIAS$FTL_COUNT)

T13_TRQ_FTLW <- TIAS[, c("TA130992", "FTL_COUNT")] %>% group_by(TA130992, FTL_COUNT) %>% summarise(Count = n())

T13_TRQ_FTLW <- T13_TRQ_FTLW[1:11, ]

T13_TRQ_CAT <- TIAS2013[, c("TA130992", "CAT")] %>% group_by(TA130992, CAT) %>% summarise(Count = n())

T13_TRQ_FTLCAT <- TIAS2013_FTL[, c("TA130992", "CAT")] %>% group_by(TA130992, CAT) %>% summarise(Count = n())

T13_TRQ_IACCAT <- TIAS2013_IAC[, c("TA130992", "CAT")] %>% group_by(TA130992, CAT) %>% summarise(Count = n())

head(T13_TRQ_CAT, 3)

ggplot(T13_TRQ_CAT, aes(x = CAT, y = Count, fill = as.factor(TA130992)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2013", x = "Category", y = "Count") +
  scale_fill_manual("Tranquilizer Usage (Prev. Year)", values = c("darkseagreen2", "darkslategray2", "lightgoldenrod1", "lightsalmon", "lightpink1", "rosybrown1"), 
                    labels = c("Never", "1-2 times", "3-5 times", "6-9 times", "10-19 times"))

head(T13_TRQ_FTLW, 11)

ggplot(T13_TRQ_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA130992)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2013", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Tranquilizer Usage (Prev. Year)", values = c("darkseagreen2", "darkslategray2", "lightgoldenrod1", "lightsalmon", "lightpink1", "rosybrown1"), 
                    labels = c("Never", "1-2 times", "3-5 times", "6-9 times", "10-19 times"))

prop.table(table(TIAS2013_FTL$TA130992))

trq.pie.ftl.13 <- ggplot(data = T13_TRQ_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA130992))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0)  + 
  scale_fill_manual("Tranquilizer Usage (Prev. Year)", values = c("darkseagreen2", "lightgoldenrod1"), labels = c("Never", "3-5 times")) + 
  theme_void() 

prop.table(table(TIAS2013_IAC$TA130992))

trq.pie.iac.13 <- ggplot(data = T13_TRQ_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA130992))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("Tranquilizer Usage (Prev. Year)", values = c("darkseagreen2", "darkslategray2", "lightgoldenrod1", "lightsalmon", "lightpink1", "rosybrown1"), 
                    labels = c("Never", "1-2 times", "3-5 times", "6-9 times", "10-19 times"))                    

ggarrange(trq.pie.ftl.13, trq.pie.iac.13, ncol = 2, nrow = 1, labels = c("FTL 2013", "IAC 2013"))

### Cocaine Usage =============================================================================================================== 

####
# H42D_B. # of Occasions in Past 12mos: "On how many occasions (if any) have you used cocaine in the past 12 months?"
# Answers: 1 (1-2); 2 (3-5); 3 (6-9); 4 (10-19); 5 (20-39); 6 (40 or more); 8 (DK); 9 (NA/refused); 0 (Inap: 0 in past 12 mos or never)
#### 

table(TIAS$TA130976)

T13_CCN_FTLW <- TIAS[, c("TA130976", "FTL_COUNT")] %>% group_by(TA130976, FTL_COUNT) %>% summarise(Count = n())

T13_CCN_FTLW <- T13_CCN_FTLW[1:12, ]

T13_CCN_CAT <- TIAS2013[, c("TA130976", "CAT")] %>% group_by(TA130976, CAT) %>% summarise(Count = n())

T13_CCN_FTLCAT <- TIAS2013_FTL[, c("TA130976", "CAT")] %>% group_by(TA130976, CAT) %>% summarise(Count = n())

T13_CCN_IACCAT <- TIAS2013_IAC[, c("TA130976", "CAT")] %>% group_by(TA130976, CAT) %>% summarise(Count = n())

table(TIAS2013$TA130976, TIAS2013$CAT)

head(T13_CCN_CAT, 8)

ggplot(T13_CCN_CAT, aes(x = CAT, y = Count, fill = as.factor(TA130976))) + 
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2013", x = "Category", y = "Count") + 
  scale_fill_manual("Cocaine Usage (Prev. Year)", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1", "hotpink"), 
                    labels = c("Never", "1-2 times", "3-5 times", "6-9 times", "10-19 times", "20-39 times", "40 or more times"))

head(T13_CCN_FTLW, 12)

ggplot(T13_CCN_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA130976)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2013", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Cocaine Usage (Prev. Year)", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1", "hotpink"), 
                    labels = c("Never", "1-2 times", "3-5 times", "6-9 times", "10-19 times", "20-39 times", "40 or more times"))

prop.table(table(TIAS2013_FTL$TA130976))

ccn.pie.ftl.13 <- ggplot(data = T13_CCN_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA130976))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_manual("Cocaine Usage (Prev. Year)", values = c("lightcoral", "gold"), labels = c("Never", "1-2 times")) +
  theme_void() 

prop.table(table(TIAS2013_IAC$TA130976))

ccn.pie.iac.13 <- ggplot(data = T13_CCN_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA130976))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_manual("Cocaine Usage (Prev. Year)", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1", "hotpink"), 
  labels = c("Never", "1-2 times", "3-5 times", "6-9 times", "10-19 times", "20-39 times", "40 or more times")) + 
  theme_void() 

ggarrange(ccn.pie.ftl.13, ccn.pie.iac.13, ncol = 2, nrow = 1, labels = c("FTL 2013", "IAC 2013"))      

### Average Alcohol Consumption Frequency Over Past Year ======================================================================== 

#### 
# H37. How Often Have Drinks-HD: “In the last year, on average, how often did you have any alcohol to drink? Would you say: 
# less than once a month, about once a month, several times a month, about once a week, several times a week, or every day?”
# Answers: 1 (Less than once a month); 2 (About once a month); 3 (Several times a month); 4 (About once a week); 5 (Several times a week); 
# 6 (Every day); 8 (DK); 9 (NA/refused); 0 (Inap: Does not drink alcohol)
####

table(TIAS$TA130946)

T13_AAC_FTLW <- TIAS[, c("TA130946", "FTL_COUNT")] %>% group_by(TA130946, FTL_COUNT) %>% summarise(Count = n())

T13_AAC_FTLW <- T13_AAC_FTLW[1:23, ]

T13_AAC_CAT <- TIAS2013[, c("TA130946", "CAT")] %>% group_by(TA130946, CAT) %>% summarise(Count = n())

T13_AAC_FTLCAT <- TIAS2013_FTL[, c("TA130946", "CAT")] %>% group_by(TA130946, CAT) %>% summarise(Count = n())

T13_AAC_IACCAT <- TIAS2013_IAC[, c("TA130946", "CAT")] %>% group_by(TA130946, CAT) %>% summarise(Count = n())

head(T13_AAC_CAT, 14)

ggplot(T13_AAC_CAT, aes(x = CAT, y = Count, fill = as.factor(TA130946))) + 
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2013", x = "Category", y = "Count") + 
  scale_fill_manual("Avg. Alcohol Consumption (Prev. Year)", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1", "hotpink", "rosybrown1"), 
                    labels = c("Never", "Less than once a month", "About once a month", "Several times a month", "About once a week", "Several times a week", "Every day"))

head(T13_AAC_FTLW, 23)

ggplot(T13_AAC_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA130946)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2013", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Avg. Alcohol Consumption (Prev. Year)", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1", "hotpink"), 
                    labels = c("Never", "Less than once a month", "About once a month", "Several times a month", "About once a week", "Several times a week", "Every day"))

prop.table(table(TIAS2013_FTL$TA130946))

aac.pie.ftl.13 <- ggplot(data = T13_AAC_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA130946))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Avg. Alcohol Consumption (Prev. Year)", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "rosybrown1"), 
  labels = c("Never", "Less than once a month", "About once a month", "Several times a month", "About once a week")) +
  theme_void() 

prop.table(table(TIAS2013_IAC$TA130946))

aac.pie.iac.13 <- ggplot(data = T13_AAC_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA130946))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_manual("Avg. Alcohol Consumption (Prev. Year)", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1", "hotpink", "rosybrown1"), 
  labels = c("Never", "Less than once a month", "About once a month", "Several times a month", "About once a week", "Several times a week", "Every day")) +
  theme_void() 

ggarrange(aac.pie.ftl.13, aac.pie.iac.13, ncol = 2, nrow = 1, labels = c("FTL 2013", "IAC 2013"))        

### Average Daily Alcohol Consumption =========================================================================================== 

#### 
# H38. # Alcoholic Drinks Per Day: "In the last year, on the days you drank, about how many drinks did you have?”
# Answers: 1 (One drink or fewer); 2-50 (Actual number of drinks); 98 (DK); 99 (NA/refused); 0 (Inap.: Does not drink alcohol or drinks alcohol but frequency of drinking is DK or NA)
####

table(TIAS$TA130947)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA130947 = 98)) 

TIAS2013 <- TIAS2013 %>% 
  replace_with_na(replace = list(TA130947 = 98)) 

TIAS2013_FTL <- TIAS2013_FTL %>% 
  replace_with_na(replace = list(TA130947 = 98)) 

TIAS2013_IAC <- TIAS2013_IAC %>% 
  replace_with_na(replace = list(TA130947 = 98))  

T13_DAC_FTLW <- TIAS[, c("TA130947", "FTL_COUNT")] %>% group_by(TA130947, FTL_COUNT) %>% summarise(Count = n())

T13_DAC_FTLW <- T13_DAC_FTLW[1:46, ]

T13_DAC_CAT <- TIAS2013[, c("TA130947", "CAT")] %>% group_by(TA130947, CAT) %>% summarise(Count = n())

T13_DAC_CAT <- T13_DAC_CAT[1:30, ]

T13_DAC_FTLCAT <- TIAS2013_FTL[, c("TA130947", "CAT")] %>% group_by(TA130947, CAT) %>% summarise(Count = n())

T13_DAC_FTLCAT <- T13_DAC_FTLCAT[1:11, ]

T13_DAC_IACCAT <- TIAS2013_IAC[, c("TA130947", "CAT")] %>% group_by(TA130947, CAT) %>% summarise(Count = n())

T13_DAC_IACCAT <- T13_DAC_IACCAT[1:19, ]

head(T13_DAC_CAT, 30)

ggplot(T13_DAC_CAT, aes(x = CAT, y = Count, fill = as.factor(TA130947))) + 
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2013", x = "Category", y = "Count") + 
  scale_fill_manual("Avg. Daily Alcohol Consumption", values =  c("#E69F00", "#56B4E9", "#009E73", "gold", "#0072B2", "#D55E00", "#CC79A7", "lightcoral", "wheat", "green3", "mediumturquoise", 
  "deepskyblue", "mediumpurple1", "hotpink", "khaki1", "palegreen1", "paleturquoise1", "peachpuff", "salmon", "plum", "thistle", "slateblue1"), labels = c("Never", "1 or fewer drinks", "2 drinks", "3 drinks", "4 drinks", "5 drinks", "6 drinks", "7 drinks", "8 drinks", 
  "9 drinks", "10 drinks", "11 drinks", "12 drinks", "15 drinks", "20 drinks"))

head(T13_DAC_FTLW, 46)

ggplot(T13_DAC_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA130947)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2013", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Avg. Daily Alcohol Consumption", values =  c("#E69F00", "#56B4E9", "#009E73", "gold", "#0072B2", "#D55E00", "#CC79A7", "lightcoral", "wheat", "green3", "mediumturquoise", 
  "deepskyblue", "mediumpurple1", "hotpink", "khaki1", "paleturquoise1", "peachpuff", "salmon", "plum", "thistle", "slateblue1"), labels = c("Never", "1 or fewer drinks", "2 drinks", "3 drinks", "4 drinks", "5 drinks", "6 drinks", "7 drinks", "8 drinks", 
  "9 drinks", "10 drinks", "11 drinks", "12 drinks", "15 drinks", "20 drinks"))

prop.table(table(TIAS2013_FTL$TA130947))

dac.pie.ftl.13 <- ggplot(data = T13_DAC_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA130947))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Avg. Daily Alcohol Consumption", values =  c("#E69F00", "#56B4E9", "#009E73", "gold", "#D55E00", "paleturquoise1", "peachpuff", "salmon", "plum", "thistle", "slateblue1"), labels = c("Never", "1 or fewer drinks", "2 drinks", "3 drinks", "5 drinks")) +
  theme_void() 

prop.table(table(TIAS2013_IAC$TA130947))

dac.pie.iac.13 <- ggplot(data = T13_DAC_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA130947))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_manual("Avg. Daily Alcohol Consumption", values =  c("#E69F00", "#56B4E9", "#009E73", "gold", "#0072B2", "#D55E00", "#CC79A7", "lightcoral", "wheat", "green3", "mediumturquoise", 
  "deepskyblue", "mediumpurple1", "hotpink", "khaki1", "paleturquoise1", "peachpuff", "salmon", "plum"), labels = c("Never", "1 or fewer drinks", "2 drinks", "3 drinks", "4 drinks", "5 drinks", "6 drinks", "7 drinks", "8 drinks", 
  "9 drinks", "10 drinks", "11 drinks", "12 drinks", "15 drinks", "20 drinks")) +
  theme_void() 

ggarrange(dac.pie.ftl.13, dac.pie.iac.13, ncol = 2, nrow = 1, labels = c("FTL 2013", "IAC 2013"))          

### Degree to Which Condition Limits Normal Daily Activities ==================================================================== 

####
# H13B. How much limits normal activities: "How much does this condition limit your normal daily activities? Would you say: A lot, somewhat, just a little, or not at all?”
# Answers: 1 (A lot); 3 (Somewhat); 5 (Just a little); 7 (Not at all); 8 (DK); 9 (NA; refused); 0 (Inap: Never diagnosed with any serious chronic condition)
####

table(TIAS$TA130866)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA130866 = 9)) 

TIAS2013 <- TIAS2013 %>% 
  replace_with_na(replace = list(TA130866 = 9)) 

TIAS2013_FTL <- TIAS2013_FTL %>% 
  replace_with_na(replace = list(TA130866 = 9)) 

TIAS2013_IAC <- TIAS2013_IAC %>% 
  replace_with_na(replace = list(TA130866 = 9)) 

T13_DCN_FTLW <- TIAS[, c("TA130866", "FTL_COUNT")] %>% group_by(TA130866, FTL_COUNT) %>% summarise(Count = n())

T13_DCN_FTLW <- T13_DCN_FTLW[1:16, ]

T13_DCN_CAT <- TIAS2013[, c("TA130866", "CAT")] %>% group_by(TA130866, CAT) %>% summarise(Count = n())

T13_DCN_CAT <- T13_DCN_CAT[1:7, ]

T13_DCN_FTLCAT <- TIAS2013_FTL[, c("TA130866", "CAT")] %>% group_by(TA130866, CAT) %>% summarise(Count = n())

T13_DCN_IACCAT <- TIAS2013_IAC[, c("TA130866", "CAT")] %>% group_by(TA130866, CAT) %>% summarise(Count = n())

T13_DCN_IACCAT <- T13_DCN_IACCAT[1:5, ]

head(T13_DCN_CAT, 7)

ggplot(T13_DCN_CAT, aes(x = CAT, y = Count, fill = as.factor(TA130866)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2013", x = "Category", y = "Count") + 
  scale_fill_manual("Condition Limits Daily Activities", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue"), 
                    labels = c("Inap: No chronic condition", "A lot", "Somewhat", "Just a little", "Not at all"))

head(T13_DCN_FTLW, 16)

ggplot(T13_DCN_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA130866)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2013", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Condition Limits Daily Activities", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue"), 
                    labels = c("Inap: No chronic condition", "A lot", "Somewhat", "Just a little", "Not at all"))

prop.table(table(TIAS2013_FTL$TA130866))

dcn.pie.ftl.13 <- ggplot(data = T13_DCN_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA130866))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Condition Limits Daily Activities", values = c("lightcoral", "mediumturquoise", "deepskyblue"), 
  labels = c("Inap: No chronic condition", "Just a little", "Not at all")) +
  theme_void() 

prop.table(table(TIAS2013_IAC$TA130866))

dcn.pie.iac.13 <- ggplot(data = T13_DCN_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA130866))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Condition Limits Daily Activities", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue"), 
  labels = c("Inap: No chronic condition", "A lot", "Somewhat", "Just a little", "Not at all")) +
  theme_void() 

ggarrange(dcn.pie.ftl.13, dcn.pie.iac.13, ncol = 2, nrow = 1, labels = c("FTL 2013", "IAC 2013"))

### Depression Over Past Year =================================================================================================== 

####
# H15. WTR>2 Wks Depressed In Past 12mos: “Now I want to ask you about periods of feeling sad, empty, or depressed. 
# In the past 12 months, have you had two weeks or longer when nearly every day you felt sad, empty, or depressed for most of the day?”
# Answers: 1 (Yes); 5 (No); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA130877)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA130877 = 9)) 

TIAS2013 <- TIAS2013 %>% 
  replace_with_na(replace = list(TA130877 = 9)) 

TIAS2013_FTL <- TIAS2013_FTL %>% 
  replace_with_na(replace = list(TA130877 = 9)) 

TIAS2013_IAC <- TIAS2013_IAC %>% 
  replace_with_na(replace = list(TA130877 = 9)) 

T13_DEP_FTLW <- TIAS[, c("TA130877", "FTL_COUNT")] %>% group_by(TA130877, FTL_COUNT) %>% summarise(Count = n())

T13_DEP_FTLW <- T13_DEP_FTLW[1:11, ]

T13_DEP_CAT <- TIAS2013[, c("TA130877", "CAT")] %>% group_by(TA130877, CAT) %>% summarise(Count = n())

T13_DEP_CAT <- T13_DEP_CAT[1:5, ]

T13_DEP_FTLCAT <- TIAS2013_FTL[, c("TA130877", "CAT")] %>% group_by(TA130877, CAT) %>% summarise(Count = n())

T13_DEP_IACCAT <- TIAS2013_IAC[, c("TA130877", "CAT")] %>% group_by(TA130877, CAT) %>% summarise(Count = n())

T13_DEP_IACCAT <- T13_DEP_IACCAT[1:3, ]

head(T13_DEP_CAT, 5)

ggplot(T13_DEP_CAT, aes(x = CAT, y = Count, fill = as.factor(TA130877)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2013", x = "Category", y = "Count") +
  scale_fill_manual("Depressed >2 Weeks", values = c("aquamarine3", "cadetblue2", "rosybrown1"), labels = c("Yes", "No"))

head(T13_DEP_FTLW, 11)

ggplot(T13_DEP_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA130877)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2013", x = "# of FTL Waves", y = "Count") +
  scale_fill_manual("Depressed >2 Weeks", values = c("aquamarine3", "cadetblue2", "rosybrown1"), labels = c("Yes", "No"))

prop.table(table(TIAS2013_FTL$TA130877))

dep.pie.ftl.13 <- ggplot(data = T13_DEP_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA130877))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Depressed >2 Weeks", values = c("aquamarine3", "cadetblue2", "rosybrown1"), labels = c("Yes", "No")) + 
  theme_void() 

prop.table(table(TIAS2013_IAC$TA130877))

dep.pie.iac.13 <- ggplot(data = T13_DEP_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA130877))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Depressed >2 Weeks", values = c("aquamarine3", "cadetblue2", "rosybrown1"), labels = c("Yes", "No")) + 
  theme_void() 

ggarrange(dep.pie.ftl.13, dep.pie.iac.13, ncol = 2, nrow = 1, labels = c("FTL 2013", "IAC 2013"))

### Depression - Anhedonia ======================================================================================================= 

####
# H16. WTR>2 Wks No Interest in Life: “In the past 12 months, have you had two weeks or longer when you lost interest in most things 
# like work, hobbies, and other things you usually enjoyed?”
# Answers: 1 (Yes); 5 (No); 8 (DK); 9 (NA/refused)

table(TIAS$TA130878)

T13_ANH_FTLW <- TIAS[, c("TA130878", "FTL_COUNT")] %>% group_by(TA130878, FTL_COUNT) %>% summarise(Count = n())

T13_ANH_FTLW <- T13_ANH_FTLW[1:14,]

T13_ANH_CAT <- TIAS2013[, c("TA130878", "CAT")] %>% group_by(TA130878, CAT) %>% summarise(Count = n())

T13_ANH_FTLCAT <- TIAS2013_FTL[, c("TA130878", "CAT")] %>% group_by(TA130878, CAT) %>% summarise(Count = n())

T13_ANH_IACCAT <- TIAS2013_IAC[, c("TA130878", "CAT")] %>% group_by(TA130878, CAT) %>% summarise(Count = n())

table(TIAS2013$TA130878, TIAS2013$CAT)

ggplot(T13_ANH_CAT, aes(x = CAT, y = Count, fill = as.factor(TA130878)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2013", x = "Category", y = "Count") +
  scale_fill_manual("Anhedonia >2 Weeks", values = c("cadetblue1", "lightsalmon", "aliceblue"), labels = c("Yes", "No"))

prop.table(table(TIAS2013_FTL$TA130878))

anh.pie.ftl.13 <- ggplot(data = T13_ANH_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA130878))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Anhedonia >2 Weeks", values = c("cadetblue1", "lightsalmon", "aliceblue"), labels = c("Yes", "No")) + 
  theme_void() 

prop.table(table(TIAS2013_IAC$TA130878))

anh.pie.iac.13 <- ggplot(data = T13_ANH_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA130878))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Anhedonia >2 Weeks", values = c("cadetblue1", "lightsalmon", "aliceblue"), labels = c("Yes", "No")) + 
  theme_void() 

ggarrange(anh.pie.ftl.13, anh.pie.iac.13, ncol = 2, nrow = 1, labels = c("FTL 2013", "IAC 2013"))

### Depression Diagnosis ========================================================================================================= 

####
# H12B. WTR Depression: "What was the diagnosis? What is the emotional or psychiatric disorder?--DEPRESSION"
# Answers: 1 (Diagnosed); 8 (DK); 9 (NA/refused); 0 (Not Diagnosed)
####

table(TIAS$TA130850)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA130850 = c(8, 9))) 

TIAS2013 <- TIAS2013 %>% 
  replace_with_na(replace = list(TA130850 = c(8, 9))) 

TIAS2013_FTL <- TIAS2013_FTL %>% 
  replace_with_na(replace = list(TA130850 = c(8, 9)))  

TIAS2013_IAC <- TIAS2013_IAC %>% 
  replace_with_na(replace = list(TA130850 = c(8, 9))) 

T13_DPD_FTLW <- TIAS[, c("TA130850", "FTL_COUNT")] %>% group_by(TA130850, FTL_COUNT) %>% summarise(Count = n())

T13_DPD_FTLW <- T13_DPD_FTLW[1:8, ]

T13_DPD_CAT <- TIAS2013[, c("TA130850", "CAT")] %>% group_by(TA130850, CAT) %>% summarise(Count = n())

T13_DPD_CAT <- T13_DPD_CAT[1:3, ]

T13_DPD_FTLCAT <- TIAS2013_FTL[, c("TA130850", "CAT")] %>% group_by(TA130850, CAT) %>% summarise(Count = n())

T13_DPD_IACCAT <- TIAS2013_IAC[, c("TA130850", "CAT")] %>% group_by(TA130850, CAT) %>% summarise(Count = n())

T13_DPD_IACCAT <- T13_DPD_IACCAT[1:2, ]
  
head(T13_DPD_CAT, 3)

ggplot(T13_DPD_CAT, aes(x = CAT, y = Count, fill = as.factor(TA130850)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2013", x = "Category", y = "Count") +
  scale_fill_manual("Depression Diagnosis", values = c("cadetblue1", "lightsalmon"), labels = c("Not Diagnosed", "Diagnosed"))

head(T13_DPD_FTLW, 8)

ggplot(T13_DPD_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA130850)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2013", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Depression Diagnosis", values = c("cadetblue1", "lightsalmon"), labels = c("Not Diagnosed", "Diagnosed"))

### Bipolar Disorder Diagnosis =================================================================================================== 

####
# H12B. WTR Bipolar: "What was the diagnosis? What is the emotional or psychiatric disorder?--BIPOLAR DISORDER"
# Answers: 1 (Diagnosed); 8 (DK); 9 (NA/refused); 0 (Not Diagnosed)
####

table(TIAS$TA130851)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA130851 = c(8, 9))) 

TIAS2013 <- TIAS2013 %>% 
  replace_with_na(replace = list(TA130851 = c(8, 9))) 

TIAS2013_FTL <- TIAS2013_FTL %>% 
  replace_with_na(replace = list(TA130851 = c(8, 9)))  

TIAS2013_IAC <- TIAS2013_IAC %>% 
  replace_with_na(replace = list(TA130851 = c(8, 9))) 

T13_BIP_FTLW <- TIAS[, c("TA130851", "FTL_COUNT")] %>% group_by(TA130851, FTL_COUNT) %>% summarise(Count = n())

T13_BIP_FTLW <- T13_BIP_FTLW[1:8, ]

T13_BIP_CAT <- TIAS2013[, c("TA130851", "CAT")] %>% group_by(TA130851, CAT) %>% summarise(Count = n())

T13_BIP_CAT <- T13_BIP_CAT[1:3, ]

T13_BIP_FTLCAT <- TIAS2013_FTL[, c("TA130851", "CAT")] %>% group_by(TA130851, CAT) %>% summarise(Count = n())

T13_BIP_IACCAT <- TIAS2013_IAC[, c("TA130851", "CAT")] %>% group_by(TA130851, CAT) %>% summarise(Count = n())

T13_BIP_IACCAT <- T13_BIP_IACCAT[1:2, ]

head(T13_BIP_CAT, 3)

ggplot(T13_BIP_CAT, aes(x = CAT, y = Count, fill = as.factor(TA130851)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2013", x = "Category", y = "Count") +
  scale_fill_manual("Bipolar Disorder Diagnosis", values = c("cadetblue1", "lightsalmon"), labels = c("Not Diagnosed", "Diagnosed"))

head(T13_BIP_FTLW, 8)

ggplot(T13_BIP_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA130851)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2013", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Bipolar Disorder Diagnosis", values = c("cadetblue1", "lightsalmon"), labels = c("Not Diagnosed", "Diagnosed"))

### Phobia Diagnosis =============================================================================================================

####
# H12B. WTR Phobia: "What was the diagnosis? What is the emotional or psychiatric disorder?--PHOBIAS"
# Answers: 1 (Diagnosed); 8 (DK); 9 (NA/refused); 0 (Not Diagnosed)
####

table(TIAS$TA130854)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA130854 = c(8, 9))) 

TIAS2013 <- TIAS2013 %>% 
  replace_with_na(replace = list(TA130854 = c(8, 9))) 

TIAS2013_FTL <- TIAS2013_FTL %>% 
  replace_with_na(replace = list(TA130854 = c(8, 9)))  

TIAS2013_IAC <- TIAS2013_IAC %>% 
  replace_with_na(replace = list(TA130854 = c(8, 9))) 

T13_PHB_FTLW <- TIAS[, c("TA130854", "FTL_COUNT")] %>% group_by(TA130854, FTL_COUNT) %>% summarise(Count = n())

T13_PHB_FTLW <- T13_PHB_FTLW[1:6, ]

T13_PHB_CAT <- TIAS2013[, c("TA130854", "CAT")] %>% group_by(TA130854, CAT) %>% summarise(Count = n())

T13_PHB_CAT <- T13_PHB_CAT[1:2, ]

T13_PHB_FTLCAT <- TIAS2013_FTL[, c("TA130854", "CAT")] %>% group_by(TA130854, CAT) %>% summarise(Count = n())

T13_PHB_IACCAT <- TIAS2013_IAC[, c("TA130854", "CAT")] %>% group_by(TA130854, CAT) %>% summarise(Count = n())

T13_PHB_IACCAT <- T13_PHB_IACCAT[1:1, ]

head(T13_PHB_CAT, 2)

ggplot(T13_PHB_CAT, aes(x = CAT, y = Count, fill = as.factor(TA130854)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2013", x = "Category", y = "Count") +
  scale_fill_manual("Phobia Diagnosis", values = c("cadetblue1", "lightsalmon"), labels = c("Not Diagnosed", "Diagnosed"))

head(T13_PHB_FTLW, 6)

ggplot(T13_PHB_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA130854)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2013", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Phobia Diagnosis", values = c("cadetblue1", "lightsalmon"), labels = c("Not Diagnosed", "Diagnosed"))

### Anxiety Disorders Diagnosis ==================================================================================================

#### 
# H12B. What was the diagnosis? What is the emotional or psychiatric disorder? -- Anxiety 
# Answers: 1 (Diagnosed w/ Anxiety); 8 (DK); 9 (NA/refused); 
# 0 (Inap. Never Diagnosed w/ Anxiety or Never Diagnosed with Emotional/Nervous/Psychiatric Problems)
####

table(TIAS$TA130853)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA130853 = c(8, 9))) 

TIAS2013 <- TIAS2013 %>% 
  replace_with_na(replace = list(TA130853 = c(8, 9))) 

TIAS2013_FTL <- TIAS2013_FTL %>% 
  replace_with_na(replace = list(TA130853 = c(8, 9)))  

TIAS2013_IAC <- TIAS2013_IAC %>% 
  replace_with_na(replace = list(TA130853 = c(8, 9))) 

T13_ANX_FTLW <- TIAS[, c("TA130853", "FTL_COUNT")] %>% group_by(TA130853, FTL_COUNT) %>% summarise(Count = n())

T13_ANX_FTLW <- T13_ANX_FTLW[1:8, ]

T13_ANX_CAT <- TIAS2013[, c("TA130853", "CAT")] %>% group_by(TA130853, CAT) %>% summarise(Count = n())

T13_ANX_CAT <- T13_ANX_CAT[1:3, ]

T13_ANX_FTLCAT <- TIAS2013_FTL[, c("TA130853", "CAT")] %>% group_by(TA130853, CAT) %>% summarise(Count = n())

T13_ANX_IACCAT <- TIAS2013_IAC[, c("TA130853", "CAT")] %>% group_by(TA130853, CAT) %>% summarise(Count = n())

T13_ANX_IACCAT <- T13_ANX_IACCAT[1:8, ]

head(T13_ANX_CAT, 3)

ggplot(T13_ANX_CAT, aes(x = CAT, y = Count, fill = as.factor(TA130853)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2013", x = "Category", y = "Count") +
  scale_fill_manual("Anxiety Disorder Diagnosis", values = c("cadetblue1", "lightsalmon"), labels = c("Not Diagnosed", "Diagnosed"))

head(T13_ANX_FTLW, 8)

ggplot(T13_ANX_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA130853)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2013", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Anxiety Disorder Diagnosis", values = c("cadetblue1", "lightsalmon"), labels = c("Not Diagnosed", "Diagnosed"))

prop.table(table(TIAS2013_FTL$TA130853))

anx.pie.ftl.13 <- ggplot(data = T13_ANX_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA130853))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Anxiety Disorder Diagnosis", values = c("cadetblue1", "lightsalmon"), labels = c("Not Diagnosed", "Diagnosed")) + 
  theme_void() 

prop.table(table(TIAS2013_IAC$TA130853))

anx.pie.iac.13 <- ggplot(data = T13_ANX_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA130853))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Anxiety Disorder Diagnosis", values = c("cadetblue1", "lightsalmon"), labels = c("Not Diagnosed", "Diagnosed")) + 
  theme_void() 

ggarrange(anx.pie.ftl.13, anx.pie.iac.13, ncol = 2, nrow = 1, labels = c("FTL 2013", "IAC 2013"))

### OCD Diagnosis ================================================================================================================

#### 
# H12B. What was the diagnosis? What is the emotional or psychiatric disorder? -- Obsessive Compulsive Disorder 
# Answers: 1 (Diagnosed w/ OCD); 8 (DK); 9 (NA/refused); 
# 0 (Inap. Never Diagnosed w/ Anxiety or Never Diagnosed with Emotional/Nervous/Psychiatric Problems)
####

table(TIAS$TA130857)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA130857 = c(8, 9))) 

TIAS2013 <- TIAS2013 %>% 
  replace_with_na(replace = list(TA130857 = c(8, 9))) 

TIAS2013_FTL <- TIAS2013_FTL %>% 
  replace_with_na(replace = list(TA130857 = c(8, 9)))  

TIAS2013_IAC <- TIAS2013_IAC %>% 
  replace_with_na(replace = list(TA130857 = c(8, 9))) 

T13_OCD_FTLW <- TIAS[, c("TA130857", "FTL_COUNT")] %>% group_by(TA130857, FTL_COUNT) %>% summarise(Count = n())

T13_OCD_FTLW <- T13_OCD_FTLW[1:7, ]

T13_OCD_CAT <- TIAS2013[, c("TA130857", "CAT")] %>% group_by(TA130857, CAT) %>% summarise(Count = n())

T13_OCD_CAT <- T13_OCD_CAT[1:3, ]

T13_OCD_FTLCAT <- TIAS2013_FTL[, c("TA130857", "CAT")] %>% group_by(TA130857, CAT) %>% summarise(Count = n())

T13_OCD_IACCAT <- TIAS2013_IAC[, c("TA130857", "CAT")] %>% group_by(TA130857, CAT) %>% summarise(Count = n())

T13_OCD_IACCAT <- T13_OCD_IACCAT[1:2, ]

head(T13_OCD_CAT, 3)

ggplot(T13_OCD_CAT, aes(x = CAT, y = Count, fill = as.factor(TA130857)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2013", x = "Category", y = "Count") +
  scale_fill_manual("OCD Diagnosis", values = c("cadetblue1", "lightsalmon"), labels = c("Not Diagnosed", "Diagnosed"))

head(T13_OCD_FTLW, 7)

ggplot(T13_OCD_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA130857)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2013", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("OCD Diagnosis", values = c("cadetblue1", "lightsalmon"), labels = c("Not Diagnosed", "Diagnosed"))

### Mental Health: Non-Spec Psych Distress ======================================================================================= 

####
# Cumulative score from answers to NSPD scale questions: 
# (how often felt nervous/hopeless/restless/everything as an effort/sad/worthless in past mo.)
# Possible scores: 0-24; 99 (all items are DK/NA/refused)
####

table(TIAS$TA131217)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA131217 = 99)) 

TIAS2013 <- TIAS2013 %>% 
  replace_with_na(replace = list(TA131217 = 99)) 

TIAS2013_FTL <- TIAS2013_FTL %>% 
  replace_with_na(replace = list(TA131217 = 99)) 

TIAS2013_IAC <- TIAS2013_IAC %>% 
  replace_with_na(replace = list(TA131217 = 99)) 

T13_NPD_FTLW <- TIAS[, c("TA131217", "FTL_COUNT")] %>% group_by(TA131217, FTL_COUNT) %>% summarise(Count = n())

T13_NPD_FTLW <- T13_NPD_FTLW[1:71, ]

T13_NPD_CAT <- TIAS2013[, c("TA131217", "CAT")] %>% group_by(TA131217, CAT) %>% summarise(Count = n())

T13_NPD_CAT <- T13_NPD_CAT[1:40, ]

ggplot(T13_NPD_FTLW, aes(x = FTL_COUNT, y = TA131217, group = FTL_COUNT, fill = as.factor(FTL_COUNT))) +
  geom_boxplot() + 
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 1)) + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2013", x = "# of Waves for Which Participant Identified as FTL", y = "NSPD Scale Score") + 
  guides(fill = guide_legend(title = "# of FTL Waves"))

ggplot(T13_NPD_CAT, aes(x = CAT, y = TA131217, group = CAT, fill = as.factor(CAT))) +
  geom_boxplot() +
  labs(title = "TIAS 2013", x = "Category", y = "NSPD Scale Score") + 
  guides(fill = guide_legend(title = "Category"))

### Mental Health: Social Anxiety ================================================================================================ 

####
# Cumulative score from answers to SA scale questions: 
# (how often nervous meeting others/feel shy/feel self-conscious/feel nervous performing)
# Possible scores: 1-7; 9 (all items are DK/NA/refused)
####

table(TIAS$TA131212)

T13_SOA_FTLW <- TIAS[, c("TA131212", "FTL_COUNT")] %>% group_by(TA131212, FTL_COUNT) %>% summarise(Count = n())

T13_SOA_FTLW <- T13_SOA_FTLW[1:30, ]

T13_SOA_CAT <- TIAS2013[, c("TA131212", "CAT")] %>% group_by(TA131212, CAT) %>% summarise(Count = n())

ggplot(T13_SOA_FTLW, aes(x = FTL_COUNT, y = TA131212, group = FTL_COUNT, fill = as.factor(FTL_COUNT))) +
  geom_boxplot() +
  labs(title = "TIAS 2013", x = "# of Waves for Which Participant Identified as FTL", y = "SA Scale Score") + 
  scale_y_continuous(limits = c(1, 7), breaks = seq(1, 7, by = 1)) + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) +
  guides(fill = guide_legend(title = "# of FTL Waves"))

ggplot(T13_SOA_CAT, aes(x = CAT, y = TA131212, group = CAT, fill = as.factor(CAT))) +
  geom_boxplot() +
  labs(title = "TIAS 2013", x = "Category", y = "SA Scale Score") + 
  scale_y_continuous(limits = c(1, 7), breaks = seq(1, 7, by = 1)) +
  guides(fill = guide_legend(title = "Category"))

### Mental Health: Worry =========================================================================================================

####
# Cumulative score from answers to 'Mental Health: Worry' section questions:
# (how often worry about money/future job/feel discouraged about future)
# Possible scores: 1-7; 9 (all items are DK/NA/refused)
####

table(TIAS$TA131211)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA131211 = 9)) 

TIAS2013 <- TIAS2013 %>% 
  replace_with_na(replace = list(TA131211 = 9)) 

T13_WRY_FTLW <- TIAS[, c("TA131211", "FTL_COUNT")] %>% group_by(TA131211, FTL_COUNT) %>% summarise(Count = n())

T13_WRY_FTLW <- T13_WRY_FTLW[1:32, ]

T13_WRY_CAT <- TIAS2013[, c("TA131211", "CAT")] %>% group_by(TA131211, CAT) %>% summarise(Count = n())

T13_WRY_CAT <- T13_WRY_CAT[1:14, ]

ggplot(T13_WRY_FTLW, aes(x = FTL_COUNT, y = TA131211, group = FTL_COUNT, fill = as.factor(FTL_COUNT))) +
  geom_boxplot() +
  labs(title = "TIAS 2013", x = "# of Waves for Which Participant Identified as FTL", y = "MH-Worry Scale Score") + 
  scale_y_continuous(limits = c(1, 7), breaks = seq(1, 7, by = 1)) + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) +
  guides(fill = guide_legend(title = "# of FTL Waves"))

ggplot(T13_WRY_CAT, aes(x = CAT, y = TA131211, group = CAT, fill = as.factor(CAT))) +
  geom_boxplot() +
  labs(title = "TIAS 2013", x = "Category", y = "MH-Worry Scale Score") + 
  scale_y_continuous(limits = c(1, 7), breaks = seq(1, 7, by = 1)) +
  guides(fill = guide_legend(title = "Category"))

### Paid Employment Since Jan 1 of Prior Year ====================================================================================

#### 
# E3A. WTR Worked Since Jan 1 Of Prior Year: “Have you done any work for money since January 1, 2003? Please include any type of work, no matter how small”
# Answers: 1 (Yes); 5 (No); 8 (DK); 9 (NA/refused); 0 (Inap: Working Now)
####

table(TIAS$TA130140)

T13_EPJ_FTLW <- TIAS[, c("TA130140", "FTL_COUNT")] %>% group_by(TA130140, FTL_COUNT) %>% summarise(Count = n())

T13_EPJ_FTLW <- T13_EPJ_FTLW[1:19, ]

T13_EPJ_CAT <- TIAS2013[, c("TA130140", "CAT")] %>% group_by(TA130140, CAT) %>% summarise(Count = n())

T13_EPJ_FTLCAT <- TIAS2013_FTL[, c("TA130140", "CAT")] %>% group_by(TA130140, CAT) %>% summarise(Count = n())

T13_EPJ_IACCAT <- TIAS2013_IAC[, c("TA130140", "CAT")] %>% group_by(TA130140, CAT) %>% summarise(Count = n())

head(T13_EPJ_CAT, 9)

ggplot(T13_EPJ_CAT, aes(x = CAT, y = Count, fill = as.factor(TA130140)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2013", x = "Category", y = "Count") +
  scale_fill_manual("Paid Employment Since Prior Year", values = c("lightblue1", "lightgoldenrod1", "lightpink", "rosybrown1", "seagreen"), labels = c("Yes (Working Now)", "Yes (Not Now)", "No"))

head(T13_EPJ_FTLW, 19)

ggplot(T13_EPJ_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA130140)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2013", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Paid Employment Since Prior Year", values = c("lightblue1", "lightgoldenrod1", "lightpink", "rosybrown1", "seagreen"), labels = c("Yes (Working Now)", "Yes (Not Now)", "No"))

prop.table(table(TIAS2013_FTL$TA130140))

epj.pie.ftl.13 <- ggplot(data = T13_EPJ_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA130140))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Paid Employment Since Prior Year", values = c("lightblue1", "lightgoldenrod1", "rosybrown1", "sienna1", turquoise1"), labels = c("Yes (Working Now)", "Yes (Not Now)", "No")) +
  theme_void() 

prop.table(table(TIAS2013_IAC$TA130140))

epj.pie.iac.13 <- ggplot(data = T13_EPJ_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA130140))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Paid Employment Since Prior Year", values = c("lightblue1", "lightgoldenrod1", "rosybrown1", "turquoise1", "sienna1"), labels = c("Yes (Working Now)", "Yes (Not Now)", "No")) +
  theme_void() 

ggarrange(epj.pie.ftl.13, epj.pie.iac.13, ncol = 2, nrow = 1, labels = c("FTL 2013", "IAC 2013"))

### Paid Employment History ====================================================================================================== 

####
# E62. WTR Ever Worked: “Have you ever done any work for money?”
# Answers: 1 (Yes); 5 (No); 8 (DK); 9 (NA/refused); 0 (Inap: Has worked since 01/01/2003)
####

table(TIAS$TA130347)

T13_EPH_FTLW <- TIAS[, c("TA130347", "FTL_COUNT")] %>% group_by(TA130347, FTL_COUNT) %>% summarise(Count = n())

T13_EPH_FTLW <- T13_EPH_FTLW[1:18, ]

T13_EPH_CAT <- TIAS2013[, c("TA130347", "CAT")] %>% group_by(TA130347, CAT) %>% summarise(Count = n())

T13_EPH_FTLCAT <- TIAS2013_FTL[, c("TA130347", "CAT")] %>% group_by(TA130347, CAT) %>% summarise(Count = n())

T13_EPH_IACCAT <- TIAS2013_IAC[, c("TA130347", "CAT")] %>% group_by(TA130347, CAT) %>% summarise(Count = n())

head(T13_EPH_CAT, 7)

ggplot(T13_EPH_CAT, aes(x = CAT, y = Count, fill = as.factor(TA130347)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2013", x = "Category", y = "Count") +
  scale_fill_manual("Paid Employment History", values = c("darkturquoise", "darkviolet", "deeppink", "sienna1"), labels = c("Yes (Employed Since 2003 or Before)", "Yes (Not Now)", "No"))

head(T13_EPH_FTLW, 18)

ggplot(T13_EPH_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA130347)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2013", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Paid Employment History", values = c("darkturquoise", "darkviolet", "deeppink", "sienna1"), labels = c("Yes (Employed Since 2003 or Before)", "Yes (Not Now)", "No"))

prop.table(table(TIAS2013_FTL$TA130347))

eph.pie.ftl.13 <- ggplot(data = T13_EPH_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA130347))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Paid Employment History", values = c("darkturquoise", "darkviolet", "deeppink", "sienna1"), labels = c("Yes (Employed Since 2003 or Before)", "Yes (Not Now)", "No")) +
  theme_void() 

prop.table(table(TIAS2013_IAC$TA130347))

eph.pie.iac.13 <- ggplot(data = T13_EPH_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA130347))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Paid Employment History", values = c("darkturquoise", "darkviolet", "deeppink", "sienna1"), labels = c("Yes (Employed Since 2003 or Before)", "Yes (Not Now)", "No")) +
  theme_void() 

ggarrange(eph.pie.ftl.13, eph.pie.iac.13, ncol = 2, nrow = 1, labels = c("FTL 2013", "IAC 2013"))

### Current Marital Status =======================================================================================================

####
# D1 Current Marital Status: Are you married, have you never been married, or are you widowed, divorced, or separated?
# Answers: 1 (Married); 2 (Never Married); 3 (Widowed); 4 (Divorced); 5 (Separated); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA130078)

T13_CMS_FTLW <- TIAS[, c("TA130078", "FTL_COUNT")] %>% group_by(TA130078, FTL_COUNT) %>% summarise(Count = n())

T13_CMS_FTLW <- T13_CMS_FTLW[1:15, ]

T13_CMS_CAT <- TIAS2013[, c("TA130078", "CAT")] %>% group_by(TA130078, CAT) %>% summarise(Count = n())

T13_CMS_FTLCAT <- TIAS2013_FTL[, c("TA130078", "CAT")] %>% group_by(TA130078, CAT) %>% summarise(Count = n())

T13_CMS_IACCAT <- TIAS2013_IAC[, c("TA130078", "CAT")] %>% group_by(TA130078, CAT) %>% summarise(Count = n())

head(T13_CMS_CAT, 9)

ggplot(T13_CMS_CAT, aes(x = CAT, y = Count, fill = as.factor(TA130078)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2013", x = "Category", y = "Count") +
  scale_fill_manual("Current Marital Status", values = c("lightcoral", "khaki1", "mediumaquamarine", "lightskyblue", "sienna1"), labels = c("Married", "Never Married", "Divorced", "Separated"))

head(T13_CMS_FTLW, 15)

ggplot(T13_CMS_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA130078)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2013", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Current Marital Status", values = c("lightcoral", "khaki1", "mediumaquamarine", "lightskyblue", "sienna1"), labels = c("Married", "Never Married", "Divorced", "Separated"))

### Romantic Relationships ======================================================================================================= 

####
# D8. WTR Romantic Relationship Now: “Are you currently involved in a romantic relationship?”
# Answers: 1 (Yes); 5 (No); 8 (DK); 9 (NA/refused); 0 (Inap: Married)
####

table(TIAS$TA130087)

T13_RRN_FTLW <- TIAS[, c("TA130087", "FTL_COUNT")] %>% group_by(TA130087, FTL_COUNT) %>% summarise(Count = n())

T13_RRN_FTLW <- T13_RRN_FTLW[1:17, ]

T13_RRN_CAT <- TIAS2013[, c("TA130087", "CAT")] %>% group_by(TA130087, CAT) %>% summarise(Count = n())

T13_RRN_FTLCAT <- TIAS2013_FTL[, c("TA130087", "CAT")] %>% group_by(TA130087, CAT) %>% summarise(Count = n())

T13_RRN_IACCAT <- TIAS2013_IAC[, c("TA130087", "CAT")] %>% group_by(TA130087, CAT) %>% summarise(Count = n())

head(T13_RRN_CAT, 9)

ggplot(T13_RRN_CAT, aes(x = CAT, y = Count, fill = as.factor(TA130087)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2013", x = "Category", y = "Count") +
  scale_fill_manual("Involved in Romantic Rel.", values = c("darkturquoise", "darkviolet", "deeppink", "sienna1"), labels = c("Inap: Married", "Yes", "No"))

head(T13_RRN_FTLW, 17)

ggplot(T13_RRN_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA130087)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2013", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Involved in Romantic Rel.", values = c("darkturquoise", "darkviolet", "deeppink", "sienna1"), labels = c("Inap: Married", "Yes", "No"))

prop.table(table(TIAS2013_FTL$TA130087))

rrn.pie.ftl.13 <- ggplot(data = T13_RRN_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA130087))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Involved in Romantic Rel.", values = c("darkturquoise", "darkviolet", "deeppink", "sienna1"), labels = c("Inap: Married", "Yes", "No")) +
  theme_void() 

prop.table(table(TIAS2013_IAC$TA130087))

rrn.pie.iac.13 <- ggplot(data = T13_RRN_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA130087))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Involved in Romantic Rel.", values = c("darkturquoise", "darkviolet", "deeppink", "sienna1"), labels = c("Inap: Married", "Yes", "No")) +
  theme_void() 

ggarrange(rrn.pie.ftl.13, rrn.pie.iac.13, ncol = 2, nrow = 1, labels = c("FTL 2013", "IAC 2013"))

### Number of Children ===========================================================================================================

####
# D28A. Number of Children: “How many (biological,) adopted, or step- children do you have?”
# Answers: 0-20 (# of Children); 98 (DK); 99 (NA/refused)
####

table(TIAS$TA050091)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050091 = 99)) 

TIAS2013 <- TIAS2013 %>% 
  replace_with_na(replace = list(TA050091 = 99)) 

TIAS2013_FTL <- TIAS2013_FTL %>% 
  replace_with_na(replace = list(TA050091 = 99)) 

TIAS2013_IAC <- TIAS2013_IAC %>% 
  replace_with_na(replace = list(TA050091 = 99)) 

T13_CHL_FTLW <- TIAS[, c("TA050091", "FTL_COUNT")] %>% group_by(TA050091, FTL_COUNT) %>% summarise(Count = n())

T13_CHL_FTLW <- T13_CHL_FTLW[1:14, ]

T13_CHL_CAT <- TIAS2013[, c("TA050091", "CAT")] %>% group_by(TA050091, CAT) %>% summarise(Count = n())

T13_CHL_CAT <- T13_CHL_CAT[1:8, ]

ggplot(T13_CHL_FTLW, aes(x = FTL_COUNT, y = TA050091, group = FTL_COUNT, fill = as.factor(FTL_COUNT))) +
  geom_boxplot() + 
  labs(title = "TIAS 2013", x = "# of Waves for Which Participant Identified as FTL", y = "Number of Children") + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  guides(fill = guide_legend(title = "# of FTL Waves"))

ggplot(T13_CHL_CAT, aes(x = CAT, y = TA050091, group = CAT, fill = as.factor(CAT))) +
  geom_boxplot() +
  labs(title = "TIAS 2013", x = "Category", y = "Number of Children") + 
  guides(fill = guide_legend(title = "Category"))

### High School Education ======================================================================================================== 

####
# G1. WTR Graduated High School: “Now I would like to talk about the education you have received. Did you graduate from high school, get a GED, or neither?”
# Answers: 1 (Graduated from high school); 2 (Got a GED); 3 (Neither); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050573)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050573 = 9)) 

TIAS2013 <- TIAS2013 %>% 
  replace_with_na(replace = list(TA050573 = 9)) 

TIAS2013_FTL <- TIAS2013_FTL %>% 
  replace_with_na(replace = list(TA050573 = 9)) 

TIAS2013_IAC <- TIAS2013_IAC %>% 
  replace_with_na(replace = list(TA050573 = 9)) 

T13_HSG_FTLW <- TIAS[, c("TA050573", "FTL_COUNT")] %>% group_by(TA050573, FTL_COUNT) %>% summarise(Count = n())

T13_HSG_FTLW <- T13_HSG_FTLW[1:13, ]

T13_HSG_CAT <- TIAS2013[, c("TA050573", "CAT")] %>% group_by(TA050573, CAT) %>% summarise(Count = n())

T13_HSG_CAT <- T13_HSG_CAT[1:6, ]

T13_HSG_FTLCAT <- TIAS2013_FTL[, c("TA050573", "CAT")] %>% group_by(TA050573, CAT) %>% summarise(Count = n())

T13_HSG_IACCAT <- TIAS2013_IAC[, c("TA050573", "CAT")] %>% group_by(TA050573, CAT) %>% summarise(Count = n())

T13_HSG_IACCAT <- T13_HSG_IACCAT[1:3, ]

head(T13_HSG_CAT, 6)

ggplot(T13_HSG_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050573)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2013", x = "Category", y = "Count") +
  scale_fill_manual("High School Education", values = c("lightblue1", "lightgoldenrod1", "lightpink"), labels = c("HS Diploma", "GED", "Neither"))

head(T13_HSG_FTLW, 13)

ggplot(T13_HSG_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050573)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2013", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("High School Education", values = c("lightblue1", "lightgoldenrod1", "lightpink"), labels = c("HS Diploma", "GED", "Neither"))

prop.table(table(TIAS2013_FTL$TA050573))

hsg.pie.ftl.13 <- ggplot(data = T13_HSG_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050573))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("High School Education", values = c("lightblue1", "lightgoldenrod1", "lightpink"), labels = c("HS Diploma", "GED", "Neither")) +
  theme_void() 

prop.table(table(TIAS2013_IAC$TA050573))

hsg.pie.iac.13 <- ggplot(data = T13_HSG_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050573))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("High School Education", values = c("lightblue1", "lightgoldenrod1", "lightpink"), labels = c("HS Diploma", "GED", "Neither")) +
  theme_void() 

ggarrange(hsg.pie.ftl.13, hsg.pie.iac.13, ncol = 2, nrow = 1, labels = c("FTL 2013", "IAC 2013"))

### College Education ============================================================================================================ 

####
# G10. WTR Ever Attended College: “Have you ever attended college?” 
# Answers: 1 (Yes); 5 (No); 8 (DK); 9 (NA/refused); 0 (Inap: did not complete high school or receive a GED)
####

table(TIAS$TA050594)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050594 = 9)) 

TIAS2013 <- TIAS2013 %>% 
  replace_with_na(replace = list(TA050594 = 9)) 

TIAS2013_FTL <- TIAS2013_FTL %>% 
  replace_with_na(replace = list(TA050594 = 9)) 

TIAS2013_IAC <- TIAS2013_IAC %>% 
  replace_with_na(replace = list(TA050594 = 9)) 

T13_CLG_FTLW <- TIAS[, c("TA050594", "FTL_COUNT")] %>% group_by(TA050594, FTL_COUNT) %>% summarise(Count = n())

T13_CLG_FTLW <- T13_CLG_FTLW[1:14, ]

T13_CLG_CAT <- TIAS2013[, c("TA050594", "CAT")] %>% group_by(TA050594, CAT) %>% summarise(Count = n())

T13_CLG_CAT <- T13_CLG_CAT[1:6, ]

T13_CLG_FTLCAT <- TIAS2013_FTL[, c("TA050594", "CAT")] %>% group_by(TA050594, CAT) %>% summarise(Count = n())

T13_CLG_IACCAT <- TIAS2013_IAC[, c("TA050594", "CAT")] %>% group_by(TA050594, CAT) %>% summarise(Count = n())

T13_CLG_IACCAT <- T13_CLG_IACCAT[1:3, ]

head(T13_CLG_CAT, 6)

ggplot(T13_CLG_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050594)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2013", x = "Category", y = "Count") +
  scale_fill_manual("Higher Education", values = c("lightblue1", "lightgoldenrod1", "lightpink"), labels = c("Attended College", "Graduated HS", "No HS Diploma or GED"))

head(T13_CLG_FTLW, 13)

ggplot(T13_CLG_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050594)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2013", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Higher Education", values = c("lightblue1", "lightgoldenrod1", "lightpink"), labels = c("Attended College", "Graduated HS", "No HS Diploma or GED"))

prop.table(table(TIAS2013_FTL$TA050594))

clg.pie.ftl.13 <- ggplot(data = T13_CLG_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050594))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Higher Education", values = c("lightblue1", "lightgoldenrod1", "lightpink"), labels = c("Attended College", "Graduated HS", "No HS Diploma or GED")) +
  theme_void() 

prop.table(table(TIAS2013_IAC$TA050594))

clg.pie.iac.13 <- ggplot(data = T13_CLG_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050594))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Higher Education", values = c("lightblue1", "lightgoldenrod1", "lightpink"), labels = c("Attended College", "Graduated HS", "No HS Diploma or GED")) +
  theme_void() 

ggarrange(clg.pie.ftl.13, clg.pie.iac.13, ncol = 2, nrow = 1, labels = c("FTL 2013", "IAC 2013"))

### Highest Education Level ====================================================================================================== 

####
# Enrollment Status 
# Answers: 1 (No high school diploma and no GED); 2 (no high school diploma but has GED); 3 (Has high school diploma); 4 (Not enrolled, some college);
# 5 (Not enrolled, 2-yr college graduate); 6 (Not enrolled, 4-yr college graduate); 7 (Not enrolled, graduate degree); 9 (Enrolled, has no prior degree);
# 10 (Enrolled, has a prior degree); 11 (Enrolled, graduate program); 99 (NA/DK)
####

table(TIAS$TA050946)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050946 = 99)) 

TIAS2013 <- TIAS2013 %>% 
  replace_with_na(replace = list(TA050946 = 99)) 

TIAS2013_FTL <- TIAS2013_FTL %>% 
  replace_with_na(replace = list(TA050946 = 99)) 

TIAS2013_IAC <- TIAS2013_IAC %>% 
  replace_with_na(replace = list(TA050946 = 99))  

T13_HEL_FTLW <- TIAS[, c("TA050946", "FTL_COUNT")] %>% group_by(TA050946, FTL_COUNT) %>% summarise(Count = n())

T13_HEL_FTLW <- T13_HEL_FTLW[1:24, ]

T13_HEL_CAT <- TIAS2013[, c("TA050946", "CAT")] %>% group_by(TA050946, CAT) %>% summarise(Count = n())

T13_HEL_CAT <- T13_HEL_CAT[1:12, ]

T13_HEL_FTLCAT <- TIAS2013_FTL[, c("TA050946", "CAT")] %>% group_by(TA050946, CAT) %>% summarise(Count = n())

T13_HEL_FTLCAT <- T13_HEL_FTLCAT[1:4, ]

T13_HEL_IACCAT <- TIAS2013_IAC[, c("TA050946", "CAT")] %>% group_by(TA050946, CAT) %>% summarise(Count = n())

T13_HEL_IACCAT <- T13_HEL_IACCAT[1:8, ]

head(T13_HEL_CAT, 12)

ggplot(T13_HEL_CAT, aes(x = CAT, y = Count, fill = as.factor(TA050946))) + 
  geom_bar(stat="identity", width=1, position = "dodge") + 
  labs(title = "TIAS 2013", x = "Category", y = "Count") + 
  scale_fill_manual("Highest Education Level", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumspringgreen", "mediumslateblue"), 
  labels = c("No HS Diploma, No GED", "No HS Diploma, GED", "HS Diploma", "Not Enrolled, Some College", "Not Enrolled, 2-yr College Graduate", "Enrolled in College, No Prior Degree", 
             "Enrolled in College, Has Prior Degree", "Enrolled in Graduate Program"))

head(T13_HEL_FTLW, 24)

ggplot(T13_HEL_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050946)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2013", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Highest Education Level", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumspringgreen", "mediumslateblue"), 
  labels = c("No HS Diploma, No GED", "No HS Diploma, GED", "HS Diploma", "Not Enrolled, Some College", "Not Enrolled, 2-yr College Graduate", "Enrolled in College, No Prior Degree", 
            "Enrolled in College, Has Prior Degree", "Enrolled in Graduate Program"))

prop.table(table(TIAS2013_FTL$TA050946))

hel.pie.ftl.13 <- ggplot(data = T13_HEL_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050946))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("Highest Education Level", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumspringgreen", "mediumslateblue"), 
  labels = c("No HS Diploma, No GED", "No HS Diploma, GED", "HS Diploma", "Not Enrolled, Some College", "Not Enrolled, 2-yr College Graduate", "Enrolled in College, No Prior Degree", 
            "Enrolled in College, Has Prior Degree", "Enrolled in Graduate Program"))

prop.table(table(TIAS2013_IAC$TA050946))

hel.pie.iac.13 <- ggplot(data = T13_HEL_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050946))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("Highest Education Level", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumspringgreen", "mediumslateblue"), 
  labels = c("No HS Diploma, No GED", "No HS Diploma, GED", "HS Diploma", "Not Enrolled, Some College", "Not Enrolled, 2-yr College Graduate", "Enrolled in College, No Prior Degree", 
            "Enrolled in College, Has Prior Degree", "Enrolled in Graduate Program"))

ggarrange(hel.pie.ftl.13, hel.pie.iac.13, ncol = 2, nrow = 1, labels = c("FTL 2013", "IAC 2013"))

### Responsibility - Earning Own Living ==========================================================================================

####
# B5A. How Much Responsibility Earning Own Living: “As people get older they begin to take more responsibility for themselves. How much responsibility do you currently take for earning your own living?”
# Answers: 1 (Somebody else does this for me all of the time); 2 (Somebody else does this most of the time); 3 (I do this half of the time); 4 (I do this most of the time); 
# 5 (I am completely responsible for this all the time); 8 (DK); 9 (NA; refused)
####

table(TIAS$TA130045)

T13_REO_FTLW <- TIAS[, c("TA130045", "FTL_COUNT")] %>% group_by(TA130045, FTL_COUNT) %>% summarise(Count = n())

T13_REO_FTLW <- T13_REO_FTLW[1:23, ]

T13_REO_CAT <- TIAS2013[, c("TA130045", "CAT")] %>% group_by(TA130045, CAT) %>% summarise(Count = n())

T13_REO_FTLCAT <- TIAS2013_FTL[, c("TA130045", "CAT")] %>% group_by(TA130045, CAT) %>% summarise(Count = n())

T13_REO_IACCAT <- TIAS2013_IAC[, c("TA130045", "CAT")] %>% group_by(TA130045, CAT) %>% summarise(Count = n())

head(T13_REO_CAT, 10)

ggplot(T13_REO_CAT, aes(x = CAT, y = Count, fill = as.factor(TA130045)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2013", x = "Category", y = "Count") + 
  scale_fill_manual("Responsibility - Earning Own Living", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue"), 
                    labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time"))

head(T13_REO_FTLW, 23)

ggplot(T13_REO_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA130045)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2013", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Responsibility - Earning Own Living", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue"), 
                    labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time"))

prop.table(table(TIAS2013_FTL$TA130045))

reo.pie.ftl.13 <- ggplot(data = T13_REO_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA130045))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Responsibility - Earning Own Living", values = c("lightcoral", "mediumturquoise", "deepskyblue", "gold", "green3"),
  labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time")) +
  theme_void()

prop.table(table(TIAS2013_IAC$TA130045))

reo.pie.iac.13 <- ggplot(data = T13_REO_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA130045))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Responsibility - Earning Own Living", values = c("lightcoral", "mediumturquoise", "deepskyblue", "gold", "green3"),
  labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time")) + 
  theme_void()

ggarrange(reo.pie.ftl.13, reo.pie.iac.13, ncol = 2, nrow = 1, labels = c("FTL 2013", "IAC 2013"))

### Responsibility - Paying Own Rent =============================================================================================

####
# B5B. How Much Responsibility Paying Own Rent: “How much responsibility do you currently take for paying your rent or mortgage?"  
# Answers: 1 (Somebody else does this for me all of the time); 2 (Somebody else does this most of the time); 3 (I do this half of the time); 
# 4 (I do this most of the time); 5 (I am completely responsible for this all the time); 6 (No rent or mortgage to pay); 8 (DK); 9 (NA; refused)
####

table(TIAS$TA130046)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA130046 = 8)) 

TIAS2013 <- TIAS2013 %>% 
  replace_with_na(replace = list(TA130046 = 8)) 

TIAS2013_FTL <- TIAS2013_FTL %>% 
  replace_with_na(replace = list(TA130046 = 8)) 

TIAS2013_IAC <- TIAS2013_IAC %>% 
  replace_with_na(replace = list(TA130046 = 8)) 

T13_POR_FTLW <- TIAS[, c("TA130046", "FTL_COUNT")] %>% group_by(TA130046, FTL_COUNT) %>% summarise(Count = n())

T13_POR_FTLW <- T13_POR_FTLW[1:28, ]

T13_POR_CAT <- TIAS2013[, c("TA130046", "CAT")] %>% group_by(TA130046, CAT) %>% summarise(Count = n())

T13_POR_CAT <- T13_POR_CAT[1:13, ]

T13_POR_FTLCAT <- TIAS2013_FTL[, c("TA130046", "CAT")] %>% group_by(TA130046, CAT) %>% summarise(Count = n())

T13_POR_IACCAT <- TIAS2013_IAC[, c("TA130046", "CAT")] %>% group_by(TA130046, CAT) %>% summarise(Count = n())

T13_POR_IACCAT <- T13_POR_IACCAT[1:7, ]

head(T13_POR_CAT, 13)

ggplot(T13_POR_CAT, aes(x = CAT, y = Count, fill = as.factor(TA130046)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2013", x = "Category", y = "Count") + 
  scale_fill_manual("Responsibility - Paying Own Rent", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "blueviolet", "sienna1"), 
                    labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time", "No payments"))

head(T13_POR_FTLW, 28)

ggplot(T13_POR_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA130046)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2013", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Responsibility - Paying Own Rent", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "blueviolet", "sienna1"), 
                    labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time", "No payments"))

prop.table(table(TIAS2013_FTL$TA130046))

por.pie.ftl.13 <- ggplot(data = T13_POR_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA130046))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Responsibility - Paying Own Rent", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "blueviolet", "sienna1"), 
                    labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time", "No payments"))  + theme_void()

prop.table(table(TIAS2013_IAC$TA130046))

por.pie.iac.13 <- ggplot(data = T13_POR_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA130046))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Responsibility - Paying Own Rent", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "blueviolet", "sienna1"), 
                    labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time", "No payments"))  + theme_void()

ggarrange(por.pie.ftl.13, por.pie.iac.13, ncol = 2, nrow = 1, labels = c("FTL 2013", "IAC 2013"))

### Responsibility - Paying Own Bills ============================================================================================

####
# B5C. How Much Responsibility for Own Bills: How much responsibility do you currently take for paying your bills?  
# Answers: 1 (Somebody else does this for me all of the time); 2 (Somebody else does this most of the time); 3 (I do this half of the time); 4 (I do this most of the time); 
# 5 (I am completely responsible for this all the time); 6 (No bills; 8 (DK); 9 (NA; refused)
####

table(TIAS$TA130047)

T13_POB_FTLW <- TIAS[, c("TA130047", "FTL_COUNT")] %>% group_by(TA130047, FTL_COUNT) %>% summarise(Count = n())

T13_POB_FTLW <- T13_POB_FTLW[1:24, ]

T13_POB_CAT <- TIAS2013[, c("TA130047", "CAT")] %>% group_by(TA130047, CAT) %>% summarise(Count = n())

T13_POB_FTLCAT <- TIAS2013_FTL[, c("TA130047", "CAT")] %>% group_by(TA130047, CAT) %>% summarise(Count = n())

T13_POB_IACCAT <- TIAS2013_IAC[, c("TA130047", "CAT")] %>% group_by(TA130047, CAT) %>% summarise(Count = n())

head(T13_POB_CAT, 14)

ggplot(T13_POB_CAT, aes(x = CAT, y = Count, fill = as.factor(TA130047)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2013", x = "Category", y = "Count") + 
  scale_fill_manual("Responsibility - Paying Own Bills", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "blueviolet", "sienna1", "lightpink"), 
                    labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time", "No bills"))

head(T13_POB_FTLW, 24)

ggplot(T13_POB_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA130047)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2013", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Responsibility - Paying Own Bills", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "blueviolet", "sienna1", "lightpink"), 
                    labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time", "No bills"))

prop.table(table(TIAS2013_FTL$TA130047))

pob.pie.ftl.13 <- ggplot(data = T13_POB_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA130047))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("Responsibility - Paying Own Bills", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "blueviolet", "sienna1", "lightpink"), 
                    labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time", "No bills"))
                    

prop.table(table(TIAS2013_IAC$TA130047))

pob.pie.iac.13 <- ggplot(data = T13_POB_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA130047))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("Responsibility - Paying Own Bills", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "blueviolet", "sienna1", "lightpink"), 
                    labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time", "No bills"))  

ggarrange(pob.pie.ftl.13, pob.pie.iac.13, ncol = 2, nrow = 1, labels = c("FTL 2013", "IAC 2013"))

### Responsibility - Managing Money ============================================================================================== 

####
# B5D. How Much Responsibility Managing Money: "How much responsibility do you currently take for managing your money?"
# Answers: 1 (Somebody else does this for me all of the time); 2 (Somebody else does this most of the time); 3 (I do this half of the time); 4 (I do this most of the time); 
# 5 (I am completely responsible for this all the time); 8 (DK); 9 (NA; refused)
####

table(TIAS$TA130048)

T13_RMM_FTLW <- TIAS[, c("TA130048", "FTL_COUNT")] %>% group_by(TA130048, FTL_COUNT) %>% summarise(Count = n())

T13_RMM_FTLW <- T13_RMM_FTLW[1:20, ]

T13_RMM_CAT <- TIAS2013[, c("TA130048", "CAT")] %>% group_by(TA130048, CAT) %>% summarise(Count = n())

T13_RMM_FTLCAT <- TIAS2013_FTL[, c("TA130048", "CAT")] %>% group_by(TA130048, CAT) %>% summarise(Count = n())

T13_RMM_IACCAT <- TIAS2013_IAC[, c("TA130048", "CAT")] %>% group_by(TA130048, CAT) %>% summarise(Count = n())

head(T13_RMM_CAT, 10)

ggplot(T13_RMM_CAT, aes(x = CAT, y = Count, fill = as.factor(TA130048)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  labs(title = "TIAS 2013", x = "Category", y = "Count") + 
  scale_fill_manual("Responsibility - Managing Money", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue"), 
                    labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time"))

head(T13_RMM_FTLW, 20)

ggplot(T13_RMM_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA130048)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2013", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Responsibility - Managing Money", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue"), 
                    labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time"))

prop.table(table(TIAS2013_FTL$TA130048))

rmm.pie.ftl.13 <- ggplot(data = T13_RMM_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA130048))) +  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Responsibility - Managing Money", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue"),
  labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time")) +
  theme_void()

prop.table(table(TIAS2013_IAC$TA130048))

rmm.pie.iac.13 <- ggplot(data = T13_RMM_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA130048))) + geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Responsibility - Managing Money", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue"),
  labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time")) + 
  theme_void()

ggarrange(rmm.pie.ftl.13, rmm.pie.iac.13, ncol = 2, nrow = 1, labels = c("FTL 2013", "IAC 2013"))

### How Good at Taking Responsibility for Actions ================================================================================

####
# B6A. On a scale of 1 to 7, where 1 means "Not At All Well" and 7 means "Extremely Well", how good are you at taking responsibility for your actions?
# Answers: 1-7 (Values range from 1 to 7; 1 represents "not at all well" and 7 represents "extremely well"); 8 (DK); 9 (NA; refused)
####

table(TIAS$TA130049)

T13_ROA_FTLW <- TIAS[, c("TA130049", "FTL_COUNT")] %>% group_by(TA130049, FTL_COUNT) %>% summarise(Count = n())

T13_ROA_FTLW <- T13_ROA_FTLW[1:25, ]

T13_ROA_CAT <- TIAS2013[, c("TA130049", "CAT")] %>% group_by(TA130049, CAT) %>% summarise(Count = n())

ggplot(T13_ROA_FTLW, aes(x = FTL_COUNT, y = TA130049, group = FTL_COUNT, fill = as.factor(FTL_COUNT))) +
  geom_boxplot() +
  labs(title = "TIAS 2013", x = "# of Waves for Which Participant Identified as FTL", y = "Taking Responsibility for Actions - Self-Rating (1 = Not at all well; 7 = Extremely Well)") + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) +
  scale_y_continuous(breaks = seq(1, 7, by = 1)) +
  guides(fill = guide_legend(title = "# of FTL Waves"))

ggplot(T13_ROA_CAT, aes(x = CAT, y = TA130049, group = CAT, fill = as.factor(CAT))) +
  geom_boxplot() +
  labs(title = "TIAS 2013", x = "# of Waves for Which Participant Identified as FTL", y = "Taking Responsibility for Actions - Self-Rating (1 = Not at all well; 7 = Extremely Well)") + 
  scale_y_continuous(breaks = seq(1, 7, by = 1)) +
  guides(fill = guide_legend(title = "Category"))

### How Good at Money Management =================================================================================================

####
# B6C. On a scale of 1 to 7, where 1 means "Not At All Well" and 7 means "Extremely Well", how good are you at managing money?
# Answers: 1-7 (Values range from 1 to 7; 1 represents "not at all well" and 7 represents "extremely well"); 8 (DK); 9 (NA; refused)
####

table(TIAS$TA130051)

T13_GMM_FTLW <- TIAS[, c("TA130051", "FTL_COUNT")] %>% group_by(TA130051, FTL_COUNT) %>% summarise(Count = n())

T13_GMM_FTLW <- T13_GMM_FTLW[1:29, ]

T13_GMM_CAT <- TIAS2013[, c("TA130051", "CAT")] %>% group_by(TA130051, CAT) %>% summarise(Count = n())

ggplot(T13_GMM_FTLW, aes(x = FTL_COUNT, y = TA130051, group = FTL_COUNT, fill = as.factor(FTL_COUNT))) +
  geom_boxplot() +
  labs(title = "TIAS 2013", x = "# of Waves for Which Participant Identified as FTL", y = "How Good at Money Management - Self-Rating (1 = Not at all well; 7 = Extremely Well)") + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) +
  scale_y_continuous(breaks = seq(1, 7, by = 1)) +
  guides(fill = guide_legend(title = "# of FTL Waves"))

ggplot(T13_GMM_CAT, aes(x = CAT, y = TA130051, group = CAT, fill = as.factor(CAT))) +
  geom_boxplot() +
  labs(title = "TIAS 2013", x = "# of Waves for Which Participant Identified as FTL", y = "How Good at Money Management - Self-Rating (1 = Not at all well; 7 = Extremely Well)") + 
  scale_y_continuous(breaks = seq(1, 7, by = 1)) +
  guides(fill = guide_legend(title = "Category"))

### How Good at Paying Off Credit Card Balances ==================================================================================

####
# B6D. On a scale of 1 to 7, where 1 means "Not At All Well" and 7 means "Extremely Well", how good are you at paying off credit card balances each month?
# Answers: 0 (does not have a credit card); 1-7 (Values range from 1 to 7; 1 represents "not at all well" and 7 represents "extremely well"); 8 (DK); 9 (NA; refused)
####

table(TIAS$TA130052)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA130052 = 8)) 

TIAS2013 <- TIAS2013 %>% 
  replace_with_na(replace = list(TA130052 = 8)) 

T13_CCB_FTLW <- TIAS[, c("TA130052", "FTL_COUNT")] %>% group_by(TA130052, FTL_COUNT) %>% summarise(Count = n())

T13_CCB_FTLW <- T13_CCB_FTLW[1:26, ]

T13_CCB_CAT <- TIAS2013[, c("TA130052", "CAT")] %>% group_by(TA130052, CAT) %>% summarise(Count = n())

T13_CCB_CAT <- T13_CCB_CAT[1:16, ]

ggplot(T13_CCB_FTLW, aes(x = FTL_COUNT, y = TA130052, group = FTL_COUNT, fill = as.factor(FTL_COUNT))) +
  geom_boxplot() +
  labs(title = "TIAS 2013", x = "# of Waves for Which Participant Identified as FTL", y = "Paying off Credit Card Balances - Self-Rating (1 = Not at all well; 7 = Extremely Well)") + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) +
  scale_y_continuous(breaks = seq(1, 7, by = 1)) +
  guides(fill = guide_legend(title = "# of FTL Waves"))

ggplot(T13_CCB_CAT, aes(x = CAT, y = TA130052, group = CAT, fill = as.factor(CAT))) +
  geom_boxplot() +
  labs(title = "TIAS 2013", x = "# of Waves for Which Participant Identified as FTL", y = "How Good at Money Management - Self-Rating (1 = Not at all well; 7 = Extremely Well)") + 
  scale_y_continuous(breaks = seq(1, 7, by = 1)) +
  guides(fill = guide_legend(title = "Category"))

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

### Diet Pill Usage =============================================================================================================

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

### Diet Pill Usage =============================================================================================================

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

### Diet Pill Usage =============================================================================================================

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
