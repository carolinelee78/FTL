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

trql.pie.ftl.05 <- ggplot(data = T05_TRQ_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050816))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0)  + 
  scale_fill_manual("Tranquilizer Usage (Prev. Year)", values = c("darkseagreen2", "lightgoldenrod1"), labels = c("Never", "3-5 times")) + 
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050816))

trql.pie.iac.05 <- ggplot(data = T05_TRQ_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050816))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("Tranquilizer Usage (Prev. Year)", values = c("darkseagreen2", "darkslategray2", "lightgoldenrod1", "lightsalmon", "lightpink1"), 
                    labels = c("Never", "1-2 times", "3-5 times", "6-9 times", "10-19 times"))                    

ggarrange(trql.pie.ftl.05, trql.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

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

coca.pie.ftl.05 <- ggplot(data = T05_CCN_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050797))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_manual("Cocaine Usage (Prev. Year)", values = c("lightcoral", "gold"), labels = c("Never", "1-2 times")) +
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050797))

coca.pie.iac.05 <- ggplot(data = T05_CCN_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050797))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_manual("Cocaine Usage (Prev. Year)", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1", "hotpink"), 
  labels = c("Never", "1-2 times", "3-5 times", "6-9 times", "10-19 times", "20-39 times", "40 or more times")) + 
  theme_void() 

ggarrange(coca.pie.ftl.05, coca.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))      

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

avac.pie.ftl.05 <- ggplot(data = T05_AAC_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050767))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Avg. Alcohol Consumption (Prev. Year)", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue"), 
  labels = c("Never", "Less than once a month", "About once a month", "Several times a month", "About once a week")) +
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050767))

avac.pie.iac.05 <- ggplot(data = T05_AAC_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050767))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_manual("Avg. Alcohol Consumption (Prev. Year)", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue", "mediumpurple1", "hotpink"), 
  labels = c("Never", "Less than once a month", "About once a month", "Several times a month", "About once a week", "Several times a week", "Every day")) +
  theme_void() 

ggarrange(avac.pie.ftl.05, avac.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))        

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

adac.pie.ftl.05 <- ggplot(data = T05_DAC_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050768))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Avg. Daily Alcohol Consumption", values =  c("#E69F00", "#56B4E9", "#009E73", "gold", "#D55E00"), labels = c("Never", "1 or fewer drinks", "2 drinks", "3 drinks", "5 drinks")) +
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050768))

adac.pie.iac.05 <- ggplot(data = T05_DAC_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050768))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_manual("Avg. Daily Alcohol Consumption", values =  c("#E69F00", "#56B4E9", "#009E73", "gold", "#0072B2", "#D55E00", "#CC79A7", "lightcoral", "wheat", "green3", "mediumturquoise", 
  "deepskyblue", "mediumpurple1", "hotpink", "khaki1"), labels = c("Never", "1 or fewer drinks", "2 drinks", "3 drinks", "4 drinks", "5 drinks", "6 drinks", "7 drinks", "8 drinks", 
  "9 drinks", "10 drinks", "11 drinks", "12 drinks", "15 drinks", "20 drinks")) +
  theme_void() 

ggarrange(adac.pie.ftl.05, adac.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))          

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

clda.pie.ftl.05 <- ggplot(data = T05_DCN_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050723))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Condition Limits Daily Activities", values = c("lightcoral", "mediumturquoise", "deepskyblue"), 
  labels = c("Inap: No chronic condition", "Just a little", "Not at all")) +
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050723))

clda.pie.iac.05 <- ggplot(data = T05_DCN_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050723))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Condition Limits Daily Activities", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue"), 
  labels = c("Inap: No chronic condition", "A lot", "Somewhat", "Just a little", "Not at all")) +
  theme_void() 

ggarrange(clda.pie.ftl.05, clda.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

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

depr.pie.ftl.05 <- ggplot(data = T05_DEP_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050733))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Depressed >2 Weeks", values = c("aquamarine3", "cadetblue2"), labels = c("Yes", "No")) + 
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050733))

depr.pie.iac.05 <- ggplot(data = T05_DEP_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050733))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Depressed >2 Weeks", values = c("aquamarine3", "cadetblue2"), labels = c("Yes", "No")) + 
  theme_void() 

ggarrange(depr.pie.ftl.05, depr.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

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

anhe.pie.ftl.05 <- ggplot(data = T05_ANH_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050734))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Anhedonia >2 Weeks", values = c("cadetblue1", "lightsalmon"), labels = c("Yes", "No")) + 
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050734))

anhe.pie.iac.05 <- ggplot(data = T05_ANH_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050734))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Anhedonia >2 Weeks", values = c("cadetblue1", "lightsalmon"), labels = c("Yes", "No")) + 
  theme_void() 

ggarrange(anhe.pie.ftl.05, anhe.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

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

anxd.pie.ftl.05 <- ggplot(data = T05_ANX_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050713))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Anxiety Disorder Diagnosis", values = c("cadetblue1", "lightsalmon"), labels = c("Not Diagnosed", "Diagnosed")) + 
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050713))

anxd.pie.iac.05 <- ggplot(data = T05_ANX_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050713))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Anxiety Disorder Diagnosis", values = c("cadetblue1", "lightsalmon"), labels = c("Not Diagnosed", "Diagnosed")) + 
  theme_void() 

ggarrange(anxd.pie.ftl.05, anxd.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

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

# Aggregating mean NSPD scale scores for individuals with different total # of FTL wave match counts 

nspd.ftl.count <- aggregate.data.frame(T05_NPD_FTLW$TA050938, list(T05_NPD_FTLW$FTL_COUNT), mean, na.rm=T)

# Assigning column labels for resulting data frame 

colnames(nspd.ftl.count) <- c("FTL_Wave_Count", "Mean_NSPD")

# Viewing mean NSPD scale score for FTL wave match count total no. categories 

ggplot(nspd.ftl.count, aes(x = FTL_Wave_Count, y = Mean_NSPD, fill = FTL_Wave_Count)) +
  geom_bar(stat = "identity") +
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "Mean NSPD Scale Score") + 
  scale_y_continuous(limits = c(0, 24), breaks = seq(0, 24, by = 2)) + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) +
  theme(legend.position = "none") +
  scale_fill_viridis_c() 

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

# Aggregating mean SA scale scores for individuals with different total # of FTL wave match counts 

sanx.ftl.count <- aggregate.data.frame(T05_SOA_FTLW$TA050933, list(T05_SOA_FTLW$FTL_COUNT), mean, na.rm=T)

# Assigning column labels for resulting data frame 

colnames(sanx.ftl.count) <- c("FTL_Wave_Count", "Mean_SANX")

# Viewing mean SA scale score for FTL wave match count total no. categories 

ggplot(sanx.ftl.count, aes(x = FTL_Wave_Count, y = Mean_SANX, fill = FTL_Wave_Count)) +
  geom_bar(stat = "identity") +
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "Mean SA Scale Score") + 
  scale_y_continuous(limits = c(0, 7), breaks = seq(0, 7, by = 1)) + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) +
  theme(legend.position = "none") +
  scale_fill_viridis_c() 

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

ggplot(worr.ftl.count, aes(x = FTL_Wave_Count, y = Mean_WORR, fill = FTL_Wave_Count)) +
  geom_bar(stat = "identity") +
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "Mean MH-Worry Scale Score") + 
  scale_y_continuous(limits = c(0, 7), breaks = seq(0, 7, by = 1)) + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) +
  theme(legend.position = "none") +
  scale_fill_viridis_c() 

worr.ftl.count <- aggregate.data.frame(T05_WRY_FTLW$TA050932, list(T05_WRY_FTLW$FTL_COUNT), mean, na.rm=T)

colnames(worr.ftl.count) <- c("FTL_Wave_Count", "Mean_WORR")

worr.ftl.count

ggplot(T05_WRY_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050932)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  guides(fill = guide_legend(title = "MH-Worry Scale Score"))

ggplot(T05_WRY_FTLW, aes(x = FTL_COUNT, y = TA050932, group = FTL_COUNT, fill = as.factor(FTL_COUNT))) +
  geom_boxplot() +
  labs(title = "TIAS 2005", x = "# of Waves for Which Participant Identified as FTL", y = "MH-Worry Scale Score") + 
  scale_y_continuous(limits = c(1, 7), breaks = seq(1, 7, by = 1)) + 
  scale_x_continuous(breaks = seq(0, 5, by = 1)) +
  guides(fill = guide_legend(title = "# of FTL Waves"))

worr.cat.count <- aggregate.data.frame(T05_WRY_CAT$TA050932, list(T05_WRY_CAT$CAT), mean, na.rm=T)

colnames(worr.cat.count) <- c("Category", "Mean_WORR")

worr.cat.count

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

empy.pie.ftl.05 <- ggplot(data = T05_EPJ_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050131))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Paid Employment Since Prior Year", values = c("lightblue1", "lightgoldenrod1", "lightpink"), labels = c("Yes (Working Now)", "Yes (Not Now)", "No")) +
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050131))

empy.pie.iac.05 <- ggplot(data = T05_EPJ_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050131))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Paid Employment Since Prior Year", values = c("lightblue1", "lightgoldenrod1", "lightpink"), labels = c("Yes (Working Now)", "Yes (Not Now)", "No")) +
  theme_void() 

ggarrange(empy.pie.ftl.05, empy.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

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

emph.pie.ftl.05 <- ggplot(data = T05_EPH_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050368))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Paid Employment History", values = c("darkturquoise", "darkviolet", "deeppink"), labels = c("Yes (Employed Since 2003 or Before)", "Yes (Not Now)", "No")) +
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050368))

emph.pie.iac.05 <- ggplot(data = T05_EPH_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050368))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Paid Employment History", values = c("darkturquoise", "darkviolet", "deeppink"), labels = c("Yes (Employed Since 2003 or Before)", "Yes (Not Now)", "No")) +
  theme_void() 

ggarrange(emph.pie.ftl.05, emph.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

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

roma.pie.ftl.05 <- ggplot(data = T05_RRN_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050078))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Involved in Romantic Rel.", values = c("darkturquoise", "darkviolet", "deeppink"), labels = c("Inap: Married", "Yes", "No")) +
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050078))

roma.pie.iac.05 <- ggplot(data = T05_RRN_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050078))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Involved in Romantic Rel.", values = c("darkturquoise", "darkviolet", "deeppink"), labels = c("Inap: Married", "Yes", "No")) +
  theme_void() 

ggarrange(roma.pie.ftl.05, roma.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

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

hsgr.pie.ftl.05 <- ggplot(data = T05_HSG_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050573))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("High School Education", values = c("lightblue1", "lightgoldenrod1", "lightpink"), labels = c("HS Diploma", "GED", "Neither")) +
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050573))

hsgr.pie.iac.05 <- ggplot(data = T05_HSG_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050573))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("High School Education", values = c("lightblue1", "lightgoldenrod1", "lightpink"), labels = c("HS Diploma", "GED", "Neither")) +
  theme_void() 

ggarrange(hsgr.pie.ftl.05, hsgr.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

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

hred.pie.ftl.05 <- ggplot(data = T05_CLG_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050594))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Higher Education", values = c("lightblue1", "lightgoldenrod1", "lightpink"), labels = c("Attended College", "Graduated HS", "No HS Diploma or GED")) +
  theme_void() 

prop.table(table(TIAS2005_IAC$TA050594))

hred.pie.iac.05 <- ggplot(data = T05_CLG_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050594))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Higher Education", values = c("lightblue1", "lightgoldenrod1", "lightpink"), labels = c("Attended College", "Graduated HS", "No HS Diploma or GED")) +
  theme_void() 

ggarrange(hred.pie.ftl.05, hred.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

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

helv.pie.ftl.05 <- ggplot(data = T05_HEL_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050946))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("Highest Education Level", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumspringgreen", "mediumslateblue"), 
  labels = c("No HS Diploma, No GED", "No HS Diploma, GED", "HS Diploma", "Not Enrolled, Some College", "Not Enrolled, 2-yr College Graduate", "Enrolled in College, No Prior Degree", 
            "Enrolled in College, Has Prior Degree", "Enrolled in Graduate Program"))

prop.table(table(TIAS2005_IAC$TA050946))

helv.pie.iac.05 <- ggplot(data = T05_HEL_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050946))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("Highest Education Level", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise", "mediumspringgreen", "mediumslateblue"), 
  labels = c("No HS Diploma, No GED", "No HS Diploma, GED", "HS Diploma", "Not Enrolled, Some College", "Not Enrolled, 2-yr College Graduate", "Enrolled in College, No Prior Degree", 
            "Enrolled in College, Has Prior Degree", "Enrolled in Graduate Program"))

ggarrange(helv.pie.ftl.05, helv.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

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

head(T05_REQ_FTLW, 19)

ggplot(T05_REO_FTLW, aes(x = FTL_COUNT, y = Count, fill = as.factor(TA050044)), xlab="Category") +
  geom_bar(stat="identity", width=1, position = "dodge") +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) + 
  labs(title = "TIAS 2005", x = "# of FTL Waves", y = "Count") + 
  scale_fill_manual("Responsibility - Earning Own Living", values = c("lightcoral", "gold", "green3", "mediumturquoise", "deepskyblue"), 
                    labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time"))

prop.table(table(TIAS2005_FTL$TA050044))

reol.pie.ftl.05 <- ggplot(data = T05_REO_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050044))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Responsibility - Earning Own Living", values = c("lightcoral", "mediumturquoise", "deepskyblue", "gold", "green3"),
  labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time")) +
  theme_void()

prop.table(table(TIAS2005_IAC$TA050723))

reol.pie.iac.05 <- ggplot(data = T05_REO_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050044))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Responsibility - Earning Own Living", values = c("lightcoral", "mediumturquoise", "deepskyblue", "gold", "green3"),
  labels = c("Never", "Sometimes", "Half of the time", "Most of the time", "All of the time")) + 
  theme_void()

ggarrange(reol.pie.ftl.05, reol.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

### Responsibility - Paying Own Rent =============================================================================================

####
# How much responsibility do you currently take for paying your rent or mortgage?   
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

####
# A12B. How Often Did School Clubs: “During the last 12 months, about how often were you involved in school clubs or student government? (Would you say: 
# less than once a month, at least once a month, once a week, several times a week, almost every day, or every day?)”
# Answers: 1 (Less than once a month); 2 (At least once a month) 2 (At least once a month); 3 (Once a week); 4 (Several times a week)
# 5 (Almost every day); 6 (Every day); 8 (DK); 9 (NA/refused); 0 (Inap.: not involved with school clubs or student government
####

table(TIAS$TA050033)

### Time Use - Unpaid Volunteer/Community Sevice Work (Over 12 Mos) ==============================================================

####
# A13. WTR Did OTR Volunteer Work: “During the last 12 months, did you do any unpaid volunteer or community service work that you have not told me about?”
# Answers: 1 (Yes); 5 (No); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050034)

### Time Use - Type of Volunteer Work ============================================================================================

#### 
# A14. Type Volunteer ORG--FIRST MENTION: “Which types of organizations have you been involved with in your volunteer or community 
# service work in the last 12 months?--(FIRST MENTION) [PROBE: Anything else?]”
# Answers: 1 (Organizations for children and youth); 2 (Service organizations, such as Big Brothers-Big Sisters or Junior league); 
# 3 (Organized volunteer groups in hospitals or nursing homes); 4 (Religious groups, not including worship); 
# 5 (Conservation, recycling, or environmental groups, such as the Sierra Club or Nature Conservancy); 6 (Shelters, soup kitchens, 
# Habitat for Humanity, or other organizations helping families in need); 7 (Other); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050035)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050035 = 0)) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050035 = 0)) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050035 = 0)) 

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050035 = 0)) 

#### 
# A14 Type Volunteer ORG--SECOND MENTION: “Which types of organizations have you been involved with in your volunteer or community 
# service work in the last 12 months?--(SECOND MENTION) [PROBE: Anything else?]”
####

table(TIAS$TA050036)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050036 = 0)) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050036 = 0)) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050036 = 0)) 

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050036 = 0)) 

#### 
# A14 Type Volunteer ORG--THIRD MENTION: “Which types of organizations have you been involved with in your volunteer or community 
# service work in the last 12 months?--(THIRD MENTION) [PROBE: Anything else?]”
####

table(TIAS$TA050037)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050037 = 0)) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050037 = 0)) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050037 = 0)) 

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050037 = 0)) 

#### 
# A14 Type Volunteer ORG--FOURTH MENTION: “Which types of organizations have you been involved with in your volunteer or community 
# service work in the last 12 months?--(FOURTH MENTION) [PROBE: Anything else?]”
####

table(TIAS$TA050038)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050038 = 0)) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050038 = 0)) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050038 = 0)) 

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050038 = 0)) 


#### 
# A14 Type Volunteer ORG--FIFTH MENTION: “Which types of organizations have you been involved with in your volunteer or community 
# service work in the last 12 months?--(FIFTH MENTION) [PROBE: Anything else?]”
####

table(TIAS$TA050039)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050039 = 0)) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050039 = 0)) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050039 = 0)) 

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050039 = 0)) 


### Time Use - How Often Volunteered (Over 12 Mos) ===============================================================================

####
# A14B. How Often Volunteered: “During the last 12 months, about how often did you participate in volunteer or community service work? 
# Would you say: less than once a month, at least once a month, once a week, several times a week, almost every day, or every day?”
# Answers: 1 (Less than once a month); 2 (At least once a month); 3 (Once a week); 4 (Several times a week); 5 (Almost every day); 
# 6 (Every day); 8 (DK); 9 (NA/refused); 0 (Inap: no unpaid volunteer or community service work)
####

table(TIAS$TA050041)

### Self-Rating (Compared to Others) - Listening & Understanding Others ==========================================================

####
# C1J. How Well Listen Compared w/ Others: “Compared to other people, how good are you at listening to and understanding others? 
# Answers: 1-7 (1 = a lot worse than others <=> 7 = a lot better than others); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050060)

### Self-Rating (Compared to Others) - Teaching & Explaining to Others ===========================================================

####
# C1K. How Good At Teaching Compared w/ Others: “Compared to other people, how good are you at teaching and explaining to others?”
# Answers: 1-7 (1 = a lot worse than others <=> 7 = a lot better than others); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050061)

### Self-Rating (Compared to Others) - Supervising Others ========================================================================

####
# C1A. How Good At Supervising Comp: "Compared to other people, how good are you at supervising others?"
# Answers: 1-7 (1 = a lot worse than others <=> 7 = a lot better than others); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050052)

### Self-Rating (Compared to Others) - Being a Leader ============================================================================

####
# C1B. How Good At Leading Comp w/ Others: “Compared to other people, how good are you at being a leader?”
# Answers: 1-7 (1 = a lot worse than others <=> 7 = a lot better than others); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050053)

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

### Self-Rating (Compared to Others) - Helping Others Solve Their Problems =======================================================

####
# C1D. How Good At Helping Comp w/ Others: “Compared to other people, how good are you at helping others solve their problems?”
# Answers: 1-7 (1 = a lot worse than others <=> 7 = a lot better than others); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050055)

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

### Self-Rating (Compared to Others) - Intelligence ==============================================================================

####
# C1E How Intelligent Compared w/ Others: “Compared to other people, how would you rate your intelligence? (On a scale of 1 to 7, 
# where 1 means ‘A lot worse than other people’ and 7 means ‘A lot better than other people’)”
# Answers: 1-7 (1 = a lot worse than others <=> 7 = a lot better than others); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050056)

### Self-Rating (Compared to Others) - Independence ==============================================================================

####
# C1F. How Independent Compared w/ Others: “Compared to other people, how would you rate your independence?
# Answers: 1-7 (1 = a lot worse than others <=> 7 = a lot better than others); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050057)

### Self-Rating (Compared to Others) - Confidence ================================================================================

####
# C1G. How Confident Compared w/ Others: “Compared to other people, how would you rate your self-confidence?”
# Answers: 1-7 (1 = a lot worse than others <=> 7 = a lot better than others); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050058)

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

### Race - Hispanic ==============================================================================================================

####
# L6. Hispanicity: “In order to get an idea of the different races and ethnic groups that participate in the study, I would like to ask you 
# about your background. Are you Spanish, Hispanic, or Latino? That is, Mexican, Mexican American, Chicano, Puerto Rican, Cuban, or other Spanish?”
# Answers: 0 (Not Spanish, Hispanic, or Latino); 1 (Mexican); 2 (Mexican-American); 3 (Chicano); 4 (Puerto Rican); 5 (Cuban); 7 (Other Spanish); 8 (DK); 9 (NA/refused)
####

table(TIAS$TA050883)

TIAS <- TIAS %>% 
  replace_with_na(replace = list(TA050883 = 9)) 

TIAS2005 <- TIAS2005 %>% 
  replace_with_na(replace = list(TA050883 = 9)) 

TIAS2005_FTL <- TIAS2005_FTL %>% 
  replace_with_na(replace = list(TA050883 = 9)) 

TIAS2005_IAC <- TIAS2005_IAC %>% 
  replace_with_na(replace = list(TA050883 = 9)) 

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

race.pie.ftl.05 <- ggplot(data = T05_RAC_FTLCAT, aes(x = " ", y = Count, fill = as.factor(TA050884))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("Race", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise"), 
                    labels = c("White", "Black", "American Indian or Alaska Native", "Asian", "Native Hawaiian or Pacific Islander", "Other"))

prop.table(table(TIAS2005_IAC$TA050884))

race.pie.iac.05 <- ggplot(data = T05_RAC_IACCAT, aes(x = " ", y = Count, fill = as.factor(TA050884))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual("Race", values =  c("lightcoral", "lightskyblue", "#009E73", "gold", "#0072B2", "turquoise"), 
                    labels = c("White", "Black", "American Indian or Alaska Native", "Asian", "Native Hawaiian or Pacific Islander", "Other"))

ggarrange(race.pie.ftl.05, race.pie.iac.05, ncol = 2, nrow = 1, labels = c("FTL 2005", "IAC 2005"))

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
