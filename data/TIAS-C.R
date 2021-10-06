########### Setup Section ########### 
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

lapply(c("tidyverse"),  pkgTest)
lapply(c("ggplot2"),  pkgTest)
lapply(c("ggthemes"),  pkgTest)
lapply(c("ggpubr"),  pkgTest)
lapply(c("data.table"),  pkgTest)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)
library(data.table)

# set working directory 

setwd("/Users/carolinelee/Desktop/Lebowitz_Lab/FTL/data")

########### TIAS 2005 ###########

# The below code clears your R environment (found on the right side of the console and includes data tables and values). 
# Clearing the environment after you make heatmaps for each wave is important because otherwise the environment gets crowded – it would be hard to find data and values you need from the 2005 wave). 

rm(list=ls())

# Import the TIAS data. This dataset was downloaded from the PSID website and includes values for all waves. The raw csv file can be found on github. 

TIAS <- fread("https://raw.githubusercontent.com/carolinelee78/FTL/main/data/raw/PSID/TIAS/TIAS.csv")

# We will also add the unique individual identifier ID calculated using the method recommended by PSID researchers 

TIAS$PSID_ID <- (TIAS$ER30001 * 1000) + TIAS$ER30002

ALL_PSID_ID <- TIAS$PSID_ID

# This selects the data only from the wave of interest, which in this case is 2005.    

TIAS2005 <- TIAS[!is.na(TIAS$TAS05),]

# Now, we select the variables of interest that filter for participants who meet FTL criteria. See TIAS-C variable table for the names and details of these variables. 

TIAS2005$CAT <- with(TIAS2005, ifelse(
  TA050042 == 1 & TA050043 %in% c("1", "96") & TA050595 %in% c("5", "0") & TA050946 %in% c("1", "2", "3", "4", "5", "6", "7") & TA050631 %in% c("5", "0") & TA050127 == 3 & TA050769 < 60 & 
  TA050712 == 0 & TA050715 == 0 & TA050711 == 0 & TA050716 == 0 & TA050678 %in% c("3", "5", "7", "0") & TA050785 == 0 & TA050809 == 0 & TA050793 %in% c("0", "1", "2", "3") & TA050777 == 0 & TA050825 == 0 & 
  TA050817 == 0 & TA050798 == 0 & TA050394 %in% c("1", "2", "3", "4", "7", "8", "97", "99") & TA050091 == 0, "FTL_05", ifelse(
      TA050042 == 1 & TA050043 %in% c("1", "96") & TA050595 %in% c("5", "0") & TA050946 %in% c("1", "2", "3", "4", "5", "6", "7") & TA050631 %in% c("5", "0") & TA050127 == 3 & TA050769 < 60 & 
      TA050712 == 0 & TA050715 == 0 & TA050711 == 0 & TA050716 == 0 & TA050678 %in% c("3", "5", "7", "0") & TA050785 == 0 & TA050809 == 0 & TA050793 %in% c("0", "1", "2", "3") & TA050777 == 0 & TA050825 == 0 & 
      TA050817 == 0 & TA050798 == 0 & TA050394 == 0 & TA050371 == 1 & TA050091 == 0, "FTL_05", "IAC_05")))

# We will also add the unique individual identifier ID calculated using the method recommended by PSID researchers 

TIAS2005$PSID_ID <- (TIAS2005$ER30001 * 1000) + TIAS2005$ER30002

# Before subsetting the data to only include data for the wave of interest, we are adding PSID IDs for each participant

T05_ID <- TIAS2005$PSID_ID

# Extract IDs of participants who have been identified as FTL for the 2005 wave 

FTL05_ID <- TIAS2005[TIAS2005$CAT == "FTL_05", "PSID_ID"]

# Count the number of participants who have been identified as FTL for the 2005 wave 

nrow(FTL05_ID)

# View the number of FTL vs. IAC participants for the 2005 wave 

table(TIAS2005$CAT)

FTL05_ID_VEC <- unname(unlist(FTL05_ID))

# Create a new variable ('CAT_05') for FTL vs. IAC vs. NA (no data) categorization in the 2005 wave 

TIAS$CAT_05 <- with(TIAS, ifelse(
  PSID_ID %in% FTL05_ID_VEC, "FTL_05", ifelse(
    PSID_ID %in% T05_ID, "IAC_05", "NA_05")))

# View the distribution for CAT_05

table(TIAS$CAT_05)

# View the IDs of participants who have been identified as FTL for the 2005 wave 

print(FTL05_ID_VEC)

# Again, we are selecting these variables to create a dataset with only the relevant information for our TIAS criteria. 

TIAS2005 <- TIAS2005 %>% select(ID, PSID_ID, CAT, TA050042, TA050043, TA050595, TA050946, TA050631, TA050127, TA050769, TA050712, TA050715, TA050716, TA050678, 
                                TA050785, TA050809, TA050793, TA050777, TA050825, TA050817, TA050798, TA050394, TA050371, TA050091)

# Without altering the values of the variables in the original dataframe, we will create a new boolean variable (their original PSID name with _M at the end, M for match) for each variable (TRUE if matching FTL, FALSE if not)

TIAS2005$TA050042_M <- TIAS2005$TA050042 == 1 
TIAS2005$TA050043_M <- TIAS2005$TA050043 %in% c("1", "96")
TIAS2005$TA050595_M <- TIAS2005$TA050595 %in% c("5", "0")
TIAS2005$TA050946_M <- TIAS2005$TA050946 %in% c("1", "2", "3", "4", "5", "6", "7")
TIAS2005$TA050631_M <- TIAS2005$TA050631 %in% c("5", "0")
TIAS2005$TA050127_M <- TIAS2005$TA050127 == 3
TIAS2005$TA050769_M <- TIAS2005$TA050769 < 60
TIAS2005$TA050712_M <- TIAS2005$TA050712 == 0 
TIAS2005$TA050715_M <- TIAS2005$TA050715 == 0 
TIAS2005$TA050711_M <- TIAS2005$TA050711 == 0
TIAS2005$TA050716_M <- TIAS2005$TA050716 == 0 
TIAS2005$TA050678_M <- TIAS2005$TA050678 %in% c("3", "5", "7", "0")
TIAS2005$TA050785_M <- TIAS2005$TA050785 == 0
TIAS2005$TA050809_M <- TIAS2005$TA050809 == 0
TIAS2005$TA050793_M <- TIAS2005$TA050793 %in% c("0", "1", "2", "3")
TIAS2005$TA050777_M <- TIAS2005$TA050777 == 0
TIAS2005$TA050825_M <- TIAS2005$TA050825 == 0
TIAS2005$TA050817_M <- TIAS2005$TA050817 == 0
TIAS2005$TA050798_M <- TIAS2005$TA050798 == 0
TIAS2005$TA050394_M <- ifelse(TIAS2005$TA050394 %in% c("1", "2", "3", "4", "7", "8", "97", "99"), TRUE,
                              ifelse(TIAS2005$TA050394 == 0 & TIAS2005$TA050371 == 1, TRUE, FALSE))
TIAS2005$TA050091_M <- TIAS2005$TA050091 == 0

# Here, we are creating a new dataframe with only the boolean variables.  

T2005M <- TIAS2005 %>% select(TA050042_M, TA050043_M, TA050595_M, TA050946_M, TA050631_M, TA050127_M, TA050769_M, TA050712_M, TA050715_M, TA050711_M, TA050716_M, TA050678_M, 
                              TA050785_M, TA050809_M, TA050793_M, TA050777_M, TA050825_M, TA050817_M, TA050798_M, TA050394_M, TA050091_M, PSID_ID)

# We then have to convert the boolean values (FALSE/TRUE) to binary values (0/1), as we will be working with these to create our heatmaps. 

cols <- sapply(T2005M, is.logical)
T2005M[,cols] <- lapply(T2005M[,cols], as.numeric)

# Find the number of columns in the new dataset of FTL variables for the 2005 wave. Important note: the last column is 'ID', so the column range to plug into the R function tidy.vars would be 1:(ncol-1).

ncol(T2005M)  

# Since we can't create a single plot with thousands of columns (including all participants), we have to divide the dataframe into 'chunks' to create subplots before creating aggregated plots. 

nrow(T2005M) # Find out how many rows (participants) are in T2005M. 

chunk <- 50 # The value of the chunk should be the no. of columns (participants) you would want for each heatmap subplot. 

n <- nrow(T2005M) # Save the number of rows in T2005M. 

r <- rep(1:ceiling(n/chunk), each=chunk)[1:n] # This command loops through the dataframe to create chunks (if the total # of rows was not cleanly divisible by 50, the # of rows included in the last chunk would be the remainder)

T2005M_list <- split(T2005M, r) # This command splits the dataframe into chunks as calculated above. 

length(T2005M_list) # Now find out how many chunks of 50 have been created from the total number of participants. i for functions would be 1:(# of chunks).

# This function lets us tidy the data into the long format. 

tidy.vars <- function(x){
  x %>% tidyr::gather(variable, met_FTL_crt, 1:21)
}

# Apply the function to T2005M chunks, tidying the data. 

T2005M_tidy_list <- lapply(T2005M_list, tidy.vars)

# This for-loop lets us create and assign factor levels for each variable in T2005M (excluding the last column 'ID'). 

for(i in 1:15) {
  T2005M_tidy_list[[i]]$variable <- factor(T2005M_tidy_list[[i]]$variable, levels = c("TA050042_M", "TA050043_M", "TA050595_M", "TA050946_M", "TA050631_M", "TA050127_M", "TA050769_M", "TA050712_M", "TA050715_M", "TA050711_M", "TA050716_M", "TA050678_M", 
                                                                                      "TA050785_M", "TA050809_M", "TA050793_M", "TA050777_M", "TA050825_M", "TA050817_M", "TA050798_M", "TA050394_M", "TA050091_M"))
} 

# This function lets us isolate and save the values for the 'ID' column.

set.ID.levels <- function(x){
  dplyr::pull(x, PSID_ID)
}

# Apply the function, saving ID values for each chunk.

T2005M_levels_list <- lapply(T2005M_list, set.ID.levels)

# This for-loop lets us create and assign factor levels for IDs in each T2005M chunk (in tidy long format) as corresponding to all included variables.  

for(i in 1:15) {
  T2005M_tidy_list[[i]]$PSID_ID <- factor(T2005M_tidy_list[[i]]$PSID_ID, levels = T2005M_levels_list[[i]])
}

# This for-loop lets us create and assign factor levels for the binary values (0/1; 0 = IAC, 1 = FTL) stored in each variable. 

for(i in 1:15) {
  T2005M_tidy_list[[i]]$met_FTL_crt <- factor(T2005M_tidy_list[[i]]$met_FTL_crt)
}

# This function lets us create the heatmap itself. 

create.heatmap <- function(x){
  ggplot(x, aes(x=PSID_ID, y=variable, fill=met_FTL_crt)) + geom_tile(color="white", size=0.5) +
    coord_equal() +
    labs(x="ID", y="Criteria") +
    theme_tufte(base_family="Helvetica") +
    theme(axis.ticks=element_blank()) + 
    theme(axis.text.x=element_text(angle = 45, hjust = 1)) + 
    scale_fill_discrete(name = "Met FTL Criteria", labels = c("No", "Yes"))
}

# This function lets us create the heatmap specifically with T2005M data. Now we just have to plug in the chunk/subsection # into the T05.heatmap function to view each subplot.

T05.heatmaps <- function(x){
  create.heatmap(T2005M_tidy_list[[x]])
}

# To create fewer plots to save overall and for the sake of easier data visualization, we will combine the subplots to create aggregated plots. 

TIAS2005_plot <- ggarrange(T05.heatmaps(1) + rremove("legend"), T05.heatmaps(2) + rremove("legend") + rremove("y.title"), T05.heatmaps(3) + rremove("legend") + rremove("y.title"), 
                           T05.heatmaps(4) + rremove("legend"), T05.heatmaps(5) + rremove("legend") + rremove("y.title"), T05.heatmaps(6) + rremove("legend") + rremove("y.title"), 
                           T05.heatmaps(7) + rremove("legend"), T05.heatmaps(8) + rremove("legend") + rremove("y.title"), T05.heatmaps(9) + rremove("legend") + rremove("y.title"), 
                           T05.heatmaps(10) + rremove("legend"), T05.heatmaps(11) + rremove("legend") + rremove("y.title"), T05.heatmaps(12) + rremove("legend") + rremove("y.title"),
                           T05.heatmaps(13) + rremove("legend"), T05.heatmaps(14) + rremove("legend") + rremove("y.title"), T05.heatmaps(15) + rremove("y.title"), ncol = 3, nrow = 5) 

TIAS2005_plot # This command lets us view the aggregated plot. To see clearly, you export manually as png with width 3000 height 2800. This is done by selecting plots>export>png and entering the correct values for width and height.

########### TIAS 2007 ###########

# This selects the data only from the wave of interest, which in this case is 2007.    

TIAS2007 <- TIAS[!is.na(TIAS$TAS07),]

# Now, we select the variables of interest that filter for participants who meet FTL criteria. See TIAS-C variable table for the names and details of these variables. 

TIAS2007$CAT <- with(TIAS2007, ifelse(
  TA070042 == 1 & TA070043 %in% c("1", "96") & TA070570 %in% c("5", "0") & TA070927 %in% c("1", "2", "3", "4", "5", "6", "7") & TA070602 %in% c("5", "0") & TA070127 == 3 & TA070740 < 60 & 
  TA070683 == 0 & TA070686 == 0 & TA070682 == 0 & TA070687 == 0 & TA070649 %in% c("3", "5", "7", "0") & TA070756 == 0 & TA070777 == 0 & TA070764 %in% c("0", "1", "2", "3") & TA070748 == 0 & TA070793 == 0 & 
  TA070785 == 0 & TA070769 == 0 & TA070368 %in% c("1", "2", "3", "4", "7", "8", "97", "99") & TA070091 == 0, "FTL_07", ifelse(
      TA070042 == 1 & TA070043 %in% c("1", "96") & TA070570 %in% c("5", "0") & TA070927 %in% c("1", "2", "3", "4", "5", "6", "7") & TA070602 %in% c("5", "0") & TA070127 == 3 & TA070740 < 60 & 
      TA070683 == 0 & TA070686 == 0 & TA070682 == 0 & TA070687 == 0 & TA070649 %in% c("3", "5", "7", "0") & TA070756 == 0 & TA070777 == 0 & TA070764 %in% c("0", "1", "2", "3") & TA070748 == 0 & TA070793 == 0 & 
      TA070785 == 0 & TA070769 == 0 & TA070368 == 0 & TA050371 == 1 & TA070091 == 0, "FTL_07", "IAC_07")))

# We will also add the unique individual identifier ID calculated using the method recommended by PSID researchers 

TIAS2007$PSID_ID <- (TIAS2007$ER30001 * 1000) + TIAS2007$ER30002

# Before subsetting the data to only include data for the wave of interest, we are adding PSID IDs for each participant

T07_ID <- TIAS2007$PSID_ID

# Extract IDs of participants who have been identified as FTL for the 2007 wave 

FTL07_ID <- TIAS2007[TIAS2007$CAT == "FTL_07", "PSID_ID"]

# Count the number of participants who have been identified as FTL for the 2007 wave 

nrow(FTL07_ID)

# View the number of FTL vs. IAC participants for the 2007 wave 

table(TIAS2007$CAT)

FTL07_ID_VEC <- unname(unlist(FTL07_ID))

# Create a new variable ('CAT_07') for FTL vs. IAC vs. NA (no data) categorization in the 2007 wave 

TIAS$CAT_07 <- with(TIAS, ifelse(
  PSID_ID %in% FTL07_ID_VEC, "FTL_07", ifelse(
    PSID_ID %in% T07_ID, "IAC_07", "NA_07")))

# View the distribution for CAT_07

table(TIAS$CAT_07)

# View the IDs of participants who have been identified as FTL for the 2007 wave 

print(FTL07_ID_VEC)

# Again, we are selecting these variables to create a dataset with only the relevant information for our TIAS criteria. 

TIAS2007 <- TIAS2007 %>% select(ID, PSID_ID, CAT, TA070042, TA070043, TA070570, TA070927, TA070602, TA070127, TA070740, TA070683, TA070686, TA070687, TA070649, 
                                TA070756, TA070777, TA070764, TA070748, TA070793, TA070785, TA070769, TA070368, TA050371, TA070091)

# Without altering the values of the variables in the original dataframe, we will create a new boolean variable (their original PSID name with _M at the end, M for match) for each variable (TRUE if matching FTL, FALSE if not)

TIAS2007$TA070042_M <- TIAS2007$TA070042 == 1 
TIAS2007$TA070043_M <- TIAS2007$TA070043 %in% c("1", "96")
TIAS2007$TA070570_M <- TIAS2007$TA070570 %in% c("5", "0")
TIAS2007$TA070927_M <- TIAS2007$TA070927 %in% c("1", "2", "3", "4", "5", "6", "7")
TIAS2007$TA070602_M <- TIAS2007$TA070602 %in% c("5", "0")
TIAS2007$TA070127_M <- TIAS2007$TA070127 == 3
TIAS2007$TA070740_M <- TIAS2007$TA070740 < 60
TIAS2007$TA070683_M <- TIAS2007$TA070683 == 0 
TIAS2007$TA070686_M <- TIAS2007$TA070686 == 0 
TIAS2007$TA070682_M <- TIAS2007$TA070682 == 0
TIAS2007$TA070687_M <- TIAS2007$TA070687 == 0 
TIAS2007$TA070649_M <- TIAS2007$TA070649 %in% c("3", "5", "7", "0")
TIAS2007$TA070756_M <- TIAS2007$TA070756 == 0
TIAS2007$TA070777_M <- TIAS2007$TA070777 == 0
TIAS2007$TA070764_M <- TIAS2007$TA070764 %in% c("0", "1", "2", "3")
TIAS2007$TA070748_M <- TIAS2007$TA070748 == 0
TIAS2007$TA070793_M <- TIAS2007$TA070793 == 0
TIAS2007$TA070785_M <- TIAS2007$TA070785 == 0
TIAS2007$TA070769_M <- TIAS2007$TA070769 == 0
TIAS2007$TA070368_M <- ifelse(TIAS2007$TA070368 %in% c("1", "2", "3", "4", "7", "8", "97", "99"), TRUE,
                              ifelse(TIAS2007$TA070368 == 0 & TIAS2007$TA070344 == 1, TRUE, FALSE))
TIAS2007$TA070091_M <- TIAS2007$TA070091 == 0

# Here, we are creating a new dataframe with only the boolean variables.  

T2007M <- TIAS2007 %>% select(TA070042_M, TA070043_M, TA070570_M, TA070927_M, TA070602_M, TA070127_M, TA070740_M, TA070683_M, TA070686_M, TA070682_M, TA070687_M, TA070649_M, 
                              TA070756_M, TA070777_M, TA070764_M, TA070748_M, TA070793_M, TA070785_M, TA070769_M, TA070368_M, TA070091_M, PSID_ID)

# We then have to convert the boolean values (FALSE/TRUE) to binary values (0/1), as we will be working with these to create our heatmaps. 

cols <- sapply(T2007M, is.logical)
T2007M[,cols] <- lapply(T2007M[,cols], as.numeric)

# Find the number of columns in the new dataset of FTL variables for the 2007 wave. Important note: the last column is 'ID', so the column range to plug into the R function tidy.vars would be 1:(ncol-1).

ncol(T2007M)  

# Since we can't create a single plot with thousands of columns (including all participants), we have to divide the dataframe into 'chunks' to create subplots before creating aggregated plots. 

nrow(T2007M) # Find out how many rows (participants) are in T2007M. 

chunk <- 75 # The value of the chunk should be the no. of columns (participants) you would want for each heatmap subplot. 

n <- nrow(T2007M) # Save the number of rows in T2007M. 

r <- rep(1:ceiling(n/chunk), each=chunk)[1:n] # This command loops through the dataframe to create chunks (if the total # of rows was not cleanly divisible by 50, the # of rows included in the last chunk would be the remainder)

T2007M_list <- split(T2007M, r) # This command splits the dataframe into chunks as calculated above. 

length(T2007M_list) # Now find out how many chunks of 50 have been created from the total number of participants. i for functions would be 1:(# of chunks).

# This function lets us tidy the data into the long format. 

tidy.vars <- function(x){
  x %>% tidyr::gather(variable, met_FTL_crt, 1:21)
}

# Apply the function to T2007M chunks, tidying the data. 

T2007M_tidy_list <- lapply(T2007M_list, tidy.vars)

# This for-loop lets us create and assign factor levels for each variable in T2007M (excluding the last column 'ID'). 

for(i in 1:15) {
  T2007M_tidy_list[[i]]$variable <- factor(T2007M_tidy_list[[i]]$variable, levels = c("TA070042_M", "TA070043_M", "TA070570_M", "TA070927_M", "TA070602_M", "TA070127_M", "TA070740_M", "TA070683_M", "TA070686_M", "TA070682_M", "TA070687_M", "TA070649_M", 
                                                                                      "TA070756_M", "TA070777_M", "TA070764_M", "TA070748_M", "TA070793_M", "TA070785_M", "TA070769_M", "TA070368_M", "TA070091_M"))
} 

# This function lets us isolate and save the values for the 'ID' column.

set.ID.levels <- function(x){
  dplyr::pull(x, PSID_ID)
}

# Apply the function, saving ID values for each chunk.

T2007M_levels_list <- lapply(T2007M_list, set.ID.levels)

# This for-loop lets us create and assign factor levels for IDs in each T2007M chunk (in tidy long format) as corresponding to all included variables.  

for(i in 1:15) {
  T2007M_tidy_list[[i]]$PSID_ID <- factor(T2007M_tidy_list[[i]]$PSID_ID, levels = T2007M_levels_list[[i]])
}

# This for-loop lets us create and assign factor levels for the binary values (0/1; 0 = IAC, 1 = FTL) stored in each variable. 

for(i in 1:15) {
  T2007M_tidy_list[[i]]$met_FTL_crt <- factor(T2007M_tidy_list[[i]]$met_FTL_crt)
}

# This function lets us create the heatmap itself. 

create.heatmap <- function(x){
  ggplot(x, aes(x=PSID_ID, y=variable, fill=met_FTL_crt)) + geom_tile(color="white", size=0.5) +
    coord_equal() +
    labs(x="ID", y="Criteria") +
    theme_tufte(base_family="Helvetica") +
    theme(axis.ticks=element_blank()) + 
    theme(axis.text.x=element_text(angle = 45, hjust = 1)) + 
    scale_fill_discrete(name = "Met FTL Criteria", labels = c("No", "Yes"))
}

# This function lets us create the heatmap specifically with T2007M data. Now we just have to plug in the chunk/subsection # into the T07.heatmap function to view each subplot.

T07.heatmaps <- function(x){
  create.heatmap(T2007M_tidy_list[[x]])
}

# To create fewer plots to save overall and for the sake of easier data visualization, we will combine the subplots to create aggregated plots. 

TIAS2007_plot <- ggarrange(T07.heatmaps(1) + rremove("legend"), T07.heatmaps(2) + rremove("legend") + rremove("y.title"), T07.heatmaps(3) + rremove("legend") + rremove("y.title"), 
                           T07.heatmaps(4) + rremove("legend"), T07.heatmaps(5) + rremove("legend") + rremove("y.title"), T07.heatmaps(6) + rremove("legend") + rremove("y.title"), 
                           T07.heatmaps(7) + rremove("legend"), T07.heatmaps(8) + rremove("legend") + rremove("y.title"), T07.heatmaps(9) + rremove("legend") + rremove("y.title"), 
                           T07.heatmaps(10) + rremove("legend"), T07.heatmaps(11) + rremove("legend") + rremove("y.title"), T07.heatmaps(12) + rremove("legend") + rremove("y.title"),
                           T07.heatmaps(13) + rremove("legend"), T07.heatmaps(14) + rremove("legend") + rremove("y.title"), T07.heatmaps(15) + rremove("y.title"), ncol = 3, nrow = 5) 

TIAS2007_plot # This command lets us view the aggregated plot. To see clearly, you export manually as png with width 3000 height 2800. This is done by selecting plots>export>png and entering the correct values for width and height.

########### TIAS 2009 ###########

# This selects the data only from the wave of interest, which in this case is 2009.    

TIAS2009 <- TIAS[!is.na(TIAS$TAS09),]

# Now, we select the variables of interest that filter for participants who meet FTL criteria. See TIAS-C variable table for the names and details of these variables.   

TIAS2009$CAT <- with(TIAS2009, ifelse(
  TA090043 == 1 & TA090044 %in% c("1", "96") & TA090612 %in% c("5", "0") & TA090991 %in% c("1", "2", "3", "4", "5", "6", "7") & TA090655 %in% c("5", "0") & TA090136 == 3 & TA090799 < 60 & 
  TA090739 == 0 & TA090742 == 0 & TA090738 == 0 & TA090743 == 0 & TA090705 %in% c("3", "5", "7", "0") & TA090815 == 0 & TA090836 == 0 & TA090823 %in% c("0", "1", "2", "3") & TA090807 == 0 & TA090852 == 0 & 
  TA090844 == 0 & TA090828 == 0 & TA090385 %in% c("1", "2", "3", "4", "7", "8", "97", "99") & TA090100 == 0, "FTL_09", ifelse(
      TA090043 == 1 & TA090044 %in% c("1", "96") & TA090612 %in% c("5", "0") & TA090991 %in% c("1", "2", "3", "4", "5", "6", "7") & TA090655 %in% c("5", "0") & TA090136 == 3 & TA090799 < 60 & 
      TA090739 == 0 & TA090742 == 0 & TA090738 == 0 & TA090743 == 0 & TA090705 %in% c("3", "5", "7", "0") & TA090815 == 0 & TA090836 == 0 & TA090823 %in% c("0", "1", "2", "3") & TA090807 == 0 & TA090852 == 0 & 
      TA090844 == 0 & TA090828 == 0 & TA090385 == 0 & TA090361 == 1 & TA090100 == 0, "FTL_09", "IAC_09")))

# Before subsetting the data to only include data for the wave of interest, we are adding IDs for each row in a new column ('ID') to consistently identify each row (participant).

T09_ID <- TIAS2009$ID

# We will also add the unique individual identifier ID calculated using the method recommended by PSID researchers 

TIAS2009$PSID_ID <- (TIAS2009$ER30001 * 1000) + TIAS2009$ER30002

# Extract IDs of participants who have been identified as FTL for the 2009 wave 

FTL09_ID <- TIAS2009[TIAS2009$CAT == "FTL_09", "ID"]

# Count the number of participants who have been identified as FTL for the 2009 wave 

nrow(FTL09_ID)

# View the number of FTL vs. IAC participants for the 2009 wave 

table(TIAS2009$CAT)

# Create a new variable ('CAT_09') for FTL vs. IAC vs. NA (no data) categorization in the 2009 wave 

TIAS$CAT_09 <- with(TIAS, ifelse(
  ID %in% FTL09_ID, "FTL_09", ifelse(
    ID %in% T09_ID, "IAC_09", "NA_09")))

# View the distribution for CAT_09

table(TIAS$CAT_09)

# Creating a subsetted dataframe including only FTL participants for the 2009 wave 

TIAS2009_FTL <- subset(TIAS2009, CAT == "FTL_09")

# Creating a subsetted dataframe including only IAC participants for the 2009 wave 

TIAS2009_IAC <- subset(TIAS2009, CAT == "IAC_09")

# View the IDs of participants who have been identified as FTL for the 2009 wave 

print(FTL09_ID)

print(TIAS2009_FTL$PSID_ID)

# Again, we are selecting these variables to create a dataset with only the relevant information for our TIAS criteria.

TIAS2009 <- TIAS2009 %>% select(ID, PSID_ID, CAT, TA090043, TA090044, TA090612, TA090991, TA090655, TA090136, TA090799, TA090739, TA090742, TA090738, TA090743, TA090705, TA090815, 
                                TA090836, TA090823, TA090807, TA090852, TA090844, TA090828, TA090385, TA090361, TA090100) 

# Without altering the values of the variables in the original dataframe, we will create a new boolean variable (their original PSID name with _M at the end, M for match) for each variable (TRUE if matching FTL, FALSE if not).

TIAS2009$TA090043_M <- TIAS2009$TA090043 == 1 
TIAS2009$TA090044_M <- TIAS2009$TA090044 %in% c("1", "96")
TIAS2009$TA090612_M <- TIAS2009$TA090612 %in% c("5", "0")
TIAS2009$TA090991_M <- TIAS2009$TA090991 %in% c("1", "2", "3", "4", "5", "6", "7")
TIAS2009$TA090655_M <- TIAS2009$TA090655 %in% c("5", "0")
TIAS2009$TA090136_M <- TIAS2009$TA090136 == 3
TIAS2009$TA090799_M <- TIAS2009$TA090799 < 60
TIAS2009$TA090739_M <- TIAS2009$TA090739 == 0 
TIAS2009$TA090742_M <- TIAS2009$TA090742 == 0 
TIAS2009$TA090738_M <- TIAS2009$TA090738 == 0
TIAS2009$TA090743_M <- TIAS2009$TA090743 == 0 
TIAS2009$TA090705_M <- TIAS2009$TA090705 %in% c("3", "5", "7", "0")
TIAS2009$TA090815_M <- TIAS2009$TA090815 == 0
TIAS2009$TA090836_M <- TIAS2009$TA090836 == 0
TIAS2009$TA090823_M <- TIAS2009$TA090823 %in% c("0", "1", "2", "3") 
TIAS2009$TA090807_M <- TIAS2009$TA090807 == 0
TIAS2009$TA090852_M <- TIAS2009$TA090852 == 0
TIAS2009$TA090844_M <- TIAS2009$TA090844 == 0
TIAS2009$TA090828_M <- TIAS2009$TA090828 == 0
TIAS2009$TA090385_M <- ifelse(TIAS2009$TA090385 %in% c("1", "2", "3", "4", "7", "8", "97", "99"), TRUE,
                              ifelse(TIAS2009$TA090385 == 0 & TIAS2009$TA090361 == 1, TRUE, FALSE))
TIAS2009$TA090100_M <- TIAS2009$TA090100 == 0

# Here, we are creating a new dataframe with only the boolean variables.  

T2009M <- TIAS2009 %>% select(TA090043_M, TA090044_M, TA090612_M, TA090991_M, TA090655_M, TA090136_M, TA090799_M, TA090739_M, TA090742_M, TA090738_M, TA090743_M, TA090705_M, TA090815_M, 
                              TA090836_M, TA090823_M, TA090807_M, TA090852_M, TA090844_M, TA090828_M, TA090385_M, TA090100_M, PSID_ID) 

# We then have to convert the boolean values (FALSE/TRUE) to binary values (0/1), as we will be working with these to create our heatmaps.

cols <- sapply(T2009M, is.logical)
T2009M[,cols] <- lapply(T2009M[,cols], as.numeric)

# Find the number of columns in the new dataset of FTL variables for the 2009 wave. Important note: the last column is 'ID', so the column range to plug into the R function tidy.vars would be 1:(ncol-1).

ncol(T2009M) 

# Since we can't create a single plot with thousands of columns (including all participants), we have to divide the dataframe into 'chunks' to create subplots before creating aggregated plots. 

nrow(T2009M) # Find out how many rows (participants) are in T2009M. 

chunk <- 49 # The value of the chunk should be the no. of columns (participants) you would want for each heatmap subplot. 

n <- nrow(T2009M) # Save the number of rows in T2009M. 

r <- rep(1:ceiling(n/chunk), each=chunk)[1:n] # This command loops through the dataframe to create chunks (if the total # of rows was not cleanly divisible by 50, the # of rows included in the last chunk would be the remainder).

T2009M_list <- split(T2009M, r) # This command splits the dataframe into chunks as calculated above. 

length(T2009M_list) # Now find out how many chunks of 50 have been created from the total number of participants. i for functions would be 1:(# of chunks).

# This function lets us tidy the data into the long format. 

tidy.vars <- function(x){
  x %>% tidyr::gather(variable, met_FTL_crt, 1:21)
}

# Apply the function to T2009M chunks, tidying the data. 

T2009M_tidy_list <- lapply(T2009M_list, tidy.vars)

# This for-loop lets us create and assign factor levels for each variable in T2009M (excluding the last column 'ID'). 

for(i in 1:32) {
  T2009M_tidy_list[[i]]$variable <- factor(T2009M_tidy_list[[i]]$variable, levels = c("TA090043_M", "TA090044_M", "TA090612_M", "TA090991_M", "TA090655_M", "TA090136_M", "TA090799_M", "TA090739_M", "TA090742_M", "TA090738_M", "TA090743_M", "TA090705_M", "TA090815_M", 
                                                                                      "TA090836_M", "TA090823_M", "TA090807_M", "TA090852_M", "TA090844_M", "TA090828_M", "TA090385_M", "TA090100_M"))
}                                                                                     

# This function lets us isolate and save the values for the 'ID' column.

set.ID.levels <- function(x){
  dplyr::pull(x, PSID_ID)
}

# Apply the function, saving ID values for each chunk.

T2009M_levels_list <- lapply(T2009M_list, set.ID.levels)

# This for-loop lets us create and assign factor levels for IDs in each T2009M chunk (in tidy long format) as corresponding to all included variables.  

for(i in 1:32) {
  T2009M_tidy_list[[i]]$PSID_ID <- factor(T2009M_tidy_list[[i]]$PSID_ID, levels = T2009M_levels_list[[i]])
}

# This for-loop lets us create and assign factor levels for the binary values (0/1; 0 = IAC, 1 = FTL) stored in each variable. 

for(i in 1:32) {
  T2009M_tidy_list[[i]]$met_FTL_crt <- factor(T2009M_tidy_list[[i]]$met_FTL_crt)
}

# This function lets us create the heatmap itself.

create.heatmap <- function(x){
  ggplot(x, aes(x=PSID_ID, y=variable, fill=met_FTL_crt)) + geom_tile(color="white", size=0.5) +
    coord_equal() +
    labs(x="ID", y="Criteria") +
    theme_tufte(base_family="Helvetica") +
    theme(axis.ticks=element_blank()) + 
    theme(axis.text.x=element_text(angle = 45, hjust = 1)) + 
    scale_fill_discrete(name = "Met FTL Criteria", labels = c("No", "Yes"))
}

# This function lets us create the heatmap specifically with T2009M data. Now we just have to plug in the chunk/subsection # into the T09.heatmap function to view each subplot.

T09.heatmaps <- function(x){
  create.heatmap(T2009M_tidy_list[[x]])
}

# To create fewer plots to save overall and for the sake of easier data visualization, we will combine the subplots to create aggregated plots. 

TIAS2009_plot1 <- ggarrange(T09.heatmaps(1) + rremove("legend"), T09.heatmaps(2) + rremove("legend") + rremove("y.title"), T09.heatmaps(3) + rremove("legend") + rremove("y.title"), T09.heatmaps(4) + rremove("legend") + rremove("y.title"), 
                            T09.heatmaps(5) + rremove("legend"), T09.heatmaps(6) + rremove("legend") + rremove("y.title"), T09.heatmaps(7) + rremove("legend") + rremove("y.title"), T09.heatmaps(8) + rremove("legend") + rremove("y.title"), 
                            T09.heatmaps(9) + rremove("legend"), T09.heatmaps(10) + rremove("legend") + rremove("y.title"), T09.heatmaps(11) + rremove("legend") + rremove("y.title"), T09.heatmaps(12) + rremove("legend") + rremove("y.title"), 
                            T09.heatmaps(13) + rremove("legend"), T09.heatmaps(14) + rremove("legend") + rremove("y.title"), T09.heatmaps(15) + rremove("legend") + rremove("y.title"), T09.heatmaps(16) + rremove("legend") + rremove("y.title"), ncol = 4, nrow = 4) 

TIAS2009_plot1 # This command lets us view the first aggregated plot. To see clearly, you export manually as png with width 3500 height 2000. This is done by selecting plots>export>png and entering the correct values for width and height.

TIAS2009_plot2 <- ggarrange(T09.heatmaps(17) + rremove("legend"), T09.heatmaps(18) + rremove("legend") + rremove("y.title"), T09.heatmaps(19) + rremove("legend") + rremove("y.title"), T09.heatmaps(20) + rremove("legend") + rremove("y.title"), 
                            T09.heatmaps(21) + rremove("legend"), T09.heatmaps(22) + rremove("legend") + rremove("y.title"), T09.heatmaps(23) + rremove("legend") + rremove("y.title"), T09.heatmaps(24) + rremove("legend") + rremove("y.title"), 
                            T09.heatmaps(25) + rremove("legend"), T09.heatmaps(26) + rremove("legend") + rremove("y.title"), T09.heatmaps(27) + rremove("legend") + rremove("y.title"), T09.heatmaps(28) + rremove("legend") + rremove("y.title"), 
                            T09.heatmaps(29) + rremove("legend"), T09.heatmaps(30) + rremove("legend") + rremove("y.title"), T09.heatmaps(31) + rremove("legend") + rremove("y.title"), T09.heatmaps(32) + rremove("y.title"), ncol = 4, nrow = 4) 

TIAS2009_plot2 # This command lets us view the second aggreagted plot. To see clearly, you export manually as png with width 3500 height 2000. This is done by selecting plots>export>png and entering the correct values for width and height.

########### TIAS 2011 ###########

# This selects the data only from the wave of interest, which in this case is 2011.   

TIAS2011 <- TIAS[!is.na(TIAS$TAS11),]

# Now, we select the variables of interest that filter for participants who meet FTL criteria. See TIAS-C variable table for the names and details of these variables. 

TIAS2011$CAT <- with(TIAS2011, ifelse(
  TA110044 == 1 & TA110045 %in% c("1", "96") & TA110699 %in% c("5", "0") & TA110743 %in% c("5", "0") & TA110137 == 3 & TA110915 < 60 & 
  TA110829 == 0 & TA110832 == 0 & TA110833 == 0 & TA110793 %in% c("3", "5", "7", "0") & TA110931 == 0 & TA110952 == 0 & TA110939 == 0 & TA110923 == 0 & TA110968 == 0 & 
  TA110960 == 0 & TA110944 == 0 & TA110462 %in% c("1", "2", "3", "4", "7", "8", "97", "99") & TA110351 == 5 & TA110101 == 0, "FTL_11", "IAC_11"))

# Before subsetting the data to only include data for the wave of interest, we are adding IDs for each row in a new column ('ID') to consistently identify each row (participant).

T11_ID <- TIAS2011$ID

# Extract IDs of participants who have been identified as FTL for the 2011 wave 

FTL11_ID <- TIAS2011[TIAS2011$CAT == "FTL_11", "ID"]

# We will also add the unique individual identifier ID calculated using the method recommended by PSID researchers 

TIAS2011$PSID_ID <- (TIAS2011$ER30001 * 1000) + TIAS2011$ER30002

# Count the number of participants who have been identified as FTL for the 2011 wave 

length(FTL11_ID)

# View the number of FTL vs. IAC participants for the 2011 wave 

table(TIAS2011$CAT)

# Create a new variable ('CAT_11') for FTL vs. IAC vs. NA (no data) categorization in the 2011 wave 

TIAS$CAT_11 <- with(TIAS, ifelse(
  ID %in% FTL11_ID, "FTL_11", ifelse(
    ID %in% T11_ID, "IAC_11", "NA_11")))

# View the distribution for CAT_11

table(TIAS$CAT_11)

# Creating a subsetted dataframe including only FTL participants for the 2011 wave 

TIAS2011_FTL <- subset(TIAS2011, CAT == "FTL_11")

# Creating a subsetted dataframe including only IAC participants for the 2011 wave 

TIAS2011_IAC <- subset(TIAS2011, CAT == "IAC_11")

# View the IDs of participants who have been identified as FTL for the 2011 wave 

print(FTL11_ID)

print(TIAS2011_FTL$PSID_ID)

# Again, we are selecting these variables to create a dataset with only the relevant information for our TIAS criteria. 

TIAS2011 <- TIAS2011 %>% select(ID, PSID_ID, CAT, TA110044, TA110045, TA110699, TA110743, TA110137, TA110915, TA110829, TA110832, TA110833, TA110793, TA110931, 
                                TA110952, TA110939, TA110923, TA110968, TA110960, TA110944, TA110462, TA110351, TA110101) 

# Without altering the values of the variables in the original dataframe, we will create a new boolean variable (their original PSID name with _M at the end, M for match) for each variable (TRUE if matching FTL, FALSE if not)

TIAS2011$TA110044_M <- TIAS2011$TA110044 == 1 
TIAS2011$TA110045_M <- TIAS2011$TA110045 %in% c("1", "96")
TIAS2011$TA110699_M <- TIAS2011$TA110699 %in% c("5", "0")
TIAS2011$TA110743_M <- TIAS2011$TA110743 %in% c("5", "0")
TIAS2011$TA110137_M <- TIAS2011$TA110137 == 3
TIAS2011$TA110915_M <- TIAS2011$TA110915 < 60
TIAS2011$TA110829_M <- TIAS2011$TA110829 == 0 
TIAS2011$TA110832_M <- TIAS2011$TA110832 == 0 
TIAS2011$TA110833_M <- TIAS2011$TA110833 == 0 
TIAS2011$TA110793_M <- TIAS2011$TA110793 %in% c("3", "5", "7", "0")
TIAS2011$TA110931_M <- TIAS2011$TA110931 == 0
TIAS2011$TA110952_M <- TIAS2011$TA110952 == 0
TIAS2011$TA110939_M <- TIAS2011$TA110939 == 0
TIAS2011$TA110923_M <- TIAS2011$TA110923 == 0
TIAS2011$TA110968_M <- TIAS2011$TA110968 == 0
TIAS2011$TA110960_M <- TIAS2011$TA110960 == 0
TIAS2011$TA110944_M <- TIAS2011$TA110944 == 0
TIAS2011$TA110462_M <- TIAS2011$TA110462 %in% c("1", "2", "3", "4", "7", "8", "97", "99") 
TIAS2011$TA110351_M <- TIAS2011$TA110351 == 5 
TIAS2011$TA110101_M <- TIAS2011$TA110101 == 0 

# Here, we are creating a new dataframe with only the boolean variables.  

T2011M <- TIAS2011 %>% select(TA110044_M, TA110045_M, TA110699_M, TA110743_M, TA110137_M, TA110915_M, TA110829_M, TA110832_M, TA110833_M, TA110793_M, TA110931_M, 
                              TA110952_M, TA110939_M, TA110923_M, TA110968_M, TA110960_M, TA110944_M, TA110462_M, TA110351_M, TA110101_M, PSID_ID) 

# We then have to convert the boolean values (FALSE/TRUE) to binary values (0/1), as we will be working with these to create our heatmaps. 

cols <- sapply(T2011M, is.logical)
T2011M[,cols] <- lapply(T2011M[,cols], as.numeric)

# Find the number of columns in the new dataset of FTL variables for the 2011 wave. Important note: the last column is 'ID', so the column range to plug into the R function tidy.vars would be 1:(ncol-1).

ncol(T2011M) 

# Since we can't create a single plot with thousands of columns (including all participants), we have to divide the dataframe into 'chunks' to create subplots before creating aggregated plots. 

nrow(T2011M) # Find out how many rows (participants) are in T2011M. 

chunk <- 50 # The value of the chunk should be the no. of columns (participants) you would want for each heatmap subplot. 

n <- nrow(T2011M) # Save the number of rows in T2011M. 

r <- rep(1:ceiling(n/chunk), each=chunk)[1:n] # This command loops through the dataframe to create chunks (if the total # of rows was not cleanly divisible by 50, the # of rows included in the last chunk would be the remainder)

T2011M_list <- split(T2011M, r) # This command splits the dataframe into chunks as calculated above. 

length(T2011M_list) # Now find out how many chunks of 50 have been created from the total number of participants. i for functions would be 1:(# of chunks).

# This function lets us tidy the data into the long format. 

tidy.vars <- function(x){
  x %>% tidyr::gather(variable, met_FTL_crt, 1:20)
}

# Apply the function to T2011M chunks, tidying the data. 

T2011M_tidy_list <- lapply(T2011M_list, tidy.vars)

# This for-loop lets us create and assign factor levels for each variable in T2011M (excluding the last column 'ID'). 

for(i in 1:39) {
  T2011M_tidy_list[[i]]$variable <- factor(T2011M_tidy_list[[i]]$variable, levels = c("TA110044_M", "TA110045_M", "TA110699_M", "TA110743_M", "TA110137_M", "TA110915_M", "TA110829_M", "TA110832_M", "TA110833_M", "TA110793_M", "TA110931_M", 
                                                                                      "TA110952_M", "TA110939_M", "TA110923_M", "TA110968_M", "TA110960_M", "TA110944_M", "TA110462_M", "TA110351_M", "TA110101_M"))
}

# This function lets us isolate and save the values for the 'ID' column.
set.ID.levels <- function(x){
  dplyr::pull(x, PSID_ID)
}

# Apply the function, saving ID values for each chunk.

T2011M_levels_list <- lapply(T2011M_list, set.ID.levels)

# This for-loop lets us create and assign factor levels for IDs in each T2011M chunk (in tidy long format) as corresponding to all included variables.  

for(i in 1:39) {
  T2011M_tidy_list[[i]]$PSID_ID <- factor(T2011M_tidy_list[[i]]$PSID_ID, levels = T2011M_levels_list[[i]])
}

# This for-loop lets us create and assign factor levels for the binary values (0/1; 0 = IAC, 1 = FTL) stored in each variable. 

for(i in 1:39) {
  T2011M_tidy_list[[i]]$met_FTL_crt <- factor(T2011M_tidy_list[[i]]$met_FTL_crt)
}

# This function lets us create the heatmap itself. 

create.heatmap <- function(x){
  ggplot(x, aes(x=PSID_ID, y=variable, fill=met_FTL_crt)) + geom_tile(color="white", size=0.5) +
    coord_equal() +
    labs(x="ID", y="Criteria") +
    theme_tufte(base_family="Helvetica") +
    theme(axis.ticks=element_blank()) + 
    theme(axis.text.x=element_text(angle = 45, hjust = 1)) + 
    scale_fill_discrete(name = "Met FTL Criteria", labels = c("No", "Yes"))
}

# This function lets us create the heatmap specifically with T2011M data. Now we just have to plug in the chunk/subsection # into the T11.heatmap function to view each subplot. 

T11.heatmaps <- function(x){
  create.heatmap(T2011M_tidy_list[[x]])
}

# To create fewer plots to save overall and for the sake of easier data visualization, we will combine the subplots to create aggregated plots. 

TIAS2011_plot1 <- ggarrange(T11.heatmaps(1) + rremove("legend"), T11.heatmaps(2) + rremove("legend") + rremove("y.title"), T11.heatmaps(3) + rremove("legend") + rremove("y.title"), 
                            T11.heatmaps(4) + rremove("legend"), T11.heatmaps(5) + rremove("legend") + rremove("y.title"), T11.heatmaps(6) + rremove("legend") + rremove("y.title"), 
                            T11.heatmaps(7) + rremove("legend"), T11.heatmaps(8) + rremove("legend") + rremove("y.title"), T11.heatmaps(9) + rremove("legend") + rremove("y.title"), 
                            T11.heatmaps(10) + rremove("legend"), T11.heatmaps(11) + rremove("legend") + rremove("y.title"), T11.heatmaps(12) + rremove("legend") + rremove("y.title"), ncol = 3, nrow = 4) 

TIAS2011_plot1 # This command lets us view the first aggregated plot. To see clearly, you export manually as png with width 3000 height 2000. This is done by selecting plots>export>png and entering the correct values for width and height.

TIAS2011_plot2 <- ggarrange(T11.heatmaps(13) + rremove("legend"), T11.heatmaps(14) + rremove("legend") + rremove("y.title"), T11.heatmaps(15) + rremove("legend") + rremove("y.title"), 
                            T11.heatmaps(16) + rremove("legend"), T11.heatmaps(17) + rremove("legend") + rremove("y.title"), T11.heatmaps(18) + rremove("legend") + rremove("y.title"), 
                            T11.heatmaps(19) + rremove("legend"), T11.heatmaps(20) + rremove("legend") + rremove("y.title"), T11.heatmaps(21) + rremove("legend") + rremove("y.title"), ncol = 3, nrow = 3) 

TIAS2011_plot2 # This command lets us view the second aggregated plot. To see clearly, you export manually as png with width 3000 height 2000. This is done by selecting plots>export>png and entering the correct values for width and height.

TIAS2011_plot3 <- ggarrange(T11.heatmaps(22) + rremove("legend"), T11.heatmaps(23) + rremove("legend") + rremove("y.title"), T11.heatmaps(24) + rremove("legend") + rremove("y.title"), 
                            T11.heatmaps(25) + rremove("legend"), T11.heatmaps(26) + rremove("legend") + rremove("y.title"), T11.heatmaps(27) + rremove("legend") + rremove("y.title"), 
                            T11.heatmaps(28) + rremove("legend"), T11.heatmaps(29) + rremove("legend") + rremove("y.title"), T11.heatmaps(30) + rremove("legend") + rremove("y.title"), ncol = 3, nrow = 3) 

TIAS2011_plot3 # This command lets us view the third aggregated plot. To see clearly, you export manually as png with width 3000 height 2000. This is done by selecting plots>export>png and entering the correct values for width and height.

TIAS2011_plot4 <- ggarrange(T11.heatmaps(31) + rremove("legend"), T11.heatmaps(32) + rremove("legend") + rremove("y.title"), T11.heatmaps(33) + rremove("legend") + rremove("y.title"), 
                            T11.heatmaps(34) + rremove("legend"), T11.heatmaps(35) + rremove("legend") + rremove("y.title"), T11.heatmaps(36) + rremove("legend") + rremove("y.title"), 
                            T11.heatmaps(37) + rremove("legend"), T11.heatmaps(38) + rremove("legend") + rremove("y.title"), T11.heatmaps(39) + rremove("y.title"), ncol = 3, nrow = 3) 

TIAS2011_plot4 # This command lets us view the fourth aggregated plot. To see clearly, you export manually as png with width 3000 height 2000. This is done by selecting plots>export>png and entering the correct values for width and height.

########### TIAS 2013 ###########

# This selects the data only from the wave of interest, which in this case is 2013.   

TIAS2013 <- TIAS[!is.na(TIAS$TAS13),]

# Now, we select the variables of interest that filter for participants who meet FTL criteria. See TIAS-C variable table for the names and details of these variables.   

TIAS2013$CAT <- with(TIAS2013, ifelse(
  TA130043 == 1 & TA130044 %in% c("1", "96") & TA130719 %in% c("5", "0") & TA131225 %in% c("1", "2", "3", "4", "5", "6", "7", "96") & TA130763 %in% c("5", "0") & TA130136 == 3 & TA130948 < 60 & 
  TA130852 == 0 & TA130855 == 0 & TA130851 == 0 & TA130856 == 0 & TA130813 %in% c("3", "5", "7", "0") & TA130964 == 0 & TA130982 == 0 & TA130972 %in% c("0", "1", "2", "3") & TA130956 == 0 & TA131001 == 0 & 
  TA130993 == 0 & TA130977 == 0 & TA130482 %in% c("1", "2", "3", "4", "7", "8", "97", "99") & TA130100 == 0, "FTL_13", ifelse(
      TA130043 == 1 & TA130044 %in% c("1", "96") & TA130719 %in% c("5", "0") & TA131225 %in% c("1", "2", "3", "4", "5", "6", "7") & TA130763 %in% c("5", "0") & TA130136 == 3 & TA130948 < 60 & 
      TA130852 == 0 & TA130855 == 0 & TA130851 == 0 & TA130856 == 0 & TA130813 %in% c("3", "5", "7", "0") & TA130964 == 0 & TA130982 == 0 & TA130972 %in% c("0", "1", "2", "3") & TA130956 == 0 & TA131001 == 0 & 
      TA130993 == 0 & TA130977 == 0 & TA130482 == 0 & TA130350 == 1 & TA130100 == 0, "FTL_13", "IAC_13")))

# Before subsetting the data to only include data for the wave of interest, we are adding IDs for each row in a new column ('ID') to consistently identify each row (participant).

T13_ID <- TIAS2013$PSID_ID

# We will also add the unique individual identifier ID calculated using the method recommended by PSID researchers 

TIAS2013$PSID_ID <- (TIAS2013$ER30001 * 1000) + TIAS2013$ER30002

# Extract IDs of participants who have been identified as FTL for the 2013 wave 

FTL13_ID <- TIAS2013[TIAS2013$CAT == "FTL_13", "PSID_ID"]

# Count the number of participants who have been identified as FTL for the 2013 wave 

nrow(FTL13_ID)

# View the number of FTL vs. IAC participants for the 2013 wave 

table(TIAS2013$CAT)

FTL13_ID_VEC <- unname(unlist(FTL13_ID))

# Create a new variable ('CAT_13') for FTL vs. IAC vs. NA (no data) categorization in the 2013 wave 

TIAS$CAT_13 <- with(TIAS, ifelse(
  ID %in% FTL13_ID, "FTL_13", ifelse(
    ID %in% T13_ID, "IAC_13", "NA_13")))

# View the distribution for CAT_13

table(TIAS$CAT_13)

# View the IDs of participants who have been identified as FTL for the 2013 wave 

print(FTL13_ID)

print(TIAS2013_FTL$PSID_ID)

# Again, we are selecting these variables to create a dataset with only the relevant information for our TIAS criteria.

TIAS2013 <- TIAS2013 %>% select(ID, PSID_ID, CAT, TA130043, TA130044, TA130719, TA131225, TA130763, TA130136, TA130948, TA130852, TA130855, TA130851, TA130856, TA130813, TA130964, 
                                TA130982, TA130972, TA130956, TA131001, TA130993, TA130977, TA130482, TA130350, TA130100) 

# Without altering the values of the variables in the original dataframe, we will create a new boolean variable (their original PSID name with _M at the end, M for match) for each variable (TRUE if matching FTL, FALSE if not).

TIAS2013$TA130043_M <- TIAS2013$TA130043 == 1 
TIAS2013$TA130044_M <- TIAS2013$TA130044 %in% c("1", "96")
TIAS2013$TA130719_M <- TIAS2013$TA130719 %in% c("5", "0")
TIAS2013$TA131225_M <- TIAS2013$TA131225 %in% c("1", "2", "3", "4", "5", "6", "7", "96")
TIAS2013$TA130763_M <- TIAS2013$TA130763 %in% c("5", "0")
TIAS2013$TA130136_M <- TIAS2013$TA130136 == 3
TIAS2013$TA130948_M <- TIAS2013$TA130948 < 60
TIAS2013$TA130852_M <- TIAS2013$TA130852 == 0 
TIAS2013$TA130855_M <- TIAS2013$TA130855 == 0 
TIAS2013$TA130851_M <- TIAS2013$TA130851 == 0 
TIAS2013$TA130856_M <- TIAS2013$TA130856 == 0 
TIAS2013$TA130813_M <- TIAS2013$TA130813 %in% c("3", "5", "7", "0")
TIAS2013$TA130964_M <- TIAS2013$TA130964 == 0
TIAS2013$TA130982_M <- TIAS2013$TA130982 == 0
TIAS2013$TA130972_M <- TIAS2013$TA130972 %in% c("0", "1", "2", "3")
TIAS2013$TA130956_M <- TIAS2013$TA130956 == 0
TIAS2013$TA131001_M <- TIAS2013$TA131001 == 0
TIAS2013$TA130993_M <- TIAS2013$TA130993 == 0
TIAS2013$TA130977_M <- TIAS2013$TA130977 == 0
TIAS2013$TA130482_M <- TIAS2013$TA130482 %in% c("1", "2", "3", "4", "7", "8", "97", "99")
TIAS2013$TA130350_M <- TIAS2013$TA130350 == 5 
TIAS2013$TA130100_M <- TIAS2013$TA130100 == 0 

# Here, we are creating a new dataframe with only the boolean variables.  

T2013M <- TIAS2013 %>% select(TA130043_M, TA130044_M, TA130719_M, TA131225_M, TA130763_M, TA130136_M, TA130948_M, TA130852_M, TA130855_M, TA130851_M, TA130856_M, TA130813_M, TA130964_M, 
                              TA130982_M, TA130972_M, TA130956_M, TA131001_M, TA130993_M, TA130977_M, TA130482_M, TA130100_M, PSID_ID) 

# We then have to convert the boolean values (FALSE/TRUE) to binary values (0/1), as we will be working with these to create our heatmaps.

cols <- sapply(T2013M, is.logical)
T2013M[,cols] <- lapply(T2013M[,cols], as.numeric)

# Find the number of columns in the new dataset of FTL variables for the 2013 wave. Important note: the last column is 'ID', so the column range to plug into the R function tidy.vars would be 1:(ncol-1).

ncol(T2013M)  

# Since we can't create a single plot with thousands of columns (including all participants), we have to divide the dataframe into 'chunks' to create subplots before creating aggregated plots. 

nrow(T2013M) # Find out how many rows (participants) are in T2013M.  

chunk <- 49 # The value of the chunk should be the no. of columns (participants) you would want for each heatmap subplot. 

n <- nrow(T2013M) # Save the number of rows in T2013M. 

r <- rep(1:ceiling(n/chunk), each=chunk)[1:n] # This command loops through the dataframe to create chunks (if the total # of rows was not cleanly divisible by 50, the # of rows included in the last chunk would be the remainder).

T2013M_list <- split(T2013M, r) # This command splits the dataframe into chunks as calculated above. 

length(T2013M_list) # Now find out how many chunks of 50 have been created from the total number of participants. i for functions would be 1:(# of chunks).

# This function lets us tidy the data into the long format. 

tidy.vars <- function(x){
  x %>% tidyr::gather(variable, met_FTL_crt, 1:21)
}

# Apply the function to T2013M chunks, tidying the data. 

T2013M_tidy_list <- lapply(T2013M_list, tidy.vars)

# This for-loop lets us create and assign factor levels for each variable in T2013M (excluding the last column 'ID'). 

for(i in 1:37) {
  T2013M_tidy_list[[i]]$variable <- factor(T2013M_tidy_list[[i]]$variable, levels = c("TA130043_M", "TA130044_M", "TA130719_M", "TA131225_M", "TA130763_M", "TA130136_M", "TA130948_M", "TA130852_M", "TA130855_M", "TA130851_M", "TA130856_M", "TA130813_M", "TA130964_M", 
                                                                                      "TA130982_M", "TA130972_M", "TA130956_M", "TA131001_M", "TA130993_M", "TA130977_M", "TA130482_M", "TA130100_M"))
}

# This function lets us isolate and save the values for the 'ID' column.

set.ID.levels <- function(x){
  dplyr::pull(x, PSID_ID)
}

# Apply the function, saving ID values for each chunk.

T2013M_levels_list <- lapply(T2013M_list, set.ID.levels)

# This for-loop lets us create and assign factor levels for IDs in each T2013M chunk (in tidy long format) as corresponding to all included variables.  

for(i in 1:37) {
  T2013M_tidy_list[[i]]$PSID_ID <- factor(T2013M_tidy_list[[i]]$PSID_ID, levels = T2013M_levels_list[[i]])
}

# This for-loop lets us create and assign factor levels for the binary values (0/1; 0 = IAC, 1 = FTL) stored in each variable. 

for(i in 1:37) {
  T2013M_tidy_list[[i]]$met_FTL_crt <- factor(T2013M_tidy_list[[i]]$met_FTL_crt)
}

# This function lets us create the heatmap itself.

create.heatmap <- function(x){
  ggplot(x, aes(x=PSID_ID, y=variable, fill=met_FTL_crt)) + geom_tile(color="white", size=0.5) +
    coord_equal() +
    labs(x="ID", y="Criteria") +
    theme_tufte(base_family="Helvetica") +
    theme(axis.ticks=element_blank()) + 
    theme(axis.text.x=element_text(angle = 45, hjust = 1)) + 
    scale_fill_discrete(name = "Met FTL Criteria", labels = c("No", "Yes"))
}

# This function lets us create the heatmap specifically with T2013M data. Now we just have to plug in the chunk/subsection # into the T13.heatmap function to view each subplot.

T13.heatmaps <- function(x){
  create.heatmap(T2013M_tidy_list[[x]])
}

# To create fewer plots to save overall and for the sake of easier data visualization, we will combine the subplots to create aggregated plots. 

TIAS2013_plot1 <- ggarrange(T13.heatmaps(1) + rremove("legend"), T13.heatmaps(2) + rremove("legend") + rremove("y.title"), T13.heatmaps(3) + rremove("legend") + rremove("y.title"), T13.heatmaps(4) + rremove("legend") + rremove("y.title"), 
                            T13.heatmaps(5) + rremove("legend"), T13.heatmaps(6) + rremove("legend") + rremove("y.title"), T13.heatmaps(7) + rremove("legend") + rremove("y.title"), T13.heatmaps(8) + rremove("legend") + rremove("y.title"), 
                            T13.heatmaps(9) + rremove("legend"), T13.heatmaps(10) + rremove("legend") + rremove("y.title"), T13.heatmaps(11) + rremove("legend") + rremove("y.title"), T13.heatmaps(12) + rremove("legend") + rremove("y.title"), 
                            T13.heatmaps(13) + rremove("legend"), T13.heatmaps(14) + rremove("legend") + rremove("y.title"), T13.heatmaps(15) + rremove("legend") + rremove("y.title"), T13.heatmaps(16) + rremove("legend") + rremove("y.title"), ncol = 4, nrow = 4) 

TIAS2013_plot1 # This command lets us view the first aggregated plot. To see clearly, you export manually as png with width 3500 height 2000. This is done by selecting plots>export>png and entering the correct values for width and height.

TIAS2013_plot2 <- ggarrange(T13.heatmaps(17) + rremove("legend"), T13.heatmaps(18) + rremove("legend") + rremove("y.title"), T13.heatmaps(19) + rremove("legend") + rremove("y.title"), T13.heatmaps(20) + rremove("legend") + rremove("y.title"), 
                            T13.heatmaps(21) + rremove("legend"), T13.heatmaps(22) + rremove("legend") + rremove("y.title"), T13.heatmaps(23) + rremove("legend") + rremove("y.title"), T13.heatmaps(24) + rremove("legend") + rremove("y.title"), 
                            T13.heatmaps(25) + rremove("legend"), T13.heatmaps(26) + rremove("legend") + rremove("y.title"), T13.heatmaps(27) + rremove("legend") + rremove("y.title"), T13.heatmaps(28) + rremove("legend") + rremove("y.title"), ncol = 4, nrow = 3)

TIAS2013_plot2 # This command lets us view the second aggregated plot. To see clearly, you export manually as png with width 3500 height 1500. This is done by selecting plots>export>png and entering the correct values for width and height.

TIAS2013_plot3 <- ggarrange(T13.heatmaps(29) + rremove("legend"), T13.heatmaps(30) + rremove("legend") + rremove("y.title"), T13.heatmaps(31) + rremove("legend") + rremove("y.title"),
                            T13.heatmaps(32) + rremove("legend"), T13.heatmaps(33) + rremove("legend") + rremove("y.title"), T13.heatmaps(34) + rremove("y.title") + rremove("legend"), 
                            T13.heatmaps(35) + rremove("legend"), T13.heatmaps(36) + rremove("legend") + rremove("y.title"), T13.heatmaps(37) + rremove("y.title"), ncol = 3, nrow = 3) 

TIAS2013_plot3 # This command lets us view the third aggregated plot. To see clearly, you export manually as png with width 3500 height 1500. This is done by selecting plots>export>png and entering the correct values for width and height.

########### TIAS 2015 ###########

# The below code clears your R environment (found on the right side of the console and includes data tables and values). 
# Clearing the environment after you make heatmaps for each wave is important because otherwise the environment gets crowded – it would be hard to find data and values you need from the 2015 wave). 

rm(list=ls())

# Import the TIAS data. This dataset was downloaded from the PSID website and includes values for all waves. The raw csv file can be found on github. 

TIAS <- read.csv("https://raw.githubusercontent.com/carolinelee78/FTL/main/data/raw/PSID/TIAS/TIAS.csv")

# Before subsetting the data to only include data for the wave of interest, we are adding IDs for each row in a new column ('ID') to consistently identify each row (participant).

TIAS$ID <- seq.int(nrow(TIAS)) 

# This selects the data only from the wave of interest, which in this case is 2015.    

TIAS2015 <- TIAS[!is.na(TIAS$TAS15),]

# Now, we select the variables of interest that filter for participants who meet FTL criteria. See TIAS-C variable table for the names and details of these variables.  

TIAS2015$CAT <- with(TIAS2015, ifelse(
   TA150043 == 1 & TA150044 %in% c("1", "96") & TA150731 %in% c("5", "0") & TA150776 %in% c("5", "0") & TA150128 == 3 & TA150970 < 60 & 
   TA150869 == 0 & TA150872 == 0 & TA150873 == 0 & TA150826 %in% c("3", "5", "7", "0") & TA150986 == 0 & TA151007 == 0 & TA150994 == 0 & TA150978 == 0 & TA151023 == 0 & 
   TA151015 == 0 & TA150999 == 0 & TA150491 %in% c("1", "2", "3", "4", "7", "8", "97", "99") & TA150352 == 5 & TA150092 == 0, "FTL_15", "IAC_15"))

# Before subsetting the data to only include data for the wave of interest, we are adding IDs for each row in a new column ('ID') to consistently identify each row (participant).

T15_ID <- TIAS2015$ID

# Extract IDs of participants who have been identified as FTL for the 2015 wave 

FTL15_ID <- TIAS2015[TIAS2015$CAT == "FTL_15", "ID"]

# We will also add the unique individual identifier ID calculated using the method recommended by PSID researchers 

TIAS2015$PSID_ID <- (TIAS2015$ER30001 * 1000) + TIAS2015$ER30002

# Count the number of participants who have been identified as FTL for the 2015 wave 

length(FTL15_ID)

# View the number of FTL vs. IAC participants for the 2015 wave 

table(TIAS2015$CAT)

# Create a new variable ('CAT_15') for FTL vs. IAC vs. NA (no data) categorization in the 2015 wave 

TIAS$CAT_15 <- with(TIAS, ifelse(
  ID %in% FTL15_ID, "FTL_15", ifelse(
    ID %in% T15_ID, "IAC_15", "NA_15")))

# View the distribution for CAT_15

table(TIAS$CAT_15)

# Creating a subsetted dataframe including only FTL participants for the 2015 wave 

TIAS2015_FTL <- subset(TIAS2015, CAT == "FTL_15")

# Creating a subsetted dataframe including only IAC participants for the 2015 wave 

TIAS2015_IAC <- subset(TIAS2015, CAT == "IAC_15")

# View the IDs of participants who have been identified as FTL for the 2015 wave 

print(FTL15_ID)

print(TIAS2015_FTL$PSID_ID)

# Again, we are selecting these variables to create a dataset with only the relevant information for our TIAS criteria.

TIAS2015 <- TIAS2015 %>% select(ID, PSID_ID, CAT, TA150043, TA150044, TA150731, TA150776, TA150128, TA150970, TA150869, TA150872, TA150873, TA150826, TA150986, TA151007, TA150994, TA150978, TA151023, TA151015, TA150999, TA150491, TA150352, TA150092)

# Without altering the values of the variables in the original dataframe, we will create a new boolean variable (their original PSID name with _M at the end, M for match) for each variable (TRUE if matching FTL, FALSE if not).

TIAS2015$TA150043_M <- TIAS2015$TA150043 == 1 
TIAS2015$TA150044_M <- TIAS2015$TA150044 %in% c("1", "96")
TIAS2015$TA150731_M <- TIAS2015$TA150731 %in% c("5", "0")
TIAS2015$TA150776_M <- TIAS2015$TA150776 %in% c("5", "0")
TIAS2015$TA150128_M <- TIAS2015$TA150128 == 3
TIAS2015$TA150970_M <- TIAS2015$TA150970 < 60
TIAS2015$TA150869_M <- TIAS2015$TA150869 == 0 
TIAS2015$TA150872_M <- TIAS2015$TA150872 == 0 
TIAS2015$TA150873_M <- TIAS2015$TA150873 == 0 
TIAS2015$TA150826_M <- TIAS2015$TA150826 %in% c("3", "5", "7", "0")
TIAS2015$TA150986_M <- TIAS2015$TA150986 == 0
TIAS2015$TA151007_M <- TIAS2015$TA151007 == 0
TIAS2015$TA150994_M <- TIAS2015$TA150994 == 0
TIAS2015$TA150978_M <- TIAS2015$TA150978 == 0
TIAS2015$TA151023_M <- TIAS2015$TA151023 == 0
TIAS2015$TA151015_M <- TIAS2015$TA151015 == 0
TIAS2015$TA150999_M <- TIAS2015$TA150999 == 0
TIAS2015$TA150491_M <- TIAS2015$TA150491 %in% c("1", "2", "3", "4", "7", "8", "97", "99") 
TIAS2015$TA150352_M <- TIAS2015$TA150352 == 5
TIAS2015$TA150092_M <- TIAS2015$TA150092 == 0

# Here, we are creating a new dataframe with only the boolean variables. 
  
T2015M <- TIAS2015 %>% select(TA150043_M, TA150044_M, TA150731_M, TA150776_M, TA150128_M, TA150970_M, TA150869_M, TA150872_M, TA150873_M,
                              TA150826_M, TA150986_M, TA151007_M, TA150994_M, TA150978_M, TA151023_M, TA151015_M, TA150999_M, TA150491_M, TA150352_M, TA150092_M, PSID_ID)

# We then have to convert the boolean values (FALSE/TRUE) to binary values (0/1), as we will be working with these to create our heatmaps.

cols <- sapply(T2015M, is.logical)
T2015M[,cols] <- lapply(T2015M[,cols], as.numeric)

# Find the number of columns in the new dataset of FTL variables for the 2015 wave. Important note: the last column is 'ID', so the column range to plug into the R function tidy.vars would be 1:(ncol-1).

ncol(T2015M)  

# Since we can't create a single plot with thousands of columns (including all participants), we have to divide the dataframe into 'chunks' to create subplots before creating aggregated plots. 

nrow(T2015M) # Find out how many rows (participants) are in T2015M.   

chunk <- 50 # The value of the chunk should be the no. of columns (participants) you would want for each heatmap subplot. 

n <- nrow(T2015M) # Save the number of rows in T2015M.

r <- rep(1:ceiling(n/chunk), each=chunk)[1:n] # This command loops through the dataframe to create chunks (if the total # of rows was not cleanly divisible by 50, the # of rows included in the last chunk would be the remainder).

T2015M_list <- split(T2015M, r) # This command splits the dataframe into chunks as calculated above. 

length(T2015M_list) # Now find out how many chunks of 50 have been created from the total number of participants. i for functions would be 1:(# of chunks).  

# This function lets us tidy the data into the long format. 

tidy.vars <- function(x){
  x %>% tidyr::gather(variable, met_FTL_crt, 1:20)
}

# Apply the function to T2015M chunks, tidying the data. 

T2015M_tidy_list <- lapply(T2015M_list, tidy.vars)

# This for-loop lets us create and assign factor levels for each variable in T2015M (excluding the last column 'ID'). 

for(i in 1:33) {
  T2015M_tidy_list[[i]]$variable <- factor(T2015M_tidy_list[[i]]$variable, levels = c("TA150043_M", "TA150044_M", "TA150731_M", "TA150776_M", "TA150128_M", "TA150970_M", "TA150869_M", "TA150872_M", "TA150873_M", "TA150826_M", 
                                                                                      "TA150986_M", "TA151007_M", "TA150994_M", "TA150978_M", "TA151023_M", "TA151015_M", "TA150999_M", "TA150491_M", "TA150352_M", "TA150092_M"))
} 

# This function lets us isolate and save the values for the 'ID' column.

set.ID.levels <- function(x){
  dplyr::pull(x, PSID_ID)
}

# Apply the function, saving ID values for each chunk.

T2015M_levels_list <- lapply(T2015M_list, set.ID.levels)

# This for-loop lets us create and assign factor levels for IDs in each T2015M chunk (in tidy long format) as corresponding to all included variables. 

for(i in 1:33) {
  T2015M_tidy_list[[i]]$PSID_ID <- factor(T2015M_tidy_list[[i]]$PSID_ID, levels = T2015M_levels_list[[i]])
}

# This for-loop lets us create and assign factor levels for the binary values (0/1; 0 = IAC, 1 = FTL) stored in each variable. 

for(i in 1:33) {
  T2015M_tidy_list[[i]]$met_FTL_crt <- factor(T2015M_tidy_list[[i]]$met_FTL_crt)
}

# This function lets us create the heatmap itself.

create.heatmap <- function(x){
  ggplot(x, aes(x=PSID_ID, y=variable, fill=met_FTL_crt)) + geom_tile(color="white", size=0.5) +
    coord_equal() +
    labs(x="ID", y="Criteria") +
    theme_tufte(base_family="Helvetica") +
    theme(axis.ticks=element_blank()) + 
    theme(axis.text.x=element_text(angle = 45, hjust = 1)) + 
    scale_fill_discrete(name = "Met FTL Criteria", labels = c("No", "Yes"))
}

# This function lets us create the heatmap specifically with T2015M data. Now we just have to plug in the chunk/subsection # into the T15.heatmap function to view each subplot.

T15.heatmaps <- function(x){
  create.heatmap(T2015M_tidy_list[[x]])
}

# To create fewer plots to save overall and for the sake of easier data visualization, we will combine the subplots to create aggregated plots. 

TIAS2015_plot1 <- ggarrange(T15.heatmaps(1) + rremove("legend"), T15.heatmaps(2) + rremove("legend") + rremove("y.title"), T15.heatmaps(3) + rremove("legend") + rremove("y.title"), 
                            T15.heatmaps(4) + rremove("legend"), T15.heatmaps(5) + rremove("legend") + rremove("y.title"), T15.heatmaps(6) + rremove("legend") + rremove("y.title"), 
                            T15.heatmaps(7) + rremove("legend"), T15.heatmaps(8) + rremove("legend") + rremove("y.title"), T15.heatmaps(9) + rremove("legend") + rremove("y.title"), 
                            T15.heatmaps(10) + rremove("legend"), T15.heatmaps(11) + rremove("legend") + rremove("y.title"), T15.heatmaps(12) + rremove("legend") + rremove("y.title"), ncol = 3, nrow = 4) 
                            
TIAS2015_plot1 # This command lets us view the first aggregated plot. To see clearly, you export manually as png with width 3000 height 2000. This is done by selecting plots>export>png and entering the correct values for width and height.

TIAS2015_plot2 <- ggarrange(T15.heatmaps(13) + rremove("legend"), T15.heatmaps(14) + rremove("legend") + rremove("y.title"), T15.heatmaps(15) + rremove("y.title") + rremove("legend"), 
                            T15.heatmaps(16) + rremove("legend"), T15.heatmaps(17) + rremove("legend") + rremove("y.title"), T15.heatmaps(18) + rremove("legend") + rremove("y.title"), 
                            T15.heatmaps(19) + rremove("legend"), T15.heatmaps(20) + rremove("legend") + rremove("y.title"), T15.heatmaps(21) + rremove("legend") + rremove("y.title"), 
                            T15.heatmaps(22) + rremove("legend"), T15.heatmaps(23) + rremove("legend") + rremove("y.title"), T15.heatmaps(24) + rremove("legend") + rremove("y.title"), ncol = 3, nrow = 4)

TIAS2015_plot2 # This command lets us view the second aggregated plot. To see clearly, you export manually as png with width 3000 height 2000. This is done by selecting plots>export>png and entering the correct values for width and height. 

TIAS2015_plot3 <- ggarrange(T15.heatmaps(25) + rremove("legend"), T15.heatmaps(26) + rremove("legend") + rremove("y.title"), T15.heatmaps(27) + rremove("legend") + rremove("y.title"),
                            T15.heatmaps(28) + rremove("legend"), T15.heatmaps(29) + rremove("legend") + rremove("y.title"), T15.heatmaps(30) + rremove("y.title") + rremove("legend"), 
                            T15.heatmaps(31) + rremove("legend"), T15.heatmaps(32) + rremove("legend") + rremove("y.title"), T15.heatmaps(33) + rremove("y.title"), ncol = 3, nrow = 3) 

TIAS2015_plot3 # This command lets us view the third aggregated plot. To see clearly, you export manually as png with width 3000 height 1500. This is done by selecting plots>export>png and entering the correct values for width and height.  

########### TIAS 2017 ###########

# The below code clears your R environment (found on the right side of the console and includes data tables and values). 
# Clearing the environment after you make heatmaps for each wave is important because otherwise the environment gets crowded – it would be hard to find data and values you need from the 2017 wave). 

rm(list=ls())

# Import the TIAS data. This dataset was downloaded from the PSID website and includes values for all waves. The raw csv file can be found on github. 

TIAS <- read.csv("https://raw.githubusercontent.com/carolinelee78/FTL/main/data/raw/PSID/TIAS/TIAS.csv")

# Before subsetting the data to only include data for the wave of interest, we are adding IDs for each row in a new column ('ID') to consistently identify each row (participant).

TIAS$ID <- seq.int(nrow(TIAS)) 

# This selects the data only from the wave of interest, which in this case is 2017. 

TIAS2017 <- TIAS[!is.na(TIAS$TAS17),]

# Now, we select the variables of interest that filter for participants who meet FTL criteria. See TIAS-C variable table for the names and details of these variables. 

TIAS2017$CAT <- with(TIAS2017, ifelse(
  TA170058 == 1 & TA170059 %in% c("1", "96") & TA170790 %in% c("5", "0") & TA170416 %in% c("5", "0") & TA170183 == 3 & TA171827 < 60 & 
  TA170909 == 0 & TA170912 == 0 & TA170913 == 0 & TA170866 %in% c("3", "5", "7", "0") & TA171869 == 0 & TA171885 == 0 & TA171893 == 0 & 
  TA171861 == 0 & TA171877 == 0 & TA171893 == 0 & TA171840 == 0 & TA170389 == 1 & TA170176 == 0, "FTL_17", "IAC_17"))

# Before subsetting the data to only include data for the wave of interest, we are adding IDs for each row in a new column ('ID') to consistently identify each row (participant).

T17_ID <- TIAS2017$ID

# We will also add the unique individual identifier ID calculated using the method recommended by PSID researchers 

TIAS2017$PSID_ID <- (TIAS2017$ER30001 * 1000) + TIAS2017$ER30002

# Extract IDs of participants who have been identified as FTL for the 2017 wave 

FTL17_ID <- TIAS2017[TIAS2017$CAT == "FTL_17", "ID"]

# Count the number of participants who have been identified as FTL for the 2017 wave 

length(FTL17_ID)

# View the number of FTL vs. IAC participants for the 2017 wave 

table(TIAS2017$CAT)

# Create a new variable ('CAT_17') for FTL vs. IAC vs. NA (no data) categorization in the 2017 wave 

TIAS$CAT_17 <- with(TIAS, ifelse(
  ID %in% FTL17_ID, "FTL_17", ifelse(
    ID %in% T17_ID, "IAC_17", "NA_17")))

# View the distribution for CAT_17

table(TIAS$CAT_17)

# Creating a subsetted dataframe including only FTL participants for the 2017 wave 

TIAS2017_FTL <- subset(TIAS2017, CAT == "FTL_17")

# Creating a subsetted dataframe including only IAC participants for the 2017 wave 

TIAS2017_IAC <- subset(TIAS2017, CAT == "IAC_17")

# View the IDs of participants who have been identified as FTL for the 2017 wave 

print(FTL17_ID)

print(TIAS2017_FTL$PSID_ID)

# Again, we are selecting these variables to create a dataset with only the relevant information for our TIAS criteria. 

TIAS2017 <- TIAS2017 %>% select(ID, PSID_ID, CAT, TA170058, TA170059, TA170790, TA170416, TA170183, TA171827, TA170909, TA170912, TA170913, TA170866, TA171869, TA171885, TA171893, TA171861, TA171877, TA171840, TA170389, TA170176)

# Without altering the values of the variables in the original dataframe, we will create a new boolean variable (their original PSID name with _M at the end, M for match) for each variable (TRUE if matching FTL, FALSE if not).

TIAS2017$TA170058_M <- TIAS2017$TA170058 == 1 
TIAS2017$TA170059_M <- TIAS2017$TA170059 %in% c("1", "96")
TIAS2017$TA170790_M <- TIAS2017$TA170790 %in% c("5", "0")
TIAS2017$TA170416_M <- TIAS2017$TA170416 %in% c("5", "0")
TIAS2017$TA170183_M <- TIAS2017$TA170183 == 3
TIAS2017$TA171827_M <- TIAS2017$TA171827 < 60
TIAS2017$TA170909_M <- TIAS2017$TA170909 == 0 
TIAS2017$TA170912_M <- TIAS2017$TA170912 == 0 
TIAS2017$TA170913_M <- TIAS2017$TA170913 == 0 
TIAS2017$TA170866_M <- TIAS2017$TA170866 %in% c("3", "5", "7", "0")
TIAS2017$TA171869_M <- TIAS2017$TA171869 == 0
TIAS2017$TA171885_M <- TIAS2017$TA171885 == 0
TIAS2017$TA171893_M <- TIAS2017$TA171893 == 0
TIAS2017$TA171861_M <- TIAS2017$TA171861 == 0
TIAS2017$TA171877_M <- TIAS2017$TA171877 == 0
TIAS2017$TA171840_M <- TIAS2017$TA171840 == 0
TIAS2017$TA170389_M <- TIAS2017$TA170389 == 5
TIAS2017$TA170176_M <- TIAS2017$TA170176 == 0

# Here, we are creating a new dataframe with only the boolean variables.  
  
T2017M <- TIAS2017 %>% select(TA170058_M, TA170059_M, TA170790_M, TA170416_M, TA170183_M, TA171827_M, TA170909_M, TA170912_M, TA170913_M, TA170866_M, TA171869_M, TA171885_M, TA171893_M, TA171861_M, TA171877_M, TA171840_M, TA170389_M, TA170176_M, PSID_ID)

# We then have to convert the boolean values (FALSE/TRUE) to binary values (0/1), as we will be working with these to create our heatmaps. 

cols <- sapply(T2017M, is.logical)
T2017M[,cols] <- lapply(T2017M[,cols], as.numeric)

# Find the number of columns in the new dataset of FTL variables for the 2017 wave. Important note: the last column is 'ID', so the column range to plug into the R function tidy.vars would be 1:(ncol-1).

ncol(T2017M)  

# Since we can't create a single plot with thousands of columns (including all participants), we have to divide the dataframe into 'chunks' to create subplots before creating aggregated plots. 

nrow(T2017M) # Find out how many rows (participants) are in T2017M. 

chunk <- 50 # The value of the chunk should be the no. of columns (participants) you would want for each heatmap subplot. 

n <- nrow(T2017M) # Save the number of rows in T2017M. 

r <- rep(1:ceiling(n/chunk), each=chunk)[1:n] # This command loops through the dataframe to create chunks (if the total # of rows was not cleanly divisible by 50, the # of rows included in the last chunk would be the remainder).

T2017M_list <- split(T2017M, r) # This command splits the dataframe into chunks as calculated above.

length(T2017M_list) # Now find out how many chunks of 50 have been created from the total number of participants. i for functions would be 1:(# of chunks).

# This function lets us tidy the data into the long format. 

tidy.vars <- function(x){
  x %>% tidyr::gather(variable, met_FTL_crt, 1:18)
}

# Apply the function to T2017M chunks, tidying the data. 

T2017M_tidy_list <- lapply(T2017M_list, tidy.vars)

# This for-loop lets us create and assign factor levels for each variable in T2017M (excluding the last column 'ID'). 

for(i in 1:51) {
  T2017M_tidy_list[[i]]$variable <- factor(T2017M_tidy_list[[i]]$variable, levels = c("TA170058_M", "TA170059_M", "TA170790_M", "TA170416_M", "TA170183_M", "TA171827_M", "TA170909_M", "TA170912_M", "TA170913_M", "TA170866_M", "TA171869_M", 
                                                                                      "TA171885_M", "TA171893_M", "TA171861_M", "TA171877_M", "TA171840_M", "TA170389_M", "TA170176_M"))
} 

# This function lets us isolate and save the values for the 'ID' column.

set.ID.levels <- function(x){
  dplyr::pull(x, PSID_ID)
}

# Apply the function, saving ID values for each chunk.

T2017M_levels_list <- lapply(T2017M_list, set.ID.levels)

# This for-loop lets us create and assign factor levels for IDs in each T2017M chunk (in tidy long format) as corresponding to all included variables. 

for(i in 1:51) {
  T2017M_tidy_list[[i]]$PSID_ID <- factor(T2017M_tidy_list[[i]]$PSID_ID, levels = T2017M_levels_list[[i]])
}

# This for-loop lets us create and assign factor levels for the binary values (0/1; 0 = IAC, 1 = FTL) stored in each variable. 

for(i in 1:51) {
  T2017M_tidy_list[[i]]$met_FTL_crt <- factor(T2017M_tidy_list[[i]]$met_FTL_crt)
}

# This function lets us create the heatmap itself.

create.heatmap <- function(x){
  ggplot(x, aes(x=PSID_ID, y=variable, fill=met_FTL_crt)) + geom_tile(color="white", size=0.5) +
    coord_equal() +
    labs(x="ID", y="Criteria") +
    theme_tufte(base_family="Helvetica") +
    theme(axis.ticks=element_blank()) + 
    theme(axis.text.x=element_text(angle = 45, hjust = 1)) + 
    scale_fill_discrete(name = "Met FTL Criteria", labels = c("No", "Yes"))
}

# This function lets us create the heatmap specifically with T2017M data. Now we just have to plug in the chunk/subsection # into the T17.heatmap function to view each subplot.

T17.heatmaps <- function(x){
  create.heatmap(T2017M_tidy_list[[x]])
}

# To create fewer plots to save overall and for the sake of easier data visualization, we will combine the subplots to create aggregated plots. 

TIAS2017_plot1 <- ggarrange(T17.heatmaps(1) + rremove("legend"), T17.heatmaps(2) + rremove("legend") + rremove("y.title"), T17.heatmaps(3) + rremove("legend") + rremove("y.title"), 
                            T17.heatmaps(4) + rremove("legend"), T17.heatmaps(5) + rremove("legend") + rremove("y.title"), T17.heatmaps(6) + rremove("legend") + rremove("y.title"), 
                            T17.heatmaps(7) + rremove("legend"), T17.heatmaps(8) + rremove("legend") + rremove("y.title"), T17.heatmaps(9) + rremove("legend") + rremove("y.title"), 
                            T17.heatmaps(10) + rremove("legend"), T17.heatmaps(11) + rremove("legend") + rremove("y.title"), T17.heatmaps(12) + rremove("legend") + rremove("y.title"), ncol = 3, nrow = 4) 
                            
TIAS2017_plot1 # This command lets us view the first aggregated plot. To see clearly, you export manually as png with width 3000 height 2000. This is done by selecting plots>export>png and entering the correct values for width and height. 

TIAS2017_plot2 <- ggarrange(T17.heatmaps(13) + rremove("legend"), T17.heatmaps(14) + rremove("legend") + rremove("y.title"), T17.heatmaps(15) + rremove("y.title") + rremove("legend"), 
                            T17.heatmaps(16) + rremove("legend"), T17.heatmaps(17) + rremove("legend") + rremove("y.title"), T17.heatmaps(18) + rremove("legend") + rremove("y.title"), 
                            T17.heatmaps(19) + rremove("legend"), T17.heatmaps(20) + rremove("legend") + rremove("y.title"), T17.heatmaps(21) + rremove("legend") + rremove("y.title"), 
                            T17.heatmaps(22) + rremove("legend"), T17.heatmaps(23) + rremove("legend") + rremove("y.title"), T17.heatmaps(24) + rremove("legend") + rremove("y.title"), ncol = 3, nrow = 4)

TIAS2017_plot2 # This command lets us view the second aggregated plot. To see clearly, you export manually as png with width 3000 height 2000. This is done by selecting plots>export>png and entering the correct values for width and height.  

TIAS2017_plot3 <- ggarrange(T17.heatmaps(25) + rremove("legend"), T17.heatmaps(26) + rremove("legend") + rremove("y.title"), T17.heatmaps(27) + rremove("legend") + rremove("y.title"),
                            T17.heatmaps(28) + rremove("legend"), T17.heatmaps(29) + rremove("legend") + rremove("y.title"), T17.heatmaps(30) + rremove("y.title") + rremove("legend"), 
                            T17.heatmaps(31) + rremove("legend"), T17.heatmaps(32) + rremove("legend") + rremove("y.title"), T17.heatmaps(33) + rremove("legend") + rremove("y.title"), ncol = 3, nrow = 3) 

TIAS2017_plot3 # This command lets us view the third aggregated plot. To see clearly, you export manually as png with width 3000 height 1500. This is done by selecting plots>export>png and entering the correct values for width and height.  

TIAS2017_plot4 <- ggarrange(T17.heatmaps(34) + rremove("legend"), T17.heatmaps(35) + rremove("legend") + rremove("y.title"), T17.heatmaps(36) + rremove("legend") + rremove("y.title"),
                            T17.heatmaps(37) + rremove("legend"), T17.heatmaps(38) + rremove("legend") + rremove("y.title"), T17.heatmaps(39) + rremove("y.title") + rremove("legend"), 
                            T17.heatmaps(40) + rremove("legend"), T17.heatmaps(41) + rremove("legend") + rremove("y.title"), T17.heatmaps(42) + rremove("legend") + rremove("y.title"), ncol = 3, nrow = 3) 

TIAS2017_plot4 # This command lets us view the fourth aggregated plot. To see clearly, you export manually as png with width 3000 height 1500. This is done by selecting plots>export>png and entering the correct values for width and height.   

TIAS2017_plot5 <- ggarrange(T17.heatmaps(43) + rremove("legend"), T17.heatmaps(44) + rremove("legend") + rremove("y.title"), T17.heatmaps(45) + rremove("legend") + rremove("y.title"),
                            T17.heatmaps(46) + rremove("legend"), T17.heatmaps(47) + rremove("legend") + rremove("y.title"), T17.heatmaps(48) + rremove("y.title") + rremove("legend"), 
                            T17.heatmaps(49) + rremove("legend"), T17.heatmaps(50) + rremove("legend") + rremove("y.title"), T17.heatmaps(51) + rremove("y.title"), ncol = 3, nrow = 3) 

TIAS2017_plot5 # This command lets us view the fifth aggregated plot. To see clearly, you export manually as png with width 3000 height 1500. This is done by selecting plots>export>png and entering the correct values for width and height.

########### ISOLATING TIAS FTL PARTICIPANTS IN WAVES 2005-2017 ###########

# The below code clears your R environment (found on the right side of the console and includes data tables and values). 
# Clearing the environment after you make heatmaps for each wave is important because otherwise the environment gets crowded – it would be hard to find data and values you need from each wave). 

rm(list=ls())

# Import the TIAS data. This dataset was downloaded from the PSID website and includes values for all waves. The raw csv file can be found on github. 

TIAS <- read.csv("https://raw.githubusercontent.com/carolinelee78/FTL/main/data/raw/PSID/TIAS/TIAS.csv")

# Before subsetting the data to only include data for the wave of interest, we are adding IDs for each row in a new column ('ID') to consistently identify each row (participant).

TIAS$ID <- seq.int(nrow(TIAS)) 

# We will also add the unique individual identifier ID calculated using the method recommended by PSID researchers 

TIAS$PSID_ID <- (TIAS$ER30001 * 1000) + TIAS$ER30002

# The below code applies the chosen criteria to the dataset and isolates the participants that are FTL from the 2005 wave. You will be shown the number of FTL participants,
# the number of IAC participants, and the participant IDs of all individuals who are FTL. 

# This selects the data only from the wave of interest, which in this case is 2005. 

TIAS2005 <- TIAS[!is.na(TIAS$TAS05),]

# Now, we select the variables of interest that filter for participants who meet FTL criteria. See TIAS-C variable table for the names and details of these variables. 

TIAS2005$CAT <- with(TIAS2005, ifelse(
  TA050042 == 1 & TA050043 %in% c("1", "96") & TA050595 %in% c("5", "0") & TA050631 %in% c("5", "0") & TA050127 == 3 & TA050769 < 60 & 
    TA050712 == 0 & TA050715 == 0 & TA050716 == 0 & TA050678 %in% c("3", "5", "7", "0") & TA050785 == 0 & TA050809 == 0 & TA050793 == 0 & TA050777 == 0 & TA050825 == 0 & 
    TA050817 == 0 & TA050798 == 0 & TA050394 %in% c("1", "2", "3", "4", "7", "8", "97", "99") & TA050371 == 5 & TA050091 == 0, "FTL_05", "IAC_05"))  

# Before subsetting the data to only include data for the wave of interest, we are adding IDs for each row in a new column ('ID') to consistently identify each row (participant).

T05_ID <- TIAS2005$ID

# We will also add the unique individual identifier ID calculated using the method recommended by PSID researchers 

TIAS2005$PSID_ID <- (TIAS2005$ER30001 * 1000) + TIAS2005$ER30002

# Extract IDs of participants who have been identified as FTL for the 2005 wave 

FTL05_ID <- TIAS2005[TIAS2005$CAT == "FTL_05", "ID"]

# Count the number of participants who have been identified as FTL for the 2005 wave 

length(FTL05_ID)

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

# View the IDs of participants who have been identified as FTL for the 2005 wave 

print(FTL05_ID)

print(TIAS2005_FTL$PSID_ID)

# The below code applies the chosen criteria to the dataset and isolates the participants that are FTL from the 2007 wave. You will be shown the number of FTL participants,
# the number of IAC participants, and the participant IDs of all individuals who are FTL.

# This selects the data only from the wave of interest, which in this case is 2007. 

TIAS2007 <- TIAS[!is.na(TIAS$TAS07),]

# Now, we select the variables of interest that filter for participants who meet FTL criteria. See TIAS-C variable table for the names and details of these variables. 

TIAS2007$CAT <- with(TIAS2007, ifelse(
  TA070042 == 1 & TA070043 %in% c("1", "96") & TA070570 %in% c("5", "0") & TA070602 %in% c("5", "0") & TA070127 == 3 & TA070740 < 60 & 
    TA070683 == 0 & TA070686 == 0 & TA070687 == 0 & TA070649 %in% c("3", "5", "7", "0") & TA070756 == 0 & TA070777 == 0 & TA070764 == 0 & TA070748 == 0 & TA070793 == 0 & 
    TA070785 == 0 & TA070769 == 0 & TA070368 %in% c("1", "2", "3", "4", "7", "8", "97", "99") & TA070344 == 5 & TA070091 == 0, "FTL_07", "IAC_07"))  

# Before subsetting the data to only include data for the wave of interest, we are adding IDs for each row in a new column ('ID') to consistently identify each row (participant).

T07_ID <- TIAS2007$ID

# We will also add the unique individual identifier ID calculated using the method recommended by PSID researchers 

TIAS2007$PSID_ID <- (TIAS2007$ER30001 * 1000) + TIAS2007$ER30002

# Extract IDs of participants who have been identified as FTL for the 2007 wave 

FTL07_ID <- TIAS2007[TIAS2007$CAT == "FTL_07", "ID"]

# Count the number of participants who have been identified as FTL for the 2007 wave 

length(FTL07_ID)

# View the number of FTL vs. IAC participants for the 2007 wave 

table(TIAS2007$CAT)

# Create a new variable ('CAT_07') for FTL vs. IAC vs. NA (no data) categorization in the 2007 wave 

TIAS$CAT_07 <- with(TIAS, ifelse(
  ID %in% FTL07_ID, "FTL_07", ifelse(
    ID %in% T07_ID, "IAC_07", "NA_07")))

# View the distribution for CAT_07

table(TIAS$CAT_07)

# Creating a subsetted dataframe including only FTL participants for the 2007 wave 

TIAS2007_FTL <- subset(TIAS2007, CAT == "FTL_07")

# Creating a subsetted dataframe including only IAC participants for the 2007 wave 

TIAS2007_IAC <- subset(TIAS2007, CAT == "IAC_07")

# View the IDs of participants who have been identified as FTL for the 2007 wave 

print(FTL07_ID)

print(TIAS2007_FTL$PSID_ID)

# This selects the data only from the wave of interest, which in this case is 2009. 

TIAS2009 <- TIAS[!is.na(TIAS$TAS09),]

# Now, we select the variables of interest that filter for participants who meet FTL criteria. See TIAS-C variable table for the names and details of these variables. 

TIAS2009$CAT <- with(TIAS2009, ifelse(
  TA090043 == 1 & TA090044 %in% c("1", "96") & TA090612 %in% c("5", "0") & TA090655 %in% c("5", "0") & TA090136 == 3 & TA090799 < 60 & 
    TA090739 == 0 & TA090742 == 0 & TA090743 == 0 & TA090705 %in% c("3", "5", "7", "0") & TA090815 == 0 & TA090836 == 0 & TA090823 == 0 & TA090807 == 0 & TA090852 == 0 & 
    TA090844 == 0 & TA090828 == 0 & TA090385 %in% c("1", "2", "3", "4", "7", "8", "97", "99") & TA090361 == 5 & TA090100 == 0, "FTL_09", "IAC_07"))  

# Before subsetting the data to only include data for the wave of interest, we are adding IDs for each row in a new column ('ID') to consistently identify each row (participant).

T09_ID <- TIAS2009$ID

# We will also add the unique individual identifier ID calculated using the method recommended by PSID researchers 

TIAS2009$PSID_ID <- (TIAS2009$ER30001 * 1000) + TIAS2009$ER30002

# Extract IDs of participants who have been identified as FTL for the 2009 wave 

FTL09_ID <- TIAS2009[TIAS2009$CAT == "FTL_09", "ID"]

# Count the number of participants who have been identified as FTL for the 2009 wave 

length(FTL09_ID)

# View the number of FTL vs. IAC participants for the 2009 wave 

table(TIAS2009$CAT)

# Create a new variable ('CAT_09') for FTL vs. IAC vs. NA (no data) categorization in the 2009 wave 

TIAS$CAT_09 <- with(TIAS, ifelse(
  ID %in% FTL09_ID, "FTL_09", ifelse(
    ID %in% T09_ID, "IAC_09", "NA_09")))

# View the distribution for CAT_09

table(TIAS$CAT_09)

# Creating a subsetted dataframe including only FTL participants for the 2009 wave 

TIAS2009_FTL <- subset(TIAS2009, CAT == "FTL_09")

# Creating a subsetted dataframe including only IAC participants for the 2009 wave 

TIAS2009_IAC <- subset(TIAS2009, CAT == "IAC_09")

# View the IDs of participants who have been identified as FTL for the 2009 wave 

print(FTL09_ID)

print(TIAS2009_FTL$PSID_ID)

# The below code applies the chosen criteria to the dataset and isolates the participants that are FTL from the 2011 wave. You will be shown the number of FTL participants,
# the number of IAC participants, and the participant IDs of all individuals who are FTL.

# This selects the data only from the wave of interest, which in this case is 2011. 

TIAS2011 <- TIAS[!is.na(TIAS$TAS11),]

# Now, we select the variables of interest that filter for participants who meet FTL criteria. See TIAS-C variable table for the names and details of these variables. 

TIAS2011$CAT <- with(TIAS2011, ifelse(
  TA110044 == 1 & TA110045 %in% c("1", "96") & TA110699 %in% c("5", "0") & TA110743 %in% c("5", "0") & TA110137 == 3 & TA110915 < 60 & 
    TA110829 == 0 & TA110832 == 0 & TA110833 == 0 & TA110793 %in% c("3", "5", "7", "0") & TA110931 == 0 & TA110952 == 0 & TA110939 == 0 & TA110923 == 0 & TA110968 == 0 & 
    TA110960 == 0 & TA110944 == 0 & TA110462 %in% c("1", "2", "3", "4", "7", "8", "97", "99") & TA110351 == 5 & TA110101 == 0, "FTL_11", "IAC_11"))

# Before subsetting the data to only include data for the wave of interest, we are adding IDs for each row in a new column ('ID') to consistently identify each row (participant).

T11_ID <- TIAS2011$ID

# Extract IDs of participants who have been identified as FTL for the 2011 wave 

FTL11_ID <- TIAS2011[TIAS2011$CAT == "FTL_11", "ID"]

# We will also add the unique individual identifier ID calculated using the method recommended by PSID researchers 

TIAS2011$PSID_ID <- (TIAS2011$ER30001 * 1000) + TIAS2011$ER30002

# Count the number of participants who have been identified as FTL for the 2011 wave 

length(FTL11_ID)

# View the number of FTL vs. IAC participants for the 2011 wave 

table(TIAS2011$CAT)

# Create a new variable ('CAT_11') for FTL vs. IAC vs. NA (no data) categorization in the 2011 wave 

TIAS$CAT_11 <- with(TIAS, ifelse(
  ID %in% FTL11_ID, "FTL_11", ifelse(
    ID %in% T11_ID, "IAC_11", "NA_11")))

# View the distribution for CAT_11

table(TIAS$CAT_11)

# Creating a subsetted dataframe including only FTL participants for the 2011 wave 

TIAS2011_FTL <- subset(TIAS2011, CAT == "FTL_11")

# Creating a subsetted dataframe including only IAC participants for the 2011 wave 

TIAS2011_IAC <- subset(TIAS2011, CAT == "IAC_11")

# View the IDs of participants who have been identified as FTL for the 2011 wave 

print(FTL11_ID)

print(TIAS2011_FTL$PSID_ID)

# The below code applies the chosen criteria to the dataset and isolates the participants that are FTL from the 2013 wave. You will be shown the number of FTL participants,
# the number of IAC participants, and the participant IDs of all individuals who are FTL.

# This selects the data only from the wave of interest, which in this case is 2013. 

TIAS2013 <- TIAS[!is.na(TIAS$TAS13),]

# Now, we select the variables of interest that filter for participants who meet FTL criteria. See TIAS-C variable table for the names and details of these variables. 

TIAS2013$CAT <- with(TIAS2013, ifelse(
  TA130043 == 1 & TA130044 %in% c("1", "96") & TA130719 %in% c("5", "0") & TA130763 %in% c("5", "0") & TA130136 == 3 & TA130948 < 60 & 
  TA130852 == 0 & TA130855 == 0 & TA130856 == 0 & TA130813 %in% c("3", "5", "7", "0") & TA130964 == 0 & TA130982 == 0 & TA130972 == 0 & 
  TA130956 == 0 & TA131001 == 0 & TA130993 == 0 & TA130977 == 0 & TA130482 %in% c("1", "2", "3", "4", "7", "8", "97", "99") & TA130350 == 5 & TA130100 == 0, "FTL_13", "IAC_13"))

# Before subsetting the data to only include data for the wave of interest, we are adding IDs for each row in a new column ('ID') to consistently identify each row (participant).

T13_ID <- TIAS2013$ID

# We will also add the unique individual identifier ID calculated using the method recommended by PSID researchers 

TIAS2013$PSID_ID <- (TIAS2013$ER30001 * 1000) + TIAS2013$ER30002

# Extract IDs of participants who have been identified as FTL for the 2013 wave 

FTL13_ID <- TIAS2013[TIAS2013$CAT == "FTL_13", "ID"]

# Count the number of participants who have been identified as FTL for the 2013 wave 

length(FTL13_ID)

# View the number of FTL vs. IAC participants for the 2013 wave 

table(TIAS2013$CAT)

# Create a new variable ('CAT_13') for FTL vs. IAC vs. NA (no data) categorization in the 2013 wave 

TIAS$CAT_13 <- with(TIAS, ifelse(
  ID %in% FTL13_ID, "FTL_13", ifelse(
    ID %in% T13_ID, "IAC_13", "NA_13")))

# View the distribution for CAT_13

table(TIAS$CAT_13)

# Creating a subsetted dataframe including only FTL participants for the 2013 wave 

TIAS2013_FTL <- subset(TIAS2013, CAT == "FTL_13")

# Creating a subsetted dataframe including only IAC participants for the 2013 wave 

TIAS2013_IAC <- subset(TIAS2013, CAT == "IAC_13")

# View the IDs of participants who have been identified as FTL for the 2013 wave 

print(FTL13_ID)

print(TIAS2013_FTL$PSID_ID)

# The below code applies the chosen criteria to the dataset and isolates the participants that are FTL from the 2015 wave. You will be shown the number of FTL participants,
# the number of IAC participants, and the participant IDs of all individuals who are FTL.

# This selects the data only from the wave of interest, which in this case is 2015. 

TIAS2015 <- TIAS[!is.na(TIAS$TAS15),]

# Now, we select the variables of interest that filter for participants who meet FTL criteria. See TIAS-C variable table for the names and details of these variables. 

TIAS2015$CAT <- with(TIAS2015, ifelse(
  TA150043 == 1 & TA150044 %in% c("1", "96") & TA150731 %in% c("5", "0") & TA150776 %in% c("5", "0") & TA150128 == 3 & TA150970 < 60 & 
    TA150869 == 0 & TA150872 == 0 & TA150873 == 0 & TA150826 %in% c("3", "5", "7", "0") & TA150986 == 0 & TA151007 == 0 & TA150994 == 0 & TA150978 == 0 & TA151023 == 0 & 
    TA151015 == 0 & TA150999 == 0 & TA150491 %in% c("1", "2", "3", "4", "7", "8", "97", "99") & TA150352 == 5 & TA150092 == 0, "FTL_15", "IAC_15"))

# Before subsetting the data to only include data for the wave of interest, we are adding IDs for each row in a new column ('ID') to consistently identify each row (participant).

T15_ID <- TIAS2015$ID

# Extract IDs of participants who have been identified as FTL for the 2015 wave 

FTL15_ID <- TIAS2015[TIAS2015$CAT == "FTL_15", "ID"]

# We will also add the unique individual identifier ID calculated using the method recommended by PSID researchers 

TIAS2015$PSID_ID <- (TIAS2015$ER30001 * 1000) + TIAS2015$ER30002

# Count the number of participants who have been identified as FTL for the 2015 wave 

length(FTL15_ID)

# View the number of FTL vs. IAC participants for the 2015 wave 

table(TIAS2015$CAT)

# Create a new variable ('CAT_15') for FTL vs. IAC vs. NA (no data) categorization in the 2015 wave 

TIAS$CAT_15 <- with(TIAS, ifelse(
  ID %in% FTL15_ID, "FTL_15", ifelse(
    ID %in% T15_ID, "IAC_15", "NA_15")))

# View the distribution for CAT_15

table(TIAS$CAT_15)

# Creating a subsetted dataframe including only FTL participants for the 2015 wave 

TIAS2015_FTL <- subset(TIAS2015, CAT == "FTL_15")

# Creating a subsetted dataframe including only IAC participants for the 2015 wave 

TIAS2015_IAC <- subset(TIAS2015, CAT == "IAC_15")

# View the IDs of participants who have been identified as FTL for the 2015 wave 

print(FTL15_ID)

print(TIAS2015_FTL$PSID_ID)

# The below code applies the chosen criteria to the dataset and isolates the participants that are FTL from the 2017 wave. You will be shown the number of FTL participants,
# the number of IAC participants, and the participant IDs of all individuals who are FTL.

# This selects the data only from the wave of interest, which in this case is 2017. 

TIAS2017 <- TIAS[!is.na(TIAS$TAS17),]

# Now, we select the variables of interest that filter for participants who meet FTL criteria. See TIAS-C variable table for the names and details of these variables. 

TIAS2017$CAT <- with(TIAS2017, ifelse(
  TA170058 == 1 & TA170059 %in% c("1", "96") & TA170790 %in% c("5", "0") & TA170416 %in% c("5", "0") & TA170183 == 3 & TA171827 < 60 & 
    TA170909 == 0 & TA170912 == 0 & TA170913 == 0 & TA170866 %in% c("3", "5", "7", "0") & TA171869 == 0 & TA171885 == 0 & TA171893 == 0 & 
    TA171861 == 0 & TA171877 == 0 & TA171893 == 0 & TA171840 == 0 & TA170389 == 1 & TA170176 == 0, "FTL_17", "IAC_17"))

# Before subsetting the data to only include data for the wave of interest, we are adding IDs for each row in a new column ('ID') to consistently identify each row (participant).

T17_ID <- TIAS2017$ID

# We will also add the unique individual identifier ID calculated using the method recommended by PSID researchers 

TIAS2017$PSID_ID <- (TIAS2017$ER30001 * 1000) + TIAS2017$ER30002

# Extract IDs of participants who have been identified as FTL for the 2017 wave 

FTL17_ID <- TIAS2017[TIAS2017$CAT == "FTL_17", "ID"]

# Count the number of participants who have been identified as FTL for the 2017 wave 

length(FTL17_ID)

# View the number of FTL vs. IAC participants for the 2017 wave 

table(TIAS2017$CAT)

# Create a new variable ('CAT_17') for FTL vs. IAC vs. NA (no data) categorization in the 2017 wave 

TIAS$CAT_17 <- with(TIAS, ifelse(
  ID %in% FTL17_ID, "FTL_17", ifelse(
    ID %in% T17_ID, "IAC_17", "NA_17")))

# View the distribution for CAT_17

table(TIAS$CAT_17)

# Creating a subsetted dataframe including only FTL participants for the 2017 wave 

TIAS2017_FTL <- subset(TIAS2017, CAT == "FTL_17")

# Creating a subsetted dataframe including only IAC participants for the 2017 wave 

TIAS2017_IAC <- subset(TIAS2017, CAT == "IAC_17")

# View the IDs of participants who have been identified as FTL for the 2017 wave 

print(FTL17_ID)

print(TIAS2017_FTL$PSID_ID)

# The following code allows you to view the final TIAS-C FTL/IAC dataset for all waves (2005-2017).

TIAS_FTL <- TIAS %>% select(ID, PSID_ID, CAT_05, CAT_07, CAT_09, CAT_11, CAT_13, CAT_15, CAT_17, TA050042, TA050043, TA050595, TA050631, TA050127, TA050769, TA050712, TA050715, TA050716, TA050678, 
                            TA050785, TA050809, TA050793, TA050777, TA050825, TA050817, TA050798, TA050394, TA050371, TA050091, TA070042, TA070043, TA070570, TA070602, TA070127, TA070740, TA070683, 
                            TA070686, TA070687, TA070649, TA070756, TA070777, TA070764, TA070748, TA070793, TA070785, TA070769, TA070368, TA070344, TA070091, TA090043, TA090044, TA090612, TA090655, 
                            TA090136, TA090799, TA090739, TA090742, TA090743, TA090705, TA090815, TA090836, TA090823, TA090807, TA090852, TA090844, TA090828, TA090385, TA090361, TA090100, TA110044,
                            TA110045, TA110699, TA110743, TA110137, TA110915, TA110829, TA110832, TA110833, TA110793, TA110931, TA110952, TA110939, TA110923, TA110968, TA110960, TA110944, TA110462, 
                            TA110351, TA110101, TA130043, TA130044, TA130719, TA130763, TA130136, TA130948, TA130852, TA130855, TA130856, TA130813, TA130964, TA130982, TA130972, TA130956, TA131001,  
                            TA130993, TA130977, TA130482, TA130350, TA130100, TA150043, TA150044, TA150731, TA150776, TA150128, TA150970, TA150869, TA150872, TA150873, TA150826, TA150986, TA151007, 
                            TA150994, TA150978, TA151023, TA151015, TA150999, TA150491, TA150352, TA150092, TA170058, TA170059, TA170790, TA170416, TA170183, TA171827, TA170909, TA170912, TA170913, 
                            TA170866, TA171869, TA171885, TA171893, TA171861, TA171877, TA171840, TA170389, TA170176)


# Now, you can export final TIAS-C FTL/IAC dataset to a csv file.

write.csv2(TIAS_FTL, "TIASC_FTL.csv")   

########### Create FTL/IAC Heatmaps for all waves ########### 

# This next chunk of code allows you to view whether each participant was FTL or IAC for all waves 2005-2017. Get started by importing FTL IDs.

FTL_ID <- TIAS_FTL[,1:9]

# First, run the below lines to designate each participant ID as FTL or IAC for each wave, starting with 2005... 

FTL_ID$CAT_05 <- with(FTL_ID, ifelse(
  CAT_05 == "FTL_05", 0, ifelse(
   CAT_05 == "IAC_05", 1, 2)))

#...then 2007...

FTL_ID$CAT_07 <- with(FTL_ID, ifelse(
  CAT_07 == "FTL_07", 0, ifelse(
    CAT_07 == "IAC_07", 1, 2)))

#...then 2009, and etc...

FTL_ID$CAT_09 <- with(FTL_ID, ifelse(
  CAT_09 == "FTL_09", 0, ifelse(
    CAT_09 == "IAC_09", 1, 2)))

FTL_ID$CAT_11 <- with(FTL_ID, ifelse(
  CAT_11 == "FTL_11", 0, ifelse(
    CAT_11 == "IAC_11", 1, 2)))

FTL_ID$CAT_13 <- with(FTL_ID, ifelse(
  CAT_13 == "FTL_13", 0, ifelse(
    CAT_13 == "IAC_13", 1, 2)))

FTL_ID$CAT_15 <- with(FTL_ID, ifelse(
  CAT_15 == "FTL_15", 0, ifelse(
    CAT_15 == "IAC_15", 1, 2)))

FTL_ID$CAT_17 <- with(FTL_ID, ifelse(
  CAT_17 == "FTL_17", 0, ifelse(
    CAT_17 == "IAC_17", 1, 2)))

#These next few lines separate the data into chunks so that the heatmaps are easy to view and read. 

ncol(FTL_ID)  

nrow(FTL_ID)  
chunk <- 100 
n <- nrow(FTL_ID)
r <- rep(1:ceiling(n/chunk), each=chunk)[1:n]

FTLID_list <- split(FTL_ID, r)
length(FTLID_list)  

tidy.vars <- function(x){
  x %>% tidyr::gather(variable, met_FTL_crt, 3:9)
}

FTLID_tidy_list <- lapply(FTLID_list, tidy.vars)

for(i in 1:41) {
  FTLID_tidy_list[[i]]$variable <- factor(FTLID_tidy_list[[i]]$variable, levels = c("CAT_05", "CAT_07", "CAT_09", "CAT_11", "CAT_13", "CAT_15", "CAT_17"))
} 

set.ID.levels <- function(x){
  dplyr::pull(x, PSID_ID)
}

FTLID_levels_list <- lapply(FTLID_list, set.ID.levels)

for(i in 1:41) {
  FTLID_tidy_list[[i]]$PSID_ID <- factor(FTLID_tidy_list[[i]]$PSID_ID, levels = FTLID_levels_list[[i]])
}

for(i in 1:41) {
  FTLID_tidy_list[[i]]$met_FTL_crt <- factor(FTLID_tidy_list[[i]]$met_FTL_crt)
}

# This dictates the aesthetics of our heatmaps, such as the colors, axis titles, and sizes of the squares. 
legend_title <- "Met FTL Criteria"
create.heatmap <- function(x){
  ggplot(x, aes(x=variable, y=PSID_ID, fill=met_FTL_crt)) + geom_tile(color="white", size=0.5) +
    coord_equal() +
    labs(x="Waves", y="ID") +
    theme_tufte(base_family="Helvetica") +
    theme(axis.ticks=element_blank()) + 
    theme(axis.text.x=element_text(angle = 45, hjust = 1)) + 
    scale_fill_manual(legend_title, values = c("green", "orange", "grey"), labels = c("Yes", "No", "NA"))
}

FTL.heatmaps <- function(x){
  create.heatmap(FTLID_tidy_list[[x]])
}

# We have to make 4 different plots because there are a large amount of participants. For the first plot, we make heat maps for chunks 1-10. 

FTLID_plot1 <- ggarrange(FTL.heatmaps(1) + rremove("legend"), FTL.heatmaps(2) + rremove("legend") + rremove("y.title"), FTL.heatmaps(3) + rremove("legend") + rremove("y.title"), FTL.heatmaps(4) + rremove("legend") + rremove("y.title"), 
                         FTL.heatmaps(5) + rremove("legend") + rremove("y.title"), FTL.heatmaps(6) + rremove("legend") + rremove("y.title"), FTL.heatmaps(7) + rremove("legend") + rremove("y.title"), FTL.heatmaps(8) + rremove("legend") + rremove("y.title"), 
                         FTL.heatmaps(9) + rremove("legend") + rremove("y.title"), FTL.heatmaps(10) + rremove("y.title"), nrow = 1, ncol = 10)

FTLID_plot1 # This allows us to view the first aggregated plot. For best clarity, export manually as png with width 3000 height 2000.  

# For the second plot, we make heat maps for chunks 11-20. 
FTLID_plot2 <- ggarrange(FTL.heatmaps(11) + rremove("legend"), FTL.heatmaps(12) + rremove("legend") + rremove("y.title"), FTL.heatmaps(13) + rremove("legend") + rremove("y.title"), FTL.heatmaps(14) + rremove("legend") + rremove("y.title"), 
                         FTL.heatmaps(15) + rremove("legend") + rremove("y.title"), FTL.heatmaps(16) + rremove("legend") + rremove("y.title"), FTL.heatmaps(17) + rremove("legend") + rremove("y.title"), FTL.heatmaps(18) + rremove("legend") + rremove("y.title"), 
                         FTL.heatmaps(19) + rremove("legend") + rremove("y.title"), FTL.heatmaps(20) + rremove("y.title"), nrow = 1, ncol = 10)

FTLID_plot2 # This allows us to view the second aggregated plot. For best clarity, export manually as png with width 3000 height 2000.   

# For the third plot, we make heat maps for chunks 21-30. 
FTLID_plot3 <- ggarrange(FTL.heatmaps(21) + rremove("legend"), FTL.heatmaps(22) + rremove("legend") + rremove("y.title"), FTL.heatmaps(23) + rremove("legend") + rremove("y.title"), FTL.heatmaps(24) + rremove("legend") + rremove("y.title"), 
                         FTL.heatmaps(25) + rremove("legend") + rremove("y.title"), FTL.heatmaps(26) + rremove("legend") + rremove("y.title"), FTL.heatmaps(27) + rremove("legend") + rremove("y.title"), FTL.heatmaps(28) + rremove("legend") + rremove("y.title"), 
                         FTL.heatmaps(29) + rremove("legend") + rremove("y.title"), FTL.heatmaps(30) + rremove("y.title"), nrow = 1, ncol = 10)

FTLID_plot3 # This allows us to view the third aggregated plot. For best clarity, export manually as png with width 3000 height 2000.   

# For the fourth plot, we make heat maps for chunks 31-41. 
FTLID_plot4 <- ggarrange(FTL.heatmaps(31) + rremove("legend"), FTL.heatmaps(32) + rremove("legend") + rremove("y.title"), FTL.heatmaps(33) + rremove("legend") + rremove("y.title"), FTL.heatmaps(34) + rremove("legend") + rremove("y.title"), 
                         FTL.heatmaps(35) + rremove("legend") + rremove("y.title"), FTL.heatmaps(36) + rremove("legend") + rremove("y.title"), FTL.heatmaps(37) + rremove("legend") + rremove("y.title"), FTL.heatmaps(38) + rremove("legend") + rremove("y.title"), 
                         FTL.heatmaps(39) + rremove("legend") + rremove("y.title"), FTL.heatmaps(40) + rremove("legend") + rremove("y.title"), FTL.heatmaps(41) + rremove("y.title"), nrow = 1, ncol = 11)

FTLID_plot4 # This allows us to view the fourth and final aggregated plot. For best clarity, export manually as png with width 3000 height 2000.  








