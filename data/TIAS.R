#####################
# load libraries
# set wd
# clear global .envir
#####################

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
display.brewer.all()

# set working directory 

setwd("/Users/kdlee/Desktop/Lebowitz_Lab/FTL")

# import data 

TIAS <- read.csv("https://raw.githubusercontent.com/carolinelee78/FTL/main/data/raw/PSID/TIAS/TIAS.csv")
TIAS$ID <- seq.int(nrow(TIAS))

#####################
# TIAS 2005
#####################

# subset for wave 

TIAS2005 <- TIAS[!is.na(TIAS$TAS05),]
nrow(TIAS2005)

# manually paste in variable names and corresponding ftl criteria values from var table 

TIAS2005$CAT <- with(TIAS2005, ifelse(
    TA050042 == 1 & TA050043 %in% c("1", "96") & TA050595 %in% c("5", "0") & TA050631 %in% c("5", "0") & TA050127 == 3 & TA050769 < 60 & 
    TA050712 == 0 & TA050715 == 0 & TA050716 == 0 & TA050678 %in% c("3", "5", "7", "0") & TA050785 == 0 & TA050809 == 0 & TA050793 == 0 & TA050777 == 0 & TA050825 == 0 & 
    TA050817 == 0 & TA050798 == 0 & TA050394 %in% c("1", "2", "3", "4", "7", "8", "97", "99"), "FTL_05", ifelse(
        TA050042 == 1 & TA050043 %in% c("1", "96") & TA050595 %in% c("5", "0") & TA050631 %in% c("5", "0") & TA050127 == 3 & TA050769 < 60 & 
        TA050712 == 0 & TA050715 == 0 & TA050716 == 0 & TA050678 %in% c("3", "5", "7", "0") & TA050785 == 0 & TA050809 == 0 & TA050793 == 0 & TA050777 == 0 & TA050825 == 0 & 
        TA050817 == 0 & TA050798 == 0 & TA050394 == 0 & TA050371 == 1, "FTL_05", "IAC_05")))

table(TIAS2005$CAT)
TIAS2005_FTL <- subset(TIAS2005, CAT == "FTL_05")

# manually paste in variable names from var table

TIAS2005 <- TIAS2005 %>% select(ID, CAT, TA050042, TA050043, TA050595, TA050631, TA050127, TA050769, TA050712, TA050715, TA050716, TA050678, 
                                TA050785, TA050809, TA050793, TA050777, TA050825, TA050817, TA050798, TA050394, TA050371)

TIAS2005$TA050042_M <- TIAS2005$TA050042 == 1 
TIAS2005$TA050043_M <- TIAS2005$TA050043 %in% c("1", "96")
TIAS2005$TA050595_M <- TIAS2005$TA050595 %in% c("5", "0")
TIAS2005$TA050631_M <- TIAS2005$TA050631 %in% c("5", "0")
TIAS2005$TA050127_M <- TIAS2005$TA050127 == 3
TIAS2005$TA050769_M <- TIAS2005$TA050769 < 60
TIAS2005$TA050712_M <- TIAS2005$TA050712 == 0 
TIAS2005$TA050715_M <- TIAS2005$TA050715 == 0 
TIAS2005$TA050716_M <- TIAS2005$TA050716 == 0 
TIAS2005$TA050678_M <- TIAS2005$TA050678 %in% c("3", "5", "7", "0")
TIAS2005$TA050785_M <- TIAS2005$TA050785 == 0
TIAS2005$TA050809_M <- TIAS2005$TA050809 == 0
TIAS2005$TA050793_M <- TIAS2005$TA050793 == 0
TIAS2005$TA050777_M <- TIAS2005$TA050777 == 0
TIAS2005$TA050825_M <- TIAS2005$TA050825 == 0
TIAS2005$TA050817_M <- TIAS2005$TA050817 == 0
TIAS2005$TA050798_M <- TIAS2005$TA050798 == 0
TIAS2005$TA050394_M <- with(TIAS2005, ifelse(
  TA050394 %in% c("1", "2", "3", "4", "7", "8", "97", "99"), TRUE, ifelse(
    TA050394 == 0 & TA050371 == 1, TRUE, FALSE)))

T2005M <- TIAS2005 %>% select(TA050042_M, TA050043_M, TA050595_M, TA050631_M, TA050127_M, TA050769_M, TA050712_M, TA050715_M, TA050716_M,
                              TA050678_M, TA050785_M, TA050809_M, TA050793_M, TA050777_M, TA050825_M, TA050817_M, TA050798_M, TA050394_M, ID)

cols <- sapply(T2005M, is.logical)
T2005M[,cols] <- lapply(T2005M[,cols], as.numeric)

ncol(T2005M) # last column is 'ID', so the column range to plug in tidy.vars would be 1:(ncol-1)

nrow(T2005M) # find out how many rows are in T2007M, value of the chunk should the no. of columns (participants) you would want for each subplot 
chunk <- 50 
n <- nrow(T2005M)
r <- rep(1:ceiling(n/chunk), each=chunk)[1:n]

T2005M_list <- split(T2005M, r)
length(T2005M_list) # find out how many chunks have been created, i for functions would be 1:# of chunks

tidy.vars <- function(x){
  x %>% tidyr::gather(variable, met_FTL_crt, 1:18)
}

T2005M_tidy_list <- lapply(T2005M_list, tidy.vars)

# manually paste in variable names as strings to define levels for the factors 

for(i in 1:15) {
  T2005M_tidy_list[[i]]$variable <- factor(T2005M_tidy_list[[i]]$variable, levels = c("TA050042_M", "TA050043_M", "TA050595_M", "TA050631_M", "TA050127_M", "TA050769_M", "TA050712_M", "TA050715_M", "TA050716_M", "TA050678_M", "TA050785_M", "TA050809_M", "TA050793_M", "TA050777_M", "TA050825_M", "TA050817_M", "TA050798_M", "TA050394_M"))
} 

set.ID.levels <- function(x){
  dplyr::pull(x, ID)
}

T2005M_levels_list <- lapply(T2005M_list, set.ID.levels)

for(i in 1:15) {
  T2005M_tidy_list[[i]]$ID <- factor(T2005M_tidy_list[[i]]$ID, levels = T2005M_levels_list[[i]])
}

for(i in 1:15) {
  T2005M_tidy_list[[i]]$met_FTL_crt <- factor(T2005M_tidy_list[[i]]$met_FTL_crt)
}

create.heatmap <- function(x){
  ggplot(x, aes(x=ID, y=variable, fill=met_FTL_crt)) + geom_tile(color="white", size=0.5) +
    coord_equal() +
    labs(x="ID", y="Criteria") +
    theme_tufte(base_family="Helvetica") +
    theme(axis.ticks=element_blank()) + 
    theme(axis.text.x=element_text(angle = 45, hjust = 1)) + 
    scale_fill_discrete(name = "Met FTL Criteria", labels = c("No", "Yes"))
}

T05.heatmaps <- function(x){
  create.heatmap(T2005M_tidy_list[[x]])
}

# need to arrange plots with ggarrange manually, depending on how many plots you have for the wave, remove y.title for all plots except for the leftmost plots, remove legends for all plots except for the last plot

TIAS2005_plot <- ggarrange(T05.heatmaps(1) + rremove("legend"), T05.heatmaps(2) + rremove("legend") + rremove("y.title"), T05.heatmaps(3) + rremove("legend") + rremove("y.title"), 
                           T05.heatmaps(4) + rremove("legend"), T05.heatmaps(5) + rremove("legend") + rremove("y.title"), T05.heatmaps(6) + rremove("legend") + rremove("y.title"), 
                           T05.heatmaps(7) + rremove("legend"), T05.heatmaps(8) + rremove("legend") + rremove("y.title"), T05.heatmaps(9) + rremove("legend") + rremove("y.title"), 
                           T05.heatmaps(10) + rremove("legend"), T05.heatmaps(11) + rremove("legend") + rremove("y.title"), T05.heatmaps(12) + rremove("legend") + rremove("y.title"),
                           T05.heatmaps(13) + rremove("legend"), T05.heatmaps(14) + rremove("legend") + rremove("y.title"), T05.heatmaps(15) + rremove("y.title"), ncol = 3, nrow = 5) 

TIAS2005_plot # view plot, export manually as png with width 3000 height 2800

#####################
# TIAS 2007 
#####################

# clear global environment

rm(list=ls())

# import data again 

TIAS <- read.csv("https://raw.githubusercontent.com/carolinelee78/FTL/main/data/raw/PSID/TIAS/TIAS.csv")
TIAS$ID <- seq.int(nrow(TIAS))

# subset for wave  

TIAS2007 <- TIAS[!is.na(TIAS$TAS07),]

# manually paste in variable names and corresponding ftl criteria values from var table 

TIAS2007$CAT <- with(TIAS2007, ifelse(
    TA070042 == 1 & TA070043 %in% c("1", "96") & TA070570 %in% c("5", "0") & TA070602 %in% c("5", "0") & TA070127 == 3 & TA070740 < 60 & 
    TA070683 == 0 & TA070686 == 0 & TA070687 == 0 & TA070649 %in% c("3", "5", "7", "0") & TA070756 == 0 & TA070777 == 0 & TA070764 == 0 & TA070748 == 0 & TA070793 == 0 & 
    TA070785 == 0 & TA070769 == 0 & TA070368 %in% c("1", "2", "3", "4", "7", "8", "97", "99"), "FTL_07", ifelse(
        TA070042 == 1 & TA070043 %in% c("1", "96") & TA070570 %in% c("5", "0") & TA070602 %in% c("5", "0") & TA070127 == 3 & TA070740 < 60 & 
        TA070683 == 0 & TA070686 == 0 & TA070687 == 0 & TA070649 %in% c("3", "5", "7", "0") & TA070756 == 0 & TA070777 == 0 & TA070764 == 0 & TA070748 == 0 & TA070793 == 0 & 
        TA070785 == 0 & TA070769 == 0 & TA070368 == 0 & TA070344 == 1, "FTL_07", "IAC_07")))

table(TIAS2007$CAT)
TIAS2007_FTL <- subset(TIAS2007, CAT == "FTL_07")

# manually paste in variable names from var table

TIAS2007 <- TIAS2007 %>% select(ID, CAT, TA070042, TA070043, TA070570, TA070602, TA070127, TA070740, TA070683, TA070686, TA070687, TA070649, TA070756, 
                                TA070777, TA070764, TA070748, TA070793, TA070785, TA070769, TA070368, TA070344) 

# manually paste in ftl values for each variable from var table

TIAS2007$TA070042_M <- TIAS2007$TA070042 == 1 
TIAS2007$TA070043_M <- TIAS2007$TA070043 %in% c("1", "96")
TIAS2007$TA070570_M <- TIAS2007$TA070570 %in% c("5", "0")
TIAS2007$TA070602_M <- TIAS2007$TA070602 %in% c("5", "0")
TIAS2007$TA070127_M <- TIAS2007$TA070127 == 3
TIAS2007$TA070740_M <- TIAS2007$TA070740 < 60
TIAS2007$TA070683_M <- TIAS2007$TA070683 == 0 
TIAS2007$TA070686_M <- TIAS2007$TA070686 == 0 
TIAS2007$TA070687_M <- TIAS2007$TA070687 == 0 
TIAS2007$TA070649_M <- TIAS2007$TA070649 %in% c("3", "5", "7", "0")
TIAS2007$TA070756_M <- TIAS2007$TA070756 == 0
TIAS2007$TA070777_M <- TIAS2007$TA070777 == 0
TIAS2007$TA070764_M <- TIAS2007$TA070764 == 0
TIAS2007$TA070748_M <- TIAS2007$TA070748 == 0
TIAS2007$TA070793_M <- TIAS2007$TA070793 == 0
TIAS2007$TA070785_M <- TIAS2007$TA070785 == 0
TIAS2007$TA070769_M <- TIAS2007$TA070769 == 0
TIAS2007$TA070368_M <- with(TIAS2007, ifelse(
  TA070368 %in% c("1", "2", "3", "4", "7", "8", "97", "99"), TRUE, ifelse(
    TA070368 == 0 & TA070344 == 1, TRUE, FALSE)))

T2007M <- TIAS2007 %>% select(TA070042_M, TA070043_M, TA070570_M, TA070602_M, TA070127_M, TA070740_M, TA070683_M, TA070686_M, TA070687_M, TA070649_M, TA070756_M, 
                              TA070777_M, TA070764_M, TA070748_M, TA070793_M, TA070785_M, TA070769_M, TA070368_M, ID) 
  
cols <- sapply(T2007M, is.logical)
T2007M[,cols] <- lapply(T2007M[,cols], as.numeric)

ncol(T2007M) # last column is 'ID', so the column range to plug in tidy.vars would be 1:(ncol-1)

nrow(T2007M) # find out how many rows are in T2007M, value of the chunk should be the no. of columns (participants) you would want for each subplot 
chunk <- 50 
n <- nrow(T2007M)
r <- rep(1:ceiling(n/chunk), each=chunk)[1:n]

T2007M_list <- split(T2007M, r)
length(T2007M_list) # find out how many chunks have been created, i for functions would be 1:(# of chunks)

tidy.vars <- function(x){
  x %>% tidyr::gather(variable, met_FTL_crt, 1:18)
}
  
T2007M_tidy_list <- lapply(T2007M_list, tidy.vars)

for(i in 1:23) {
  T2007M_tidy_list[[i]]$variable <- factor(T2007M_tidy_list[[i]]$variable, levels = c("TA070042_M", "TA070043_M", "TA070570_M", "TA070602_M", "TA070127_M", "TA070740_M", "TA070683_M", "TA070686_M", "TA070687_M", "TA070649_M", "TA070756_M", "TA070777_M", "TA070764_M", "TA070748_M", "TA070793_M", "TA070785_M", "TA070769_M", "TA070368_M"))
}

set.ID.levels <- function(x){
  dplyr::pull(x, ID)
}

T2007M_levels_list <- lapply(T2007M_list, set.ID.levels)

for(i in 1:23) {
   T2007M_tidy_list[[i]]$ID <- factor(T2007M_tidy_list[[i]]$ID, levels = T2007M_levels_list[[i]])
}

for(i in 1:23) {
  T2007M_tidy_list[[i]]$met_FTL_crt <- factor(T2007M_tidy_list[[i]]$met_FTL_crt)
}

create.heatmap <- function(x){
  ggplot(x, aes(x=ID, y=variable, fill=met_FTL_crt)) + geom_tile(color="white", size=0.5) +
    coord_equal() +
    labs(x="ID", y="Criteria") +
    theme_tufte(base_family="Helvetica") +
    theme(axis.ticks=element_blank()) + 
    theme(axis.text.x=element_text(angle = 45, hjust = 1)) + 
    scale_fill_discrete(name = "Met FTL Criteria", labels = c("No", "Yes"))
}

T07.heatmaps <- function(x){
  create.heatmap(T2007M_tidy_list[[x]])
}

# need to arrange plots with ggarrange manually - depending on how many plots you have for the wave, remove y.title for all plots except for the leftmost plots, remove legends for all plots except for the last plot

TIAS2007_plot1 <- ggarrange(T07.heatmaps(1) + rremove("legend"), T07.heatmaps(2) + rremove("legend") + rremove("y.title"), T07.heatmaps(3) + rremove("legend") + rremove("y.title"), 
                            T07.heatmaps(4) + rremove("legend"), T07.heatmaps(5) + rremove("legend") + rremove("y.title"), T07.heatmaps(6) + rremove("legend") + rremove("y.title"), 
                            T07.heatmaps(7) + rremove("legend"), T07.heatmaps(8) + rremove("legend") + rremove("y.title"), T07.heatmaps(9) + rremove("legend") + rremove("y.title"), 
                            T07.heatmaps(10) + rremove("legend"), T07.heatmaps(11) + rremove("legend") + rremove("y.title"), T07.heatmaps(12) + rremove("legend") + rremove("y.title"), ncol = 3, nrow = 4) 

TIAS2007_plot1 # view first aggregated plot, export manually as png with width 3000 height 2000

TIAS2007_plot2 <- ggarrange(T07.heatmaps(13) + rremove("legend"), T07.heatmaps(14) + rremove("legend") + rremove("y.title"), T07.heatmaps(15) + rremove("legend") + rremove("y.title"), 
                            T07.heatmaps(16) + rremove("legend"), T07.heatmaps(17) + rremove("legend") + rremove("y.title"), T07.heatmaps(18) + rremove("legend") + rremove("y.title"), 
                            T07.heatmaps(19) + rremove("legend"), T07.heatmaps(20) + rremove("legend") + rremove("y.title"), T07.heatmaps(21) + rremove("legend") + rremove("y.title"), 
                            T07.heatmaps(22) + rremove("legend"), T07.heatmaps(23) + rremove("y.title"), ncol = 3, nrow = 4) 

TIAS2007_plot2 # view second aggregated plot, export manually as png with width 3000 height 2000 

#####################
# TIAS 2009
#####################

# clear global environment

rm(list=ls())

# import data again 

TIAS <- read.csv("https://raw.githubusercontent.com/carolinelee78/FTL/main/data/raw/PSID/TIAS/TIAS.csv")
TIAS$ID <- seq.int(nrow(TIAS))

# subset for wave  

TIAS2009 <- TIAS[!is.na(TIAS$TAS09),]

# manually paste in variable names and corresponding ftl criteria values from var table 

TIAS2009$CAT <- with(TIAS2009, ifelse(
  TA090043 == 1 & TA090044 %in% c("1", "96") & TA090612 %in% c("5", "0") & TA090655 %in% c("5", "0") & TA090136 == 3 & TA090799 < 60 & 
  TA090739 == 0 & TA090742 == 0 & TA090743 == 0 & TA090705 %in% c("3", "5", "7", "0") & TA090815 == 0 & TA090836 == 0 & TA090823 == 0 & TA090807 == 0 & TA090852 == 0 & 
  TA090844 == 0 & TA090828 == 0 & TA090385 %in% c("1", "2", "3", "4", "7", "8", "97", "99"), "FTL_09", ifelse(
      TA090043 == 1 & TA090044 %in% c("1", "96") & TA090612 %in% c("5", "0") & TA090655 %in% c("5", "0") & TA090136 == 3 & TA090799 < 60 & 
      TA090739 == 0 & TA090742 == 0 & TA090743 == 0 & TA090705 %in% c("3", "5", "7", "0") & TA090815 == 0 & TA090836 == 0 & TA090823 == 0 & TA090807 == 0 & TA090852 == 0 & 
      TA090844 == 0 & TA090828 == 0 & TA090385 == 0 & TA090361 == 1, "FTL_09", "IAC_07")))

table(TIAS2009$CAT)
TIAS2009_FTL <- subset(TIAS2009, CAT == "FTL_09")

# manually paste in variable names from var table

TIAS2009 <- TIAS2009 %>% select(ID, CAT, TA090043, TA090044, TA090612, TA090655, TA090136, TA090799, TA090739, TA090742, TA090743, TA090705, TA090815, 
                                TA090836, TA090823, TA090807, TA090852, TA090844, TA090828, TA090385, TA090361) 

# manually paste in ftl values for each variable from var table

TIAS2009$TA090043_M <- TIAS2009$TA090043 == 1 
TIAS2009$TA090044_M <- TIAS2009$TA090044 %in% c("1", "96")
TIAS2009$TA090612_M <- TIAS2009$TA090612 %in% c("5", "0")
TIAS2009$TA090655_M <- TIAS2009$TA090655 %in% c("5", "0")
TIAS2009$TA090136_M <- TIAS2009$TA090136 == 3
TIAS2009$TA090799_M <- TIAS2009$TA090799 < 60
TIAS2009$TA090739_M <- TIAS2009$TA090739 == 0 
TIAS2009$TA090742_M <- TIAS2009$TA090742 == 0 
TIAS2009$TA090743_M <- TIAS2009$TA090743 == 0 
TIAS2009$TA090705_M <- TIAS2009$TA090705 %in% c("3", "5", "7", "0")
TIAS2009$TA090815_M <- TIAS2009$TA090815 == 0
TIAS2009$TA090836_M <- TIAS2009$TA090836 == 0
TIAS2009$TA090823_M <- TIAS2009$TA090823 == 0
TIAS2009$TA090807_M <- TIAS2009$TA090807 == 0
TIAS2009$TA090852_M <- TIAS2009$TA090852 == 0
TIAS2009$TA090844_M <- TIAS2009$TA090844 == 0
TIAS2009$TA090828_M <- TIAS2009$TA090828 == 0
TIAS2009$TA090385_M <- with(TIAS2009, ifelse(
  TA090385 %in% c("1", "2", "3", "4", "7", "8", "97", "99"), TRUE, ifelse(
    TA090385 == 0 & TA090361 == 1, TRUE, FALSE)))

T2009M <- TIAS2009 %>% select(TA090043_M, TA090044_M, TA090612_M, TA090655_M, TA090136_M, TA090799_M, TA090739_M, TA090742_M, TA090743_M, TA090705_M, TA090815_M, 
                              TA090836_M, TA090823_M, TA090807_M, TA090852_M, TA090844_M, TA090828_M, TA090385_M, ID) 

cols <- sapply(T2009M, is.logical)
T2009M[,cols] <- lapply(T2009M[,cols], as.numeric)

ncol(T2009M) # last column is 'ID', so the column range to plug in tidy.vars would be 1:(ncol-1)

nrow(T2009M) # find out how many rows are in T2009M, value of the chunk should be the no. of columns (participants) you would want for each subplot 
chunk <- 50 
n <- nrow(T2009M)
r <- rep(1:ceiling(n/chunk), each=chunk)[1:n]

T2009M_list <- split(T2009M, r)
length(T2009M_list) # find out how many chunks have been created, i for functions would be 1:(# of chunks)

tidy.vars <- function(x){
  x %>% tidyr::gather(variable, met_FTL_crt, 1:18)
}

T2009M_tidy_list <- lapply(T2009M_list, tidy.vars)

for(i in 1:32) {
  T2009M_tidy_list[[i]]$variable <- factor(T2009M_tidy_list[[i]]$variable, levels = c("TA090043_M", "TA090044_M", "TA090612_M", "TA090655_M", "TA090136_M", "TA090799_M", "TA090739_M", "TA090742_M", "TA090743_M", "TA090705_M", "TA090815_M", "TA090836_M", "TA090823_M", "TA090807_M", "TA090852_M", "TA090844_M", "TA090828_M", "TA090385_M"))
}                                                                                     

set.ID.levels <- function(x){
  dplyr::pull(x, ID)
}

T2009M_levels_list <- lapply(T2009M_list, set.ID.levels)

for(i in 1:32) {
  T2009M_tidy_list[[i]]$ID <- factor(T2009M_tidy_list[[i]]$ID, levels = T2009M_levels_list[[i]])
}

for(i in 1:32) {
  T2009M_tidy_list[[i]]$met_FTL_crt <- factor(T2009M_tidy_list[[i]]$met_FTL_crt)
}

create.heatmap <- function(x){
  ggplot(x, aes(x=ID, y=variable, fill=met_FTL_crt)) + geom_tile(color="white", size=0.5) +
    coord_equal() +
    labs(x="ID", y="Criteria") +
    theme_tufte(base_family="Helvetica") +
    theme(axis.ticks=element_blank()) + 
    theme(axis.text.x=element_text(angle = 45, hjust = 1)) + 
    scale_fill_discrete(name = "Met FTL Criteria", labels = c("No", "Yes"))
}

T09.heatmaps <- function(x){
  create.heatmap(T2009M_tidy_list[[x]])
}

# need to arrange plots with ggarrange manually - depending on how many plots you have for the wave, remove y.title for all plots except for the leftmost plots, remove legends for all plots except for the last plot

TIAS2009_plot1 <- ggarrange(T09.heatmaps(1) + rremove("legend"), T09.heatmaps(2) + rremove("legend") + rremove("y.title"), T09.heatmaps(3) + rremove("legend") + rremove("y.title"), T09.heatmaps(4) + rremove("legend") + rremove("y.title"), 
                            T09.heatmaps(5) + rremove("legend"), T09.heatmaps(6) + rremove("legend") + rremove("y.title"), T09.heatmaps(7) + rremove("legend") + rremove("y.title"), T09.heatmaps(8) + rremove("legend") + rremove("y.title"), 
                            T09.heatmaps(9) + rremove("legend"), T09.heatmaps(10) + rremove("legend") + rremove("y.title"), T09.heatmaps(11) + rremove("legend") + rremove("y.title"), T09.heatmaps(12) + rremove("legend") + rremove("y.title"), 
                            T09.heatmaps(13) + rremove("legend"), T09.heatmaps(14) + rremove("legend") + rremove("y.title"), T09.heatmaps(15) + rremove("legend") + rremove("y.title"), T09.heatmaps(16) + rremove("legend") + rremove("y.title"), ncol = 4, nrow = 4) 

TIAS2009_plot1 # view first aggregated plot, export manually as png with width 3500 height 2000

TIAS2009_plot2 <- ggarrange(T09.heatmaps(17) + rremove("legend"), T09.heatmaps(18) + rremove("legend") + rremove("y.title"), T09.heatmaps(19) + rremove("legend") + rremove("y.title"), T09.heatmaps(20) + rremove("legend") + rremove("y.title"), 
                            T09.heatmaps(21) + rremove("legend"), T09.heatmaps(22) + rremove("legend") + rremove("y.title"), T09.heatmaps(23) + rremove("legend") + rremove("y.title"), T09.heatmaps(24) + rremove("legend") + rremove("y.title"), 
                            T09.heatmaps(25) + rremove("legend"), T09.heatmaps(26) + rremove("legend") + rremove("y.title"), T09.heatmaps(27) + rremove("legend") + rremove("y.title"), T09.heatmaps(28) + rremove("legend") + rremove("y.title"), 
                            T09.heatmaps(29) + rremove("legend"), T09.heatmaps(30) + rremove("legend") + rremove("y.title"), T09.heatmaps(31) + rremove("legend") + rremove("y.title"), T09.heatmaps(32) + rremove("y.title"), ncol = 4, nrow = 4) 

TIAS2009_plot2 # view second aggregated plot, export manually as png with width 3500 height 2000

#####################
# TIAS 2011
#####################

# clear global environment

rm(list=ls())

# import data again 

TIAS <- read.csv("https://raw.githubusercontent.com/carolinelee78/FTL/main/data/raw/PSID/TIAS/TIAS.csv")
TIAS$ID <- seq.int(nrow(TIAS))

# subset for wave  

TIAS2011 <- TIAS[!is.na(TIAS$TAS11),]

# manually paste in variable names and corresponding ftl criteria values from var table 

TIAS2011$CAT <- with(TIAS2011, ifelse(
  TA110044 == 1 & TA110045 %in% c("1", "96") & TA110699 %in% c("5", "0") & TA110743 %in% c("5", "0") & TA110137 == 3 & TA110915 < 60 & 
  TA110829 == 0 & TA110832 == 0 & TA110833 == 0 & TA110793 %in% c("3", "5", "7", "0") & TA110931 == 0 & TA110952 == 0 & TA110939 == 0 & TA110923 == 0 & TA110968 == 0 & 
  TA110960 == 0 & TA110944 == 0 & TA110462 %in% c("1", "2", "3", "4", "7", "8", "97", "99"), "FTL_11", ifelse(
      TA110044 == 1 & TA110045 %in% c("1", "96") & TA110699 %in% c("5", "0") & TA110743 %in% c("5", "0") & TA110137 == 3 & TA110915 < 60 & 
      TA110829 == 0 & TA110832 == 0 & TA110833 == 0 & TA110793 %in% c("3", "5", "7", "0") & TA110931 == 0 & TA110952 == 0 & TA110939 == 0 & TA110923 == 0 & TA110968 == 0 & 
      TA110960 == 0 & TA110944 == 0 & TA110462 == 0 & TA110351 == 1, "FTL_11", "IAC_11")))

table(TIAS2011$CAT)
TIAS2011_FTL <- subset(TIAS2011, CAT == "FTL_11")

# manually paste in variable names from var table

TIAS2011 <- TIAS2011 %>% select(ID, CAT, TA110044, TA110045, TA110699, TA110743, TA110137, TA110915, TA110829, TA110832, TA110833, TA110793, TA110931, 
                                TA110952, TA110939, TA110923, TA110968, TA110960, TA110944, TA110462, TA110351) 

# manually paste in ftl values for each variable from var table

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
TIAS2011$TA110462_M <- with(TIAS2011, ifelse(
  TA110462 %in% c("1", "2", "3", "4", "7", "8", "97", "99"), TRUE, ifelse(
    TA110462 == 0 & TA110351 == 1, TRUE, FALSE)))

T2011M <- TIAS2011 %>% select(TA110044_M, TA110045_M, TA110699_M, TA110743_M, TA110137_M, TA110915_M, TA110829_M, TA110832_M, TA110833_M, TA110793_M, TA110931_M, 
                              TA110952_M, TA110939_M, TA110923_M, TA110968_M, TA110960_M, TA110944_M, TA110462_M, ID) 

cols <- sapply(T2011M, is.logical)
T2011M[,cols] <- lapply(T2011M[,cols], as.numeric)

ncol(T2011M) # last column is 'ID', so the column range to plug in tidy.vars would be 1:(ncol-1)

nrow(T2011M) # find out how many rows are in T2011M, value of the chunk should be the no. of columns (participants) you would want for each subplot 
chunk <- 50 
n <- nrow(T2011M)
r <- rep(1:ceiling(n/chunk), each=chunk)[1:n]

T2011M_list <- split(T2011M, r)
length(T2011M_list) # find out how many chunks have been created, i for functions would be 1:(# of chunks)

tidy.vars <- function(x){
  x %>% tidyr::gather(variable, met_FTL_crt, 1:18)
}

T2011M_tidy_list <- lapply(T2011M_list, tidy.vars)

for(i in 1:39) {
  T2011M_tidy_list[[i]]$variable <- factor(T2011M_tidy_list[[i]]$variable, levels = c("TA110044_M", "TA110045_M", "TA110699_M", "TA110743_M", "TA110137_M", "TA110915_M", "TA110829_M", "TA110832_M", "TA110833_M", "TA110793_M", "TA110931_M", "TA110952_M", "TA110939_M", "TA110923_M", "TA110968_M", "TA110960_M", "TA110944_M", "TA110462_M"))
}

set.ID.levels <- function(x){
  dplyr::pull(x, ID)
}

T2011M_levels_list <- lapply(T2011M_list, set.ID.levels)

for(i in 1:39) {
  T2011M_tidy_list[[i]]$ID <- factor(T2011M_tidy_list[[i]]$ID, levels = T2011M_levels_list[[i]])
}

for(i in 1:39) {
  T2011M_tidy_list[[i]]$met_FTL_crt <- factor(T2011M_tidy_list[[i]]$met_FTL_crt)
}

create.heatmap <- function(x){
  ggplot(x, aes(x=ID, y=variable, fill=met_FTL_crt)) + geom_tile(color="white", size=0.5) +
    coord_equal() +
    labs(x="ID", y="Criteria") +
    theme_tufte(base_family="Helvetica") +
    theme(axis.ticks=element_blank()) + 
    theme(axis.text.x=element_text(angle = 45, hjust = 1)) + 
    scale_fill_discrete(name = "Met FTL Criteria", labels = c("No", "Yes"))
}

T11.heatmaps <- function(x){
  create.heatmap(T2011M_tidy_list[[x]])
}

TIAS2011_plot1 <- ggarrange(T11.heatmaps(1) + rremove("legend"), T11.heatmaps(2) + rremove("legend") + rremove("y.title"), T11.heatmaps(3) + rremove("legend") + rremove("y.title"), 
                            T11.heatmaps(4) + rremove("legend"), T11.heatmaps(5) + rremove("legend") + rremove("y.title"), T11.heatmaps(6) + rremove("legend") + rremove("y.title"), 
                            T11.heatmaps(7) + rremove("legend"), T11.heatmaps(8) + rremove("legend") + rremove("y.title"), T11.heatmaps(9) + rremove("legend") + rremove("y.title"), 
                            T11.heatmaps(10) + rremove("legend"), T11.heatmaps(11) + rremove("legend") + rremove("y.title"), T11.heatmaps(12) + rremove("legend") + rremove("y.title"), ncol = 3, nrow = 4) 

TIAS2011_plot1 # view first aggregated plot, export manually as png with width 3000 height 2000

TIAS2011_plot2 <- ggarrange(T11.heatmaps(13) + rremove("legend"), T11.heatmaps(14) + rremove("legend") + rremove("y.title"), T11.heatmaps(15) + rremove("legend") + rremove("y.title"), 
                            T11.heatmaps(16) + rremove("legend"), T11.heatmaps(17) + rremove("legend") + rremove("y.title"), T11.heatmaps(18) + rremove("legend") + rremove("y.title"), 
                            T11.heatmaps(19) + rremove("legend"), T11.heatmaps(20) + rremove("legend") + rremove("y.title"), T11.heatmaps(21) + rremove("legend") + rremove("y.title"), ncol = 3, nrow = 3) 

TIAS2011_plot2 # view second aggregated plot, export manually as png with width 3000 height 1500

TIAS2011_plot3 <- ggarrange(T11.heatmaps(22) + rremove("legend"), T11.heatmaps(23) + rremove("legend") + rremove("y.title"), T11.heatmaps(24) + rremove("legend") + rremove("y.title"), 
                            T11.heatmaps(25) + rremove("legend"), T11.heatmaps(26) + rremove("legend") + rremove("y.title"), T11.heatmaps(27) + rremove("legend") + rremove("y.title"), 
                            T11.heatmaps(28) + rremove("legend"), T11.heatmaps(29) + rremove("legend") + rremove("y.title"), T11.heatmaps(30) + rremove("legend") + rremove("y.title"), ncol = 3, nrow = 3) 

TIAS2011_plot3 # view third aggregated plot, export manually as png with width 3000 height 1500

TIAS2011_plot4 <- ggarrange(T11.heatmaps(31) + rremove("legend"), T11.heatmaps(32) + rremove("legend") + rremove("y.title"), T11.heatmaps(33) + rremove("legend") + rremove("y.title"), 
                            T11.heatmaps(34) + rremove("legend"), T11.heatmaps(35) + rremove("legend") + rremove("y.title"), T11.heatmaps(36) + rremove("legend") + rremove("y.title"), 
                            T11.heatmaps(37) + rremove("legend"), T11.heatmaps(38) + rremove("legend") + rremove("y.title"), T11.heatmaps(39) + rremove("y.title"), ncol = 3, nrow = 3) 

TIAS2011_plot4 # view fourth aggregated plot, export manually as png with width 3000 height 1500


#####################
# TIAS 2013
#####################

# clear global environment

rm(list=ls())

# import data again 

TIAS <- read.csv("https://raw.githubusercontent.com/carolinelee78/FTL/main/data/raw/PSID/TIAS/TIAS.csv")
TIAS$ID <- seq.int(nrow(TIAS))

# subset for wave  

TIAS2013 <- TIAS[!is.na(TIAS$TAS13),]

# manually paste in variable names and corresponding ftl criteria values from var table 

TIAS2013$CAT <- with(TIAS2013, ifelse(
  TA130043 == 1 & TA130044 %in% c("1", "96") & TA130719 %in% c("5", "0") & TA130763 %in% c("5", "0") & TA130136 == 3 & TA130948 < 60 & 
  TA130852 == 0 & TA130855 == 0 & TA130856 == 0 & TA130813 %in% c("3", "5", "7", "0") & TA130961 == 0 & TA130982 == 0 & TA130969 == 0 & TA130861 == 0 & TA130998 == 0 &
  TA130990 == 0 & TA130977 == 0 & TA130482 %in% c("1", "2", "3", "4", "7", "8", "97", "99"), "FTL_13", ifelse(
      TA130043 == 1 & TA130044 %in% c("1", "96") & TA130719 %in% c("5", "0") & TA130763 %in% c("5", "0") & TA130136 == 3 & TA130948 < 60 & 
      TA130852 == 0 & TA130855 == 0 & TA130856 == 0 & TA130813 %in% c("3", "5", "7", "0") & TA130961 == 0 & TA130982 == 0 & TA130969 == 0 & TA130861 == 0 & TA130998 == 0 & 
        TA130990 == 0 & TA130977 == 0 & TA130482 == 0 & TA130350 == 1, "FTL_13", "IAC_13")))
  
table(TIAS2013$CAT)
TIAS2013_FTL <- subset(TIAS2013, CAT == "FTL_13")

# manually paste in variable names from var table

TIAS2013 <- TIAS2013 %>% select(ID, CAT, TA130043, TA130044, TA130719, TA130763, TA130136, TA130948, TA130852, TA130855, TA130856, TA130813, TA130961, 
                                TA130982, TA130969, TA130861, TA130998, TA130990, TA130977, TA130482, TA130350) 

# manually paste in ftl values for each variable from var table

TIAS2013$TA130043_M <- TIAS2013$TA130043 == 1 
TIAS2013$TA130044_M <- TIAS2013$TA130044 %in% c("1", "96")
TIAS2013$TA130719_M <- TIAS2013$TA130719 %in% c("5", "0")
TIAS2013$TA130763_M <- TIAS2013$TA130763 %in% c("5", "0")
TIAS2013$TA130136_M <- TIAS2013$TA130136 == 3
TIAS2013$TA130948_M <- TIAS2013$TA130948 < 60
TIAS2013$TA130852_M <- TIAS2013$TA130852 == 0 
TIAS2013$TA130855_M <- TIAS2013$TA130855 == 0 
TIAS2013$TA130856_M <- TIAS2013$TA130856 == 0 
TIAS2013$TA130813_M <- TIAS2013$TA130813 %in% c("3", "5", "7", "0")
TIAS2013$TA130961_M <- TIAS2013$TA130961 == 0
TIAS2013$TA130982_M <- TIAS2013$TA130982 == 0
TIAS2013$TA130969_M <- TIAS2013$TA130969 == 0
TIAS2013$TA130861_M <- TIAS2013$TA130861 == 0
TIAS2013$TA130998_M <- TIAS2013$TA130998 == 0
TIAS2013$TA130990_M <- TIAS2013$TA130990 == 0
TIAS2013$TA130977_M <- TIAS2013$TA130977 == 0
TIAS2013$TA130482_M <- with(TIAS2013, ifelse( 
  TA130482 %in% c("1", "2", "3", "4", "7", "8", "97", "99"), TRUE, ifelse(
    TA130482 == 0 & TA130350 == 1, TRUE, FALSE)))

T2013M <- TIAS2013 %>% select(TA130043_M, TA130044_M, TA130719_M, TA130763_M, TA130136_M, TA130948_M, TA130852_M, TA130855_M, TA130856_M, TA130813_M, TA130961_M, 
                              TA130982_M, TA130969_M, TA130861_M, TA130998_M, TA130990_M, TA130977_M, TA130482_M, ID) 

cols <- sapply(T2013M, is.logical)
T2013M[,cols] <- lapply(T2013M[,cols], as.numeric)

ncol(T2013M) # last column is 'ID', so the column range to plug in tidy.vars would be 1:(ncol-1)

nrow(T2013M) # find out how many rows are in T2011M, value of the chunk should be the no. of columns (participants) you would want for each subplot 
chunk <- 50 
n <- nrow(T2013M)
r <- rep(1:ceiling(n/chunk), each=chunk)[1:n]

T2013M_list <- split(T2013M, r)
length(T2013M_list) # find out how many chunks have been created, i for functions would be 1:(# of chunks)

tidy.vars <- function(x){
  x %>% tidyr::gather(variable, met_FTL_crt, 1:18)
}

T2013M_tidy_list <- lapply(T2013M_list, tidy.vars)

for(i in 1:37) {
  T2013M_tidy_list[[i]]$variable <- factor(T2013M_tidy_list[[i]]$variable, levels = c("TA130043_M", "TA130044_M", "TA130719_M", "TA130763_M", "TA130136_M", "TA130948_M", "TA130852_M", "TA130855_M", "TA130856_M", "TA130813_M", "TA130961_M",  "TA130982_M", "TA130969_M", "TA130861_M", "TA130998_M", "TA130990_M", "TA130977_M", "TA130482_M"))
}

set.ID.levels <- function(x){
  dplyr::pull(x, ID)
}

T2013M_levels_list <- lapply(T2013M_list, set.ID.levels)

for(i in 1:37) {
  T2013M_tidy_list[[i]]$ID <- factor(T2013M_tidy_list[[i]]$ID, levels = T2013M_levels_list[[i]])
}

for(i in 1:37) {
  T2013M_tidy_list[[i]]$met_FTL_crt <- factor(T2013M_tidy_list[[i]]$met_FTL_crt)
}

create.heatmap <- function(x){
  ggplot(x, aes(x=ID, y=variable, fill=met_FTL_crt)) + geom_tile(color="white", size=0.5) +
    coord_equal() +
    labs(x="ID", y="Criteria") +
    theme_tufte(base_family="Helvetica") +
    theme(axis.ticks=element_blank()) + 
    theme(axis.text.x=element_text(angle = 45, hjust = 1)) + 
    scale_fill_discrete(name = "Met FTL Criteria", labels = c("No", "Yes"))
}

T13.heatmaps <- function(x){
  create.heatmap(T2013M_tidy_list[[x]])
}

TIAS2013_plot1 <- ggarrange(T13.heatmaps(1) + rremove("legend"), T13.heatmaps(2) + rremove("legend") + rremove("y.title"), T13.heatmaps(3) + rremove("legend") + rremove("y.title"), T13.heatmaps(4) + rremove("legend") + rremove("y.title"), 
                            T13.heatmaps(5) + rremove("legend"), T13.heatmaps(6) + rremove("legend") + rremove("y.title"), T13.heatmaps(7) + rremove("legend") + rremove("y.title"), T13.heatmaps(8) + rremove("legend") + rremove("y.title"), 
                            T13.heatmaps(9) + rremove("legend"), T13.heatmaps(10) + rremove("legend") + rremove("y.title"), T13.heatmaps(11) + rremove("legend") + rremove("y.title"), T13.heatmaps(12) + rremove("legend") + rremove("y.title"), 
                            T13.heatmaps(13) + rremove("legend"), T13.heatmaps(14) + rremove("legend") + rremove("y.title"), T13.heatmaps(15) + rremove("legend") + rremove("y.title"), T13.heatmaps(16) + rremove("legend") + rremove("y.title"), ncol = 4, nrow = 4) 

TIAS2013_plot1 # view first aggregated plot, export manually as png with width 3500 height 2000

TIAS2013_plot2 <- ggarrange(T13.heatmaps(17) + rremove("legend"), T13.heatmaps(18) + rremove("legend") + rremove("y.title"), T13.heatmaps(19) + rremove("legend") + rremove("y.title"), T13.heatmaps(20) + rremove("legend") + rremove("y.title"), 
                            T13.heatmaps(21) + rremove("legend"), T13.heatmaps(22) + rremove("legend") + rremove("y.title"), T13.heatmaps(23) + rremove("legend") + rremove("y.title"), T13.heatmaps(24) + rremove("legend") + rremove("y.title"), 
                            T13.heatmaps(25) + rremove("legend"), T13.heatmaps(26) + rremove("legend") + rremove("y.title"), T13.heatmaps(27) + rremove("legend") + rremove("y.title"), T13.heatmaps(28) + rremove("legend") + rremove("y.title"), ncol = 4, nrow = 3)

TIAS2013_plot2 # view second aggregated plot, export manually as png with width 3500 height 1500

TIAS2013_plot3 <- ggarrange(T13.heatmaps(29) + rremove("legend"), T13.heatmaps(30) + rremove("legend") + rremove("y.title"), T13.heatmaps(31) + rremove("legend") + rremove("y.title"),
                            T13.heatmaps(32) + rremove("legend"), T13.heatmaps(33) + rremove("legend") + rremove("y.title"), T13.heatmaps(34) + rremove("y.title") + rremove("legend"), 
                            T13.heatmaps(35) + rremove("legend"), T13.heatmaps(36) + rremove("legend") + rremove("y.title"), T13.heatmaps(37) + rremove("y.title"), ncol = 3, nrow = 3) 

TIAS2013_plot3 # view third aggregated plot, export manually as png with width 3500 height 1500

#####################
# TIAS 2015 
#####################

# clear global environment

rm(list=ls())

# import data again 

TIAS <- read.csv("https://raw.githubusercontent.com/carolinelee78/FTL/main/data/raw/PSID/TIAS/TIAS.csv")
TIAS$ID <- seq.int(nrow(TIAS))

# subset for wave  

TIAS2015 <- TIAS[!is.na(TIAS$TAS15),]

# manually paste in variable names and corresponding ftl criteria values from var table 

TIAS2015$CAT <- with(TIAS2015, ifelse(
   TA150043 == 1 & TA150044 %in% c("1", "96") & TA150731 %in% c("5", "0") & TA150776 %in% c("5", "0") & TA150128 == 3 & TA150970 < 60 & 
   TA150869 == 0 & TA150872 == 0 & TA150873 == 0 & TA150826 %in% c("3", "5", "7", "0") & TA150986 == 0 & TA151007 == 0 & TA150994 == 0 & TA150978 == 0 & TA151023 == 0 & 
   TA151015 == 0 & TA150999 == 0 & TA150491 %in% c("1", "2", "3", "4", "7", "8", "97", "99"), "FTL_15", ifelse(
      TA150043 == 1 & TA150044 %in% c("1", "96") & TA150731 %in% c("5", "0") & TA150776 %in% c("5", "0") & TA150128 == 3 & TA150970 < 60 & 
      TA150869 == 0 & TA150872 == 0 & TA150873 == 0 & TA150826 %in% c("3", "5", "7", "0") & TA150986 == 0 & TA151007 == 0 & TA150994 == 0 & TA150978 == 0 & TA151023 == 0 & 
      TA151015 == 0 & TA150999 == 0 & TA150491 == 0 & TA150352 == 1, "FTL_15", "IAC_15")))
    
table(TIAS2015$CAT)
TIAS2015_FTL <- subset(TIAS2015, CAT == "FTL_15")

TIAS2015 <- TIAS2015 %>% select(ID, CAT, TA150043, TA150044, TA150731, TA150776, TA150128, TA150970, TA150869, TA150872, TA150873, TA150826, TA150986, TA151007, TA150994, TA150978, TA151023, TA151015, TA150999, TA150491, TA150352)

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
TIAS2015$TA150491_M <- with(TIAS2015, ifelse(
  TA150491 %in% c("1", "2", "3", "4", "7", "8", "97", "99"), TRUE, ifelse(
    TA150491 == 0 & TA150352 == 1, TRUE, FALSE)))

table(TIAS2015$TA150826_M)
TIAS2015_FTL <- subset(TIAS2015, CAT == "FTL_15")
  
T2015M <- TIAS2015 %>% select(TA150043_M, TA150044_M, TA150731_M, TA150776_M, TA150128_M, TA150970_M, TA150869_M, TA150872_M, TA150873_M,
                                TA150826_M, TA150986_M, TA151007_M, TA150994_M, TA150978_M, TA151023_M, TA151015_M, TA150999_M, TA150491_M, ID)
  
cols <- sapply(T2015M, is.logical)
T2015M[,cols] <- lapply(T2015M[,cols], as.numeric)

ncol(T2015M) # last column is 'ID', so the column range to plug in tidy.vars would be 1:(ncol-1)

nrow(T2015M) # find out how many rows are in T2015M, value of the chunk should the no. of columns (participants) you would want for each subplot 
chunk <- 50 
n <- nrow(T2015M)
r <- rep(1:ceiling(n/chunk), each=chunk)[1:n]

T2015M_list <- split(T2015M, r)
length(T2015M_list) # find out how many chunks have been created, i for functions would be 1:(# of chunks)

tidy.vars <- function(x){
  x %>% tidyr::gather(variable, met_FTL_crt, 1:18)
}

T2015M_tidy_list <- lapply(T2015M_list, tidy.vars)

# manually paste in variable names as strings to define levels for the factors 

for(i in 1:33) {
  T2015M_tidy_list[[i]]$variable <- factor(T2015M_tidy_list[[i]]$variable, levels = c("TA150043_M", "TA150044_M", "TA150731_M", "TA150776_M", "TA150128_M", "TA150970_M", "TA150869_M", "TA150872_M", "TA150873_M", "TA150826_M", "TA150986_M", "TA151007_M", "TA150994_M", "TA150978_M", "TA151023_M", "TA151015_M", "TA150999_M", "TA150491_M"))
} 

set.ID.levels <- function(x){
  dplyr::pull(x, ID)
}

T2015M_levels_list <- lapply(T2015M_list, set.ID.levels)

for(i in 1:33) {
  T2015M_tidy_list[[i]]$ID <- factor(T2015M_tidy_list[[i]]$ID, levels = T2015M_levels_list[[i]])
}

for(i in 1:33) {
  T2015M_tidy_list[[i]]$met_FTL_crt <- factor(T2015M_tidy_list[[i]]$met_FTL_crt)
}

create.heatmap <- function(x){
  ggplot(x, aes(x=ID, y=variable, fill=met_FTL_crt)) + geom_tile(color="white", size=0.5) +
    coord_equal() +
    labs(x="ID", y="Criteria") +
    theme_tufte(base_family="Helvetica") +
    theme(axis.ticks=element_blank()) + 
    theme(axis.text.x=element_text(angle = 45, hjust = 1)) + 
    scale_fill_discrete(name = "Met FTL Criteria", labels = c("No", "Yes"))
}

T15.heatmaps <- function(x){
  create.heatmap(T2015M_tidy_list[[x]])
}

# need to arrange plots with ggarrange manually - depending on how many plots you have for the wave, remove y.title for all plots except for the leftmost plots, remove legends for all plots except for the last plot

TIAS2015_plot1 <- ggarrange(T15.heatmaps(1) + rremove("legend"), T15.heatmaps(2) + rremove("legend") + rremove("y.title"), T15.heatmaps(3) + rremove("legend") + rremove("y.title"), 
                            T15.heatmaps(4) + rremove("legend"), T15.heatmaps(5) + rremove("legend") + rremove("y.title"), T15.heatmaps(6) + rremove("legend") + rremove("y.title"), 
                            T15.heatmaps(7) + rremove("legend"), T15.heatmaps(8) + rremove("legend") + rremove("y.title"), T15.heatmaps(9) + rremove("legend") + rremove("y.title"), 
                            T15.heatmaps(10) + rremove("legend"), T15.heatmaps(11) + rremove("legend") + rremove("y.title"), T15.heatmaps(12) + rremove("legend") + rremove("y.title"), ncol = 3, nrow = 4) 
                            
TIAS2015_plot1 # view first aggregated plot, export manually as png with width 3000 height 2000 

TIAS2015_plot2 <- ggarrange(T15.heatmaps(13) + rremove("legend"), T15.heatmaps(14) + rremove("legend") + rremove("y.title"), T15.heatmaps(15) + rremove("y.title") + rremove("legend"), 
                            T15.heatmaps(16) + rremove("legend"), T15.heatmaps(17) + rremove("legend") + rremove("y.title"), T15.heatmaps(18) + rremove("legend") + rremove("y.title"), 
                            T15.heatmaps(19) + rremove("legend"), T15.heatmaps(20) + rremove("legend") + rremove("y.title"), T15.heatmaps(21) + rremove("legend") + rremove("y.title"), 
                            T15.heatmaps(22) + rremove("legend"), T15.heatmaps(23) + rremove("legend") + rremove("y.title"), T15.heatmaps(24) + rremove("legend") + rremove("y.title"), ncol = 3, nrow = 4)

TIAS2015_plot2 # view second aggregated plot, export manually as png with width 3000 height 2000   

TIAS2015_plot3 <- ggarrange(T15.heatmaps(25) + rremove("legend"), T15.heatmaps(26) + rremove("legend") + rremove("y.title"), T15.heatmaps(27) + rremove("legend") + rremove("y.title"),
                            T15.heatmaps(28) + rremove("legend"), T15.heatmaps(29) + rremove("legend") + rremove("y.title"), T15.heatmaps(30) + rremove("y.title") + rremove("legend"), 
                            T15.heatmaps(31) + rremove("legend"), T15.heatmaps(32) + rremove("legend") + rremove("y.title"), T15.heatmaps(33) + rremove("y.title"), ncol = 3, nrow = 3) 

TIAS2015_plot3 # view third aggregated plot, export manually as png with width 3000 height 1500   

#####################
# TIAS 2017
#####################

# clear global environment

rm(list=ls())

# import data again 

TIAS <- read.csv("https://raw.githubusercontent.com/carolinelee78/FTL/main/data/raw/PSID/TIAS/TIAS.csv")
TIAS$ID <- seq.int(nrow(TIAS))

# subset for wave  

TIAS2017 <- TIAS[!is.na(TIAS$TAS17),]

# manually paste in variable names and corresponding ftl criteria values from var table 

TIAS2017$CAT <- with(TIAS2017, ifelse(
  TA170058 == 1 & TA170059 %in% c("1", "96") & TA170790 %in% c("5", "0") & TA170416 %in% c("5", "0") & TA170183 == 3 & TA171827 < 60 & 
    TA170909 == 0 & TA170912 == 0 & TA170913 == 0 & TA170866 %in% c("3", "5", "7", "0") & TA171869 == 0 & TA171885 == 0 & TA171835 == 0 & TA171861 == 0 & TA171877 == 0 & 
    TA171835 == 0 & TA171840 == 0 & TA170389 == 0, "FTL_17", "IAC_17"))

table(TIAS2017$CAT)
TIAS2017_FTL <- subset(TIAS2017, CAT == "FTL_17")

TIAS2017 <- TIAS2017 %>% select(ID, CAT, TA170058, TA170059, TA170790, TA170416, TA170183, TA171827, TA170909, TA170912, TA170913, TA170866, TA171869, TA171885, TA171835, TA171861, TA171877, TA171840, TA170389)

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
TIAS2017$TA171835_M <- TIAS2017$TA171835 == 0
TIAS2017$TA171861_M <- TIAS2017$TA171861 == 0
TIAS2017$TA171877_M <- TIAS2017$TA171877 == 0
TIAS2017$TA171840_M <- TIAS2017$TA171840 == 0
TIAS2017$TA170389_M <- TIAS2017$TA170389 == 0

TIAS2017[TIAS2017$CAT == "FTL_17", "ID"]
TIAS2017_FTL <- subset(TIAS2017, CAT == "FTL_17")
  
T2017M <- TIAS2017 %>% select(TA170058_M, TA170059_M, TA170790_M, TA170416_M, TA170183_M, TA171827_M, TA170909_M, TA170912_M, TA170913_M, TA170866_M, TA171869_M, TA171885_M, TA171835_M, TA171861_M, TA171877_M, TA171840_M, TA170389_M, ID)
  
cols <- sapply(T2017M, is.logical)
T2017M[,cols] <- lapply(T2017M[,cols], as.numeric)

ncol(T2017M) # last column is 'ID', so the column range to plug in tidy.vars would be 1:(ncol-1)

nrow(T2017M) # find out how many rows are in T2015M, value of the chunk should the no. of columns (participants) you would want for each subplot 
chunk <- 50 
n <- nrow(T2017M)
r <- rep(1:ceiling(n/chunk), each=chunk)[1:n]

T2017M_list <- split(T2017M, r)
length(T2017M_list) # find out how many chunks have been created, i for functions would be 1:(# of chunks)

tidy.vars <- function(x){
  x %>% tidyr::gather(variable, met_FTL_crt, 1:17)
}

T2017M_tidy_list <- lapply(T2017M_list, tidy.vars)

# manually paste in variable names as strings to define levels for the factors 

for(i in 1:51) {
  T2017M_tidy_list[[i]]$variable <- factor(T2017M_tidy_list[[i]]$variable, levels = c("TA170058_M", "TA170059_M", "TA170790_M", "TA170416_M", "TA170183_M", "TA171827_M", "TA170909_M", "TA170912_M", "TA170913_M", "TA170866_M", "TA171869_M", "TA171885_M", "TA171835_M", "TA171861_M", "TA171877_M", "TA171840_M", "TA170389_M"))
} 

set.ID.levels <- function(x){
  dplyr::pull(x, ID)
}

T2017M_levels_list <- lapply(T2017M_list, set.ID.levels)

for(i in 1:51) {
  T2017M_tidy_list[[i]]$ID <- factor(T2017M_tidy_list[[i]]$ID, levels = T2017M_levels_list[[i]])
}

for(i in 1:51) {
  T2017M_tidy_list[[i]]$met_FTL_crt <- factor(T2017M_tidy_list[[i]]$met_FTL_crt)
}

create.heatmap <- function(x){
  ggplot(x, aes(x=ID, y=variable, fill=met_FTL_crt)) + geom_tile(color="white", size=0.5) +
    coord_equal() +
    labs(x="ID", y="Criteria") +
    theme_tufte(base_family="Helvetica") +
    theme(axis.ticks=element_blank()) + 
    theme(axis.text.x=element_text(angle = 45, hjust = 1)) + 
    scale_fill_discrete(name = "Met FTL Criteria", labels = c("No", "Yes"))
}

T17.heatmaps <- function(x){
  create.heatmap(T2017M_tidy_list[[x]])
}

# need to arrange plots with ggarrange manually - depending on how many plots you have for the wave, remove y.title for all plots except for the leftmost plots, remove legends for all plots except for the last plot

TIAS2017_plot1 <- ggarrange(T17.heatmaps(1) + rremove("legend"), T17.heatmaps(2) + rremove("legend") + rremove("y.title"), T17.heatmaps(3) + rremove("legend") + rremove("y.title"), 
                            T17.heatmaps(4) + rremove("legend"), T17.heatmaps(5) + rremove("legend") + rremove("y.title"), T17.heatmaps(6) + rremove("legend") + rremove("y.title"), 
                            T17.heatmaps(7) + rremove("legend"), T17.heatmaps(8) + rremove("legend") + rremove("y.title"), T17.heatmaps(9) + rremove("legend") + rremove("y.title"), 
                            T17.heatmaps(10) + rremove("legend"), T17.heatmaps(11) + rremove("legend") + rremove("y.title"), T17.heatmaps(12) + rremove("legend") + rremove("y.title"), ncol = 3, nrow = 4) 
                            
TIAS2017_plot1 # view first aggregated plot, export manually as png with width 3000 height 2000 

TIAS2017_plot2 <- ggarrange(T17.heatmaps(13) + rremove("legend"), T17.heatmaps(14) + rremove("legend") + rremove("y.title"), T17.heatmaps(15) + rremove("y.title") + rremove("legend"), 
                            T17.heatmaps(16) + rremove("legend"), T17.heatmaps(17) + rremove("legend") + rremove("y.title"), T17.heatmaps(18) + rremove("legend") + rremove("y.title"), 
                            T17.heatmaps(19) + rremove("legend"), T17.heatmaps(20) + rremove("legend") + rremove("y.title"), T17.heatmaps(21) + rremove("legend") + rremove("y.title"), 
                            T17.heatmaps(22) + rremove("legend"), T17.heatmaps(23) + rremove("legend") + rremove("y.title"), T17.heatmaps(24) + rremove("legend") + rremove("y.title"), ncol = 3, nrow = 4)

TIAS2017_plot2 # view second aggregated plot, export manually as png with width 3000 height 2800   

TIAS2017_plot3 <- ggarrange(T17.heatmaps(25) + rremove("legend"), T17.heatmaps(26) + rremove("legend") + rremove("y.title"), T17.heatmaps(27) + rremove("legend") + rremove("y.title"),
                            T17.heatmaps(28) + rremove("legend"), T17.heatmaps(29) + rremove("legend") + rremove("y.title"), T17.heatmaps(30) + rremove("y.title") + rremove("legend"), 
                            T17.heatmaps(31) + rremove("legend"), T17.heatmaps(32) + rremove("legend") + rremove("y.title"), T17.heatmaps(33) + rremove("y.title"), ncol = 3, nrow = 3) 

TIAS2017_plot3 # view third aggregated plot, export manually as png with width 3000 height 2800   

TIAS2017_plot4 <- ggarrange(T17.heatmaps(34) + rremove("legend"), T17.heatmaps(35) + rremove("legend") + rremove("y.title"), T17.heatmaps(36) + rremove("legend") + rremove("y.title"),
                            T17.heatmaps(37) + rremove("legend"), T17.heatmaps(38) + rremove("legend") + rremove("y.title"), T17.heatmaps(39) + rremove("y.title") + rremove("legend"), 
                            T17.heatmaps(40) + rremove("legend"), T17.heatmaps(41) + rremove("legend") + rremove("y.title"), T17.heatmaps(42) + rremove("y.title"), ncol = 3, nrow = 3) 

TIAS2017_plot4 # view fourth aggregated plot, export manually as png with width 3000 height 2800   

TIAS2017_plot5 <- ggarrange(T17.heatmaps(43) + rremove("legend"), T17.heatmaps(44) + rremove("legend") + rremove("y.title"), T17.heatmaps(45) + rremove("legend") + rremove("y.title"),
                            T17.heatmaps(46) + rremove("legend"), T17.heatmaps(47) + rremove("legend") + rremove("y.title"), T17.heatmaps(48) + rremove("y.title") + rremove("legend"), 
                            T17.heatmaps(49) + rremove("legend"), T17.heatmaps(50) + rremove("legend") + rremove("y.title"), T17.heatmaps(51) + rremove("y.title"), ncol = 3, nrow = 3) 

TIAS2017_plot5 # view fifth aggregated plot, export manually as png with width 3000 height 2800   

#####################
# TIAS 2005-2017
#####################

# clear global environment

rm(list=ls())

# import data again 

TIAS <- read.csv("https://raw.githubusercontent.com/carolinelee78/FTL/main/data/raw/PSID/TIAS/TIAS.csv")
TIAS$ID <- seq.int(nrow(TIAS))

# TIAS 2005 IAC vs. FTL 

TIAS2005 <- TIAS[!is.na(TIAS$TAS05),]
nrow(TIAS2005)

TIAS2005$CAT <- with(TIAS2005, ifelse(
   TA050042 == 1 & TA050043 %in% c("1", "96") & TA050595 %in% c("5", "0") & TA050631 %in% c("5", "0") & TA050127 == 3 & TA050769 < 60 & 
   TA050712 == 0 & TA050715 == 0 & TA050716 == 0 & TA050678 %in% c("3", "5", "7", "0") & TA050785 == 0 & TA050809 == 0 & TA050793 == 0 & TA050777 == 0 & TA050825 == 0 & 
   TA050817 == 0 & TA050798 == 0 & TA050394 %in% c("1", "2", "3", "4", "7", "8", "97", "99"), "FTL_05", ifelse(
      TA050042 == 1 & TA050043 %in% c("1", "96") & TA050595 %in% c("5", "0") & TA050631 %in% c("5", "0") & TA050127 == 3 & TA050769 < 60 & 
      TA050712 == 0 & TA050715 == 0 & TA050716 == 0 & TA050678 %in% c("3", "5", "7", "0") & TA050785 == 0 & TA050809 == 0 & TA050793 == 0 & TA050777 == 0 & TA050825 == 0 & 
      TA050817 == 0 & TA050798 == 0 & TA050394 == 0 & TA050371 == 1, "FTL_05", "IAC_05")))

T05_ID <- TIAS2005$ID
FTL05_ID <- TIAS2005[TIAS2005$CAT == "FTL_05", "ID"]
print(FTL05_ID)
TIAS$CAT_05 <- with(TIAS, ifelse(
  ID %in% FTL05_ID, "FTL_05", ifelse(
    ID %in% T05_ID, "IAC_05", NA)))

# TIAS 2007 IAC vs. FTL 

TIAS2007 <- TIAS[!is.na(TIAS$TAS07),]

TIAS2007$CAT <- with(TIAS2007, ifelse(
  TA070042 == 1 & TA070043 %in% c("1", "96") & TA070570 %in% c("5", "0") & TA070602 %in% c("5", "0") & TA070127 == 3 & TA070740 < 60 & 
  TA070683 == 0 & TA070686 == 0 & TA070687 == 0 & TA070649 %in% c("3", "5", "7", "0") & TA070756 == 0 & TA070777 == 0 & TA070764 == 0 & TA070748 == 0 & TA070793 == 0 & 
  TA070785 == 0 & TA070769 == 0 & TA070368 %in% c("1", "2", "3", "4", "7", "8", "97", "99"), "FTL_07", ifelse(
      TA070042 == 1 & TA070043 %in% c("1", "96") & TA070570 %in% c("5", "0") & TA070602 %in% c("5", "0") & TA070127 == 3 & TA070740 < 60 & 
      TA070683 == 0 & TA070686 == 0 & TA070687 == 0 & TA070649 %in% c("3", "5", "7", "0") & TA070756 == 0 & TA070777 == 0 & TA070764 == 0 & TA070748 == 0 & TA070793 == 0 & 
      TA070785 == 0 & TA070769 == 0 & TA070368 == 0 & TA070344 == 1, "FTL_07", "IAC_07")))

T07_ID <- TIAS2007$ID
FTL07_ID <- TIAS2007[TIAS2007$CAT == "FTL_07", "ID"]
print(FTL07_ID)
TIAS$CAT_07 <- with(TIAS, ifelse(
  ID %in% FTL07_ID, "FTL_07", ifelse(
    ID %in% T07_ID, "IAC_07", NA)))

# TIAS 2009 IAC vs. FTL 

TIAS2009 <- TIAS[!is.na(TIAS$TAS09),]

TIAS2009$CAT <- with(TIAS2009, ifelse(
  TA090043 == 1 & TA090044 %in% c("1", "96") & TA090612 %in% c("5", "0") & TA090655 %in% c("5", "0") & TA090136 == 3 & TA090799 < 60 & 
  TA090739 == 0 & TA090742 == 0 & TA090743 == 0 & TA090705 %in% c("3", "5", "7", "0") & TA090815 == 0 & TA090836 == 0 & TA090823 == 0 & TA090807 == 0 & TA090852 == 0 & 
  TA090844 == 0 & TA090828 == 0 & TA090385 %in% c("1", "2", "3", "4", "7", "8", "97", "99"), "FTL_09", ifelse(
      TA090043 == 1 & TA090044 %in% c("1", "96") & TA090612 %in% c("5", "0") & TA090655 %in% c("5", "0") & TA090136 == 3 & TA090799 < 60 & 
      TA090739 == 0 & TA090742 == 0 & TA090743 == 0 & TA090705 %in% c("3", "5", "7", "0") & TA090815 == 0 & TA090836 == 0 & TA090823 == 0 & TA090807 == 0 & TA090852 == 0 & 
      TA090844 == 0 & TA090828 == 0 & TA090385 == 0 & TA090361 == 1, "FTL_09", "IAC_07")))

T09_ID <- TIAS2009$ID
FTL09_ID <- TIAS2009[TIAS2009$CAT == "FTL_09", "ID"]
print(FTL09_ID)
TIAS$CAT_09 <- with(TIAS, ifelse(
  ID %in% FTL09_ID, "FTL_09", ifelse(
    ID %in% T09_ID, "IAC_09", NA)))

# TIAS 2011 IAC vs. FTL 

TIAS2011 <- TIAS[!is.na(TIAS$TAS11),]

TIAS2011$CAT <- with(TIAS2011, ifelse(
  TA110044 == 1 & TA110045 %in% c("1", "96") & TA110699 %in% c("5", "0") & TA110743 %in% c("5", "0") & TA110137 == 3 & TA110915 < 60 & 
  TA110829 == 0 & TA110832 == 0 & TA110833 == 0 & TA110793 %in% c("3", "5", "7", "0") & TA110931 == 0 & TA110952 == 0 & TA110939 == 0 & TA110923 == 0 & TA110968 == 0 & 
  TA110960 == 0 & TA110944 == 0 & TA110462 %in% c("1", "2", "3", "4", "7", "8", "97", "99"), "FTL_11", ifelse(
      TA110044 == 1 & TA110045 %in% c("1", "96") & TA110699 %in% c("5", "0") & TA110743 %in% c("5", "0") & TA110137 == 3 & TA110915 < 60 & 
      TA110829 == 0 & TA110832 == 0 & TA110833 == 0 & TA110793 %in% c("3", "5", "7", "0") & TA110931 == 0 & TA110952 == 0 & TA110939 == 0 & TA110923 == 0 & TA110968 == 0 & 
      TA110960 == 0 & TA110944 == 0 & TA110462 == 0 & TA110351 == 1, "FTL_11", "IAC_11")))

T11_ID <- TIAS2011$ID
FTL11_ID <- TIAS2011[TIAS2011$CAT == "FTL_11", "ID"]
print(FTL11_ID)
TIAS$CAT_11 <- with(TIAS, ifelse(
  ID %in% FTL11_ID, "FTL_11", ifelse(
    ID %in% T11_ID, "IAC_11", NA)))

# TIAS 2013 IAC vs. FTL 

TIAS2013 <- TIAS[!is.na(TIAS$TAS13),]

TIAS2013$CAT <- with(TIAS2013, ifelse(
  TA130043 == 1 & TA130044 %in% c("1", "96") & TA130719 %in% c("5", "0") & TA130763 %in% c("5", "0") & TA130136 == 3 & TA130948 < 60 & 
  TA130852 == 0 & TA130855 == 0 & TA130856 == 0 & TA130813 %in% c("3", "5", "7", "0") & TA130961 == 0 & TA130982 == 0 & TA130969 == 0 & TA130861 == 0 & TA130998 == 0 &
  TA130990 == 0 & TA130977 == 0 & TA130482 %in% c("1", "2", "3", "4", "7", "8", "97", "99"), "FTL_13", ifelse(
      TA130043 == 1 & TA130044 %in% c("1", "96") & TA130719 %in% c("5", "0") & TA130763 %in% c("5", "0") & TA130136 == 3 & TA130948 < 60 & 
      TA130852 == 0 & TA130855 == 0 & TA130856 == 0 & TA130813 %in% c("3", "5", "7", "0") & TA130961 == 0 & TA130982 == 0 & TA130969 == 0 & TA130861 == 0 & TA130998 == 0 & 
      TA130990 == 0 & TA130977 == 0 & TA130482 == 0 & TA130350 == 1, "FTL_13", "IAC_13")))

T13_ID <- TIAS2013$ID
FTL13_ID <- TIAS2013[TIAS2013$CAT == "FTL_13", "ID"]
print(FTL13_ID)
TIAS$CAT_13 <- with(TIAS, ifelse(
  ID %in% FTL13_ID, "FTL_13", ifelse(
    ID %in% T13_ID, "IAC_13", NA)))

# TIAS 2015 IAC vs. FTL 

TIAS2015 <- TIAS[!is.na(TIAS$TAS15),]

TIAS2015$CAT <- with(TIAS2015, ifelse(
  TA150043 == 1 & TA150044 %in% c("1", "96") & TA150731 %in% c("5", "0") & TA150776 %in% c("5", "0") & TA150128 == 3 & TA150970 < 60 & 
    TA150869 == 0 & TA150872 == 0 & TA150873 == 0 & TA150826 %in% c("3", "5", "7", "0") & TA150986 == 0 & TA151007 == 0 & TA150994 == 0 & TA150978 == 0 & TA151023 == 0 & 
    TA151015 == 0 & TA150999 == 0 & TA150491 %in% c("1", "2", "3", "4", "7", "8", "97", "99"), "FTL_15", ifelse(
      TA150043 == 1 & TA150044 %in% c("1", "96") & TA150731 %in% c("5", "0") & TA150776 %in% c("5", "0") & TA150128 == 3 & TA150970 < 60 & 
        TA150869 == 0 & TA150872 == 0 & TA150873 == 0 & TA150826 %in% c("3", "5", "7", "0") & TA150986 == 0 & TA151007 == 0 & TA150994 == 0 & TA150978 == 0 & TA151023 == 0 & 
        TA151015 == 0 & TA150999 == 0 & TA150491 == 0 & TA150352 == 1, "FTL_15", "IAC_15")))

T15_ID <- TIAS2015$ID
FTL15_ID <- TIAS2015[TIAS2015$CAT == "FTL_15", "ID"]
print(FTL15_ID)
TIAS$CAT_15 <- with(TIAS, ifelse(
  ID %in% FTL15_ID, "FTL_15", ifelse(
    ID %in% T15_ID, "IAC_15", NA)))

# TIAS 2017 IAC vs. FTL 

TIAS2017 <- TIAS[!is.na(TIAS$TAS17),]

TIAS2017$CAT <- with(TIAS2017, ifelse(
  TA170058 == 1 & TA170059 %in% c("1", "96") & TA170790 %in% c("5", "0") & TA170416 %in% c("5", "0") & TA170183 == 3 & TA171827 < 60 & 
    TA170909 == 0 & TA170912 == 0 & TA170913 == 0 & TA170866 %in% c("3", "5", "7", "0") & TA171869 == 0 & TA171885 == 0 & TA171835 == 0 & TA171861 == 0 & TA171877 == 0 & 
    TA171835 == 0 & TA171840 == 0 & TA170389 == 0, "FTL_17", "IAC_17"))

T17_ID <- TIAS2017$ID
FTL17_ID <- TIAS2017[TIAS2017$CAT == "FTL_17", "ID"]
print(FTL17_ID)
TIAS$CAT_17 <- with(TIAS, ifelse(
  ID %in% FTL17_ID, "FTL_17", ifelse(
    ID %in% T17_ID, "IAC_17", NA)))

# view final dataset 

TIAS_FINAL <- TIAS %>% select(ID, CAT_05, CAT_07, CAT_09, CAT_13, CAT_15, CAT_17, TA050042, TA050043, TA050595, TA050631, TA050127, TA050769, TA050712, TA050715, TA050716, TA050678, TA050785, TA050809, TA050793, TA050777, TA050825, TA050817, TA050798, TA050394, TA050371,
                              TA070042, TA070043, TA070570, TA070602, TA070127, TA070740, TA070683, TA070686, TA070687, TA070649, TA070756, TA070777, TA070764, TA070748, TA070793, TA070785, TA070769, TA070368, TA070344,
                              TA090043, TA090044, TA090612, TA090655, TA090136, TA090799, TA090739, TA090742, TA090743, TA090705, TA090815, TA090836, TA090823, TA090807, TA090852, TA090844, TA090828, TA090385, TA090361,
                              TA110044, TA110045, TA110699, TA110743, TA110137, TA110915, TA110829, TA110832, TA110833, TA110793, TA110931, TA110952, TA110939, TA110923, TA110968, TA110960, TA110944, TA110462, TA110351,
                              TA130043, TA130044, TA130719, TA130763, TA130136, TA130948, TA130852, TA130855, TA130856, TA130813, TA130961, TA130982, TA130969, TA130861, TA130998, TA130990, TA130977, TA130482, TA130350,
                              TA150043, TA150044, TA150731, TA150776, TA150128, TA150970, TA150869, TA150872, TA150873, TA150826, TA150986, TA151007, TA150994, TA150978, TA151023, TA151015, TA150999, TA150491, TA150352,
                              TA170058, TA170059, TA170790, TA170416, TA170183, TA171827, TA170909, TA170912, TA170913, TA170866, TA171869, TA171885, TA171835, TA171861, TA171877, TA171840, TA170389)









