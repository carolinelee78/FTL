#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects

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

TIAS2005 <- TIAS[!is.na(TIAS$TAS05),]
nrow(TIAS2005)

# manually paste in variable names and corresponding ftl criteria values from var table 

TIAS2005$CAT <- with(TIAS2005, ifelse(
  TA050042 == 1 & TA050043 == 1 & TA050595 %in% c("5", "0") & TA050631 %in% c("5", "0") & TA050127 == 3 & TA050769 < 60 & 
    TA050712 == 0 & TA050715 == 0 & TA050716 == 0 & TA050678 %in% c("3", "5", "7", "0") & TA050785 == 0 & TA050809 == 0 & TA050793 == 0 & TA050777 == 0 & TA050825 == 0 & 
    TA050817 == 0 & TA050798 == 0 & TA050394 %in% c("1", "2", "3", "4", "7", "8", "97", "99"), "FTL_05", ifelse(
      TA050042 == 1 & TA050043 == 1 & TA050595 %in% c("5", "0") & TA050631 %in% c("5", "0") & TA050127 == 3 & TA050769 < 60 & 
        TA050712 == 0 & TA050715 == 0 & TA050716 == 0 & TA050678 %in% c("3", "5", "7", "0") & TA050785 == 0 & TA050809 == 0 & TA050793 == 0 & TA050777 == 0 & TA050825 == 0 & 
        TA050817 == 0 & TA050798 == 0 & TA050394 == 0 & TA050371 %in% c("5", "8", "9"), "FTL_05", "IAC_05")))

table(TIAS2005$CAT)

# manually paste in variable names from var table

TIAS2005 <- TIAS2005 %>% select(ID, CAT, TA050042, TA050043, TA050595, TA050631, TA050127, TA050769, TA050712, TA050715, TA050716, TA050678, 
                                TA050785, TA050809, TA050793, TA050777, TA050825, TA050817, TA050798, TA050394, TA050371)

TIAS2005$TA050042_M <- TIAS2005$TA050042 == 1 
TIAS2005$TA050043_M <- TIAS2005$TA050043 == 1
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
    TA050394 == 0 & TA050371 %in% c("5", "8", "9"), TRUE, FALSE)))

table(TIAS2005$TA050678_M)

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

# need to arrange plots with ggarrange manually, depending on how many plots you have for the wave, remove y.title for except for the leftmost plots, remove legends for all plots except for the last plot

TIAS2005_plot <- ggarrange(T05.heatmaps(1) + rremove("legend"), T05.heatmaps(2) + rremove("legend") + rremove("y.title"), T05.heatmaps(3) + rremove("legend") + rremove("y.title"), T05.heatmaps(4) + rremove("legend"), 
                           T05.heatmaps(5) + rremove("legend") + rremove("y.title"), T05.heatmaps(6) + rremove("legend") + rremove("y.title"), T05.heatmaps(7) + rremove("legend"), T05.heatmaps(8) + rremove("legend") + rremove("y.title"), 
                           T05.heatmaps(9) + rremove("legend") + rremove("y.title"), T05.heatmaps(10) + rremove("legend"), T05.heatmaps(11) + rremove("legend") + rremove("y.title"), T05.heatmaps(12) + rremove("legend") + rremove("y.title"),
                           T05.heatmaps(13) + rremove("legend"), T05.heatmaps(14) + rremove("legend") + rremove("y.title"), T05.heatmaps(15) + rremove("y.title"), ncol = 3, nrow = 5) 

TIAS2005_plot 

#####################
# TIAS 2007 
#####################

# remove objects

rm(list=ls())

# import data again 

TIAS <- read.csv("https://raw.githubusercontent.com/carolinelee78/FTL/main/data/raw/PSID/TIAS/TIAS.csv")
TIAS$ID <- seq.int(nrow(TIAS))

TIAS2007 <- TIAS[!is.na(TIAS$TAS07),]

# manually paste in variable names and corresponding ftl criteria values from var table 

TIAS2007$CAT <- with(TIAS2007, ifelse(
  TA070042 == 1 & TA070043 == 1 & TA070570 %in% c("5", "0") & TA070602 %in% c("5", "0") & TA070127 == 3 & TA070740 < 60 & 
    TA070683 == 0 & TA070686 == 0 & TA070687 == 0 & TA070649 == 1 & TA070756 == 0 & TA070777 == 0 & TA070764 == 0 & TA070748 == 0 & TA070793 == 0 & 
    TA070785 == 0 & TA070769 == 0 & TA070368 %in% c("9", "10"), "FTL_07", "IAC_07"
  ))

table(TIAS2007$CAT)

# manually paste in variable names from var table

TIAS2007 <- TIAS2007 %>% select(ID, CAT, TA070042, TA070043, TA070570, TA070602, TA070127, TA070740, TA070683, TA070686, TA070687, TA070649, TA070756, 
                                TA070777, TA070764, TA070748, TA070793, TA070785, TA070769, TA070368) 

# manually paste in ftl values for each variable from var table

TIAS2007$TA070042_M <- TIAS2007$TA070042 == 1 
TIAS2007$TA070043_M <- TIAS2007$TA070043 == 1
TIAS2007$TA070570_M <- TIAS2007$TA070570 %in% c("5", "0")
TIAS2007$TA070602_M <- TIAS2007$TA070602 %in% c("5", "0")
TIAS2007$TA070127_M <- TIAS2007$TA070127 == 3
TIAS2007$TA070740_M <- TIAS2007$TA070740 < 60
TIAS2007$TA070683_M <- TIAS2007$TA070683 == 0 
TIAS2007$TA070686_M <- TIAS2007$TA070686 == 0 
TIAS2007$TA070687_M <- TIAS2007$TA070687 == 0 
TIAS2007$TA070649_M <- TIAS2007$TA070649 == 1
TIAS2007$TA070756_M <- TIAS2007$TA070756 == 0
TIAS2007$TA070777_M <- TIAS2007$TA070777 == 0
TIAS2007$TA070764_M <- TIAS2007$TA070764 == 0
TIAS2007$TA070748_M <- TIAS2007$TA070748 == 0
TIAS2007$TA070793_M <- TIAS2007$TA070793 == 0
TIAS2007$TA070785_M <- TIAS2007$TA070785 == 0
TIAS2007$TA070769_M <- TIAS2007$TA070769 == 0
TIAS2007$TA070368_M <- TIAS2007$TA070368 %in% c("9", "10")

T2007M <- TIAS2007 %>% select(TA070042_M, TA070043_M, TA070570_M, TA070602_M, TA070127_M, TA070740_M, TA070683_M, TA070686_M, TA070687_M, TA070649_M, TA070756_M, 
                              TA070777_M, TA070764_M, TA070748_M, TA070793_M, TA070785_M, TA070769_M, TA070368_M, ID) 
  
cols <- sapply(T2007M, is.logical)
T2007M[,cols] <- lapply(T2007M[,cols], as.numeric)

ncol(T2007M) # last column is 'ID', so the column range to plug in tidy.vars would be 1:(ncol-1)

nrow(T2007M) # find out how many rows are in T2007M, value of the chunk should the no. of columns (participants) you would want for each subplot 
chunk <- 50 
n <- nrow(T2007M)
r <- rep(1:ceiling(n/chunk), each=chunk)[1:n]

T2007M_list <- split(T2007M, r)
length(T2007M_list) # find out how many chunks have been created, i for functions would be 1:# of chunks

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

# need to arrange plots with ggarrange manually, depending on how many plots you have for the wave, remove y.title for except for the leftmost plots, remove legends for all plots except for the last plot

TIAS2007_plot1 <- ggarrange(T07.heatmaps(1) + rremove("legend"), T07.heatmaps(2) + rremove("legend") + rremove("y.title"), T07.heatmaps(3) + rremove("legend") + rremove("y.title"), T07.heatmaps(4) + rremove("legend"), 
                            T07.heatmaps(5) + rremove("legend") + rremove("y.title"), T07.heatmaps(6) + rremove("legend") + rremove("y.title"), T07.heatmaps(7) + rremove("legend"), T07.heatmaps(8) + rremove("legend") + rremove("y.title"), 
                            T07.heatmaps(9) + rremove("legend") + rremove("y.title"), T07.heatmaps(10) + rremove("legend"), T07.heatmaps(11) + rremove("legend") + rremove("y.title"), T07.heatmaps(12) + rremove("legend") + rremove("y.title"), ncol = 3, nrow = 4) 

TIAS2007_plot1 # view first aggregated plot 

TIAS2007_plot2 <- ggarrange(T07.heatmaps(13) + rremove("legend"), T07.heatmaps(14) + rremove("legend") + rremove("y.title"), T07.heatmaps(15) + rremove("legend") + rremove("y.title"), T07.heatmaps(16) + rremove("legend"), 
                            T07.heatmaps(17) + rremove("legend") + rremove("y.title"), T07.heatmaps(18) + rremove("legend") + rremove("y.title"), T07.heatmaps(19) + rremove("legend"), T07.heatmaps(20) + rremove("legend") + rremove("y.title"), 
                            T07.heatmaps(21) + rremove("legend") + rremove("y.title"), T07.heatmaps(22) + rremove("legend"), T07.heatmaps(23) + rremove("y.title"), ncol = 3, nrow = 4) 

TIAS2007_plot2 # view second aggregated plot 






