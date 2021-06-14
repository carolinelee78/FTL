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
setwd("/Users/kdlee/Desktop/FTL/TIAS")

# import data 
TIAS <- read.csv("https://raw.githubusercontent.com/carolinelee78/FTL/main/data/raw/PSID/TIAS/TIAS.csv")
TIAS$ID <- seq.int(nrow(TIAS))

#####################
# TIAS 2005 
#####################

TIAS2005 <- TIAS[!is.na(TIAS$TAS05),]
nrow(TIAS2005)

TIAS2005$CAT <- with(TIAS2005, ifelse(
  TA050042 == 1 & TA050043 == 1 & TA050595 %in% c("5", "0") & TA050631 %in% c("5", "0") & TA050127 == 3 & TA050769 < 60 & 
    TA050712 == 0 & TA050715 == 0 & TA050716 == 0 & TA050678 == 1 & TA050785 == 0 & TA050809 == 0 & TA050793 == 0 & TA050777 == 0 & TA050825 == 0 & 
      TA050817 == 0 & TA050798 == 0 & TA050394 %in% c("9", "10"), "FTL_05", "IAC_05"
        ))

table(TIAS2005$CAT)

TIAS2005 <- TIAS2005 %>% select(ID, CAT, TA050042, TA050043, TA050595, TA050631, TA050127, TA050769, TA050712, TA050715, TA050716, TA050678, 
                                    TA050785, TA050809, TA050793, TA050777, TA050825, TA050817, TA050798, TA050394)

TIAS2005$TA050042_M <- TIAS2005$TA050042 == 1 
TIAS2005$TA050043_M <- TIAS2005$TA050043 == 1
TIAS2005$TA050595_M <- TIAS2005$TA050595 %in% c("5", "0")
TIAS2005$TA050631_M <- TIAS2005$TA050631 %in% c("5", "0")
TIAS2005$TA050127_M <- TIAS2005$TA050127 == 3
TIAS2005$TA050769_M <- TIAS2005$TA050769 < 60
TIAS2005$TA050712_M <- TIAS2005$TA050712 == 0 
TIAS2005$TA050715_M <- TIAS2005$TA050715 == 0 
TIAS2005$TA050716_M <- TIAS2005$TA050716 == 0 
TIAS2005$TA050678_M <- TIAS2005$TA050678 == 1
TIAS2005$TA050785_M <- TIAS2005$TA050785 == 0
TIAS2005$TA050809_M <- TIAS2005$TA050809 == 0
TIAS2005$TA050793_M <- TIAS2005$TA050793 == 0
TIAS2005$TA050777_M <- TIAS2005$TA050777 == 0
TIAS2005$TA050825_M <- TIAS2005$TA050825 == 0
TIAS2005$TA050817_M <- TIAS2005$TA050817 == 0
TIAS2005$TA050798_M <- TIAS2005$TA050798 == 0
TIAS2005$TA050394_M <- TIAS2005$TA050394 %in% c("9", "10")


T2005M <- TIAS2005 %>% select(TA050042_M, TA050043_M, TA050595_M, TA050631_M, TA050127_M, TA050769_M, TA050712_M, TA050715_M, TA050716_M,
                              TA050678_M, TA050785_M, TA050809_M, TA050793_M, TA050777_M, TA050825_M, TA050817_M, TA050798_M, TA050394_M, ID)

cols <- sapply(T2005M, is.logical)
T2005M[,cols] <- lapply(T2005M[,cols], as.numeric)

# lines 1-50 of logical match data for TIAS 2005 FTL criteria

T2005M_S1 <- T2005M[1:50,]

T2005M_S1_tidy <- T2005M_S1 %>% tidyr::gather(variable, met_FTL_crt, 1:18)

T2005M_S1_tidy$variable <- factor(T2005M_S1_tidy$variable, levels =c("TA050042_M", "TA050043_M", "TA050595_M", "TA050631_M", "TA050127_M", "TA050769_M", "TA050712_M", "TA050715_M", "TA050716_M",
                                                             "TA050678_M", "TA050785_M", "TA050809_M", "TA050793_M", "TA050777_M", "TA050825_M", "TA050817_M", "TA050798_M", "TA050394_M"))
S1_ID_levels <- dplyr::pull(T2005M_S1, ID)
T2005M_S1_tidy$ID <- factor(T2005M_S1_tidy$ID, levels = S1_ID_levels)

T2005M_S1_tidy$met_FTL_crt<- factor(T2005M_S1_tidy$met_FTL_crt)

ftl_plot1 <- ggplot(T2005M_S1_tidy, aes(x=ID, y=variable, fill=met_FTL_crt)) + geom_tile(color="white", size=0.5) +
  coord_equal() +
  labs(x="ID", y="Criteria") +
  theme_tufte(base_family="Helvetica") +
  theme(axis.ticks=element_blank()) + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
ftl_plot1 + scale_fill_discrete(name = "Met FTL Criteria", labels = c("No", "Yes"))
ggsave("TIAS2005_CRT_PLOT1.png")

# lines 50-100 of logical match data for TIAS 2005 FTL criteria

T2005M_S2 <- T2005M[51:100,]

T2005M_S2_tidy <- T2005M_S2 %>% tidyr::gather(variable, met_FTL_crt, 1:18)

T2005M_S2_tidy$variable <- factor(T2005M_S2_tidy$variable, levels =c("TA050042_M", "TA050043_M", "TA050595_M", "TA050631_M", "TA050127_M", "TA050769_M", "TA050712_M", "TA050715_M", "TA050716_M",
                                                                     "TA050678_M", "TA050785_M", "TA050809_M", "TA050793_M", "TA050777_M", "TA050825_M", "TA050817_M", "TA050798_M", "TA050394_M"))
S2_ID_levels <- dplyr::pull(T2005M_S2, ID)
T2005M_S2_tidy$ID <- factor(T2005M_S2_tidy$ID, levels = S2_ID_levels)

T2005M_S2_tidy$met_FTL_crt<- factor(T2005M_S2_tidy$met_FTL_crt)

ftl_plot2 <- ggplot(T2005M_S2_tidy, aes(x=ID, y=variable, fill=met_FTL_crt)) + geom_tile(color="white", size=0.5) +
  coord_equal() +
  labs(x="ID", y="Criteria") +
  theme_tufte(base_family="Helvetica") +
  theme(axis.ticks=element_blank()) + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
ftl_plot2 + scale_fill_discrete(name = "Met FTL Criteria", labels = c("No", "Yes"))
ggsave("TIAS2005_CRT_PLOT2.png")

# lines 101-150 of logical match data for TIAS 2005 FTL criteria

T2005M_S3 <- T2005M[101:150,]

T2005M_S3_tidy <- T2005M_S3 %>% tidyr::gather(variable, met_FTL_crt, 1:18)

T2005M_S3_tidy$variable <- factor(T2005M_S3_tidy$variable, levels =c("TA050042_M", "TA050043_M", "TA050595_M", "TA050631_M", "TA050127_M", "TA050769_M", "TA050712_M", "TA050715_M", "TA050716_M",
                                                                     "TA050678_M", "TA050785_M", "TA050809_M", "TA050793_M", "TA050777_M", "TA050825_M", "TA050817_M", "TA050798_M", "TA050394_M"))
S3_ID_levels <- dplyr::pull(T2005M_S3, ID)
T2005M_S3_tidy$ID <- factor(T2005M_S3_tidy$ID, levels = S3_ID_levels)

T2005M_S3_tidy$met_FTL_crt<- factor(T2005M_S3_tidy$met_FTL_crt)

ftl_plot3 <- ggplot(T2005M_S3_tidy, aes(x=ID, y=variable, fill=met_FTL_crt)) + geom_tile(color="white", size=0.5) +
  coord_equal() +
  labs(x="ID", y="Criteria") +
  theme_tufte(base_family="Helvetica") +
  theme(axis.ticks=element_blank()) + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
ftl_plot3 + scale_fill_discrete(name = "Met FTL Criteria", labels = c("No", "Yes"))
ggsave("TIAS2005_CRT_PLOT3.png")

# lines 151:200 of logical match data for TIAS 2005 FTL criteria

T2005M_S4 <- T2005M[151:200,]

T2005M_S4_tidy <- T2005M_S4 %>% tidyr::gather(variable, met_FTL_crt, 1:18)

T2005M_S4_tidy$variable <- factor(T2005M_S4_tidy$variable, levels =c("TA050042_M", "TA050043_M", "TA050595_M", "TA050631_M", "TA050127_M", "TA050769_M", "TA050712_M", "TA050715_M", "TA050716_M",
                                                                     "TA050678_M", "TA050785_M", "TA050809_M", "TA050793_M", "TA050777_M", "TA050825_M", "TA050817_M", "TA050798_M", "TA050394_M"))
S4_ID_levels <- dplyr::pull(T2005M_S4, ID)
T2005M_S4_tidy$ID <- factor(T2005M_S4_tidy$ID, levels = S4_ID_levels)

T2005M_S4_tidy$met_FTL_crt<- factor(T2005M_S4_tidy$met_FTL_crt)

ftl_plot4 <- ggplot(T2005M_S4_tidy, aes(x=ID, y=variable, fill=met_FTL_crt)) + geom_tile(color="white", size=0.5) +
  coord_equal() +
  labs(x="ID", y="Criteria") +
  theme_tufte(base_family="Helvetica") +
  theme(axis.ticks=element_blank()) + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
ftl_plot4 + scale_fill_discrete(name = "Met FTL Criteria", labels = c("No", "Yes"))
ggsave("TIAS2005_CRT_PLOT4.png")

# lines 201-250 of logical match data for TIAS 2005 FTL criteria

T2005M_S5 <- T2005M[201:250,]

T2005M_S5_tidy <- T2005M_S5 %>% tidyr::gather(variable, met_FTL_crt, 1:18)

T2005M_S5_tidy$variable <- factor(T2005M_S5_tidy$variable, levels =c("TA050042_M", "TA050043_M", "TA050595_M", "TA050631_M", "TA050127_M", "TA050769_M", "TA050712_M", "TA050715_M", "TA050716_M",
                                                                     "TA050678_M", "TA050785_M", "TA050809_M", "TA050793_M", "TA050777_M", "TA050825_M", "TA050817_M", "TA050798_M", "TA050394_M"))
S5_ID_levels <- dplyr::pull(T2005M_S5, ID)
T2005M_S5_tidy$ID <- factor(T2005M_S5_tidy$ID, levels = S5_ID_levels)

T2005M_S5_tidy$met_FTL_crt<- factor(T2005M_S5_tidy$met_FTL_crt)

ftl_plot5 <- ggplot(T2005M_S5_tidy, aes(x=ID, y=variable, fill=met_FTL_crt)) + geom_tile(color="white", size=0.5) +
  coord_equal() +
  labs(x="ID", y="Criteria") +
  theme_tufte(base_family="Helvetica") +
  theme(axis.ticks=element_blank()) + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
ftl_plot5 + scale_fill_discrete(name = "Met FTL Criteria", labels = c("No", "Yes"))
ggsave("TIAS2005_CRT_PLOT5.png")

# lines 251-300 of logical match data for TIAS 2005 FTL criteria

T2005M_S6 <- T2005M[251:300,]

T2005M_S6_tidy <- T2005M_S6 %>% tidyr::gather(variable, met_FTL_crt, 1:18)

T2005M_S6_tidy$variable <- factor(T2005M_S6_tidy$variable, levels =c("TA050042_M", "TA050043_M", "TA050595_M", "TA050631_M", "TA050127_M", "TA050769_M", "TA050712_M", "TA050715_M", "TA050716_M",
                                                                     "TA050678_M", "TA050785_M", "TA050809_M", "TA050793_M", "TA050777_M", "TA050825_M", "TA050817_M", "TA050798_M", "TA050394_M"))
S6_ID_levels <- dplyr::pull(T2005M_S6, ID)
T2005M_S6_tidy$ID <- factor(T2005M_S6_tidy$ID, levels = S6_ID_levels)

T2005M_S6_tidy$met_FTL_crt<- factor(T2005M_S6_tidy$met_FTL_crt)

ftl_plot6 <- ggplot(T2005M_S6_tidy, aes(x=ID, y=variable, fill=met_FTL_crt)) + geom_tile(color="white", size=0.5) +
  coord_equal() +
  labs(x="ID", y="Criteria") +
  theme_tufte(base_family="Helvetica") +
  theme(axis.ticks=element_blank()) + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
ftl_plot6 + scale_fill_discrete(name = "Met FTL Criteria", labels = c("No", "Yes"))
ggsave("TIAS2005_CRT_PLOT6.png")

# lines 301-350 of logical match data for TIAS 2005 FTL criteria

T2005M_S7 <- T2005M[301:350,]

T2005M_S7_tidy <- T2005M_S7 %>% tidyr::gather(variable, met_FTL_crt, 1:18)

T2005M_S7_tidy$variable <- factor(T2005M_S7_tidy$variable, levels =c("TA050042_M", "TA050043_M", "TA050595_M", "TA050631_M", "TA050127_M", "TA050769_M", "TA050712_M", "TA050715_M", "TA050716_M",
                                                                     "TA050678_M", "TA050785_M", "TA050809_M", "TA050793_M", "TA050777_M", "TA050825_M", "TA050817_M", "TA050798_M", "TA050394_M"))
S7_ID_levels <- dplyr::pull(T2005M_S7, ID)
T2005M_S7_tidy$ID <- factor(T2005M_S7_tidy$ID, levels = S7_ID_levels)

T2005M_S7_tidy$met_FTL_crt<- factor(T2005M_S7_tidy$met_FTL_crt)

ftl_plot7 <- ggplot(T2005M_S7_tidy, aes(x=ID, y=variable, fill=met_FTL_crt)) + geom_tile(color="white", size=0.5) +
  coord_equal() +
  labs(x="ID", y="Criteria") +
  theme_tufte(base_family="Helvetica") +
  theme(axis.ticks=element_blank()) + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
ftl_plot7 + scale_fill_discrete(name = "Met FTL Criteria", labels = c("No", "Yes"))
ggsave("TIAS2005_CRT_PLOT7.png")

# lines 351-400 of logical match data for TIAS 2005 FTL criteria

T2005M_S8 <- T2005M[351:400,]

T2005M_S8_tidy <- T2005M_S8 %>% tidyr::gather(variable, met_FTL_crt, 1:18)

T2005M_S8_tidy$variable <- factor(T2005M_S8_tidy$variable, levels =c("TA050042_M", "TA050043_M", "TA050595_M", "TA050631_M", "TA050127_M", "TA050769_M", "TA050712_M", "TA050715_M", "TA050716_M",
                                                                     "TA050678_M", "TA050785_M", "TA050809_M", "TA050793_M", "TA050777_M", "TA050825_M", "TA050817_M", "TA050798_M", "TA050394_M"))
S8_ID_levels <- dplyr::pull(T2005M_S8, ID)
T2005M_S8_tidy$ID <- factor(T2005M_S8_tidy$ID, levels = S8_ID_levels)

T2005M_S8_tidy$met_FTL_crt<- factor(T2005M_S8_tidy$met_FTL_crt)

ftl_plot8 <- ggplot(T2005M_S8_tidy, aes(x=ID, y=variable, fill=met_FTL_crt)) + geom_tile(color="white", size=0.5) +
  coord_equal() +
  labs(x="ID", y="Criteria") +
  theme_tufte(base_family="Helvetica") +
  theme(axis.ticks=element_blank()) + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
ftl_plot8 + scale_fill_discrete(name = "Met FTL Criteria", labels = c("No", "Yes"))
ggsave("TIAS2005_CRT_PLOT8.png")

# lines 401-450 of logical match data for TIAS 2005 FTL criteria

T2005M_S9 <- T2005M[401:450,]

T2005M_S9_tidy <- T2005M_S9 %>% tidyr::gather(variable, met_FTL_crt, 1:18)

T2005M_S9_tidy$variable <- factor(T2005M_S9_tidy$variable, levels =c("TA050042_M", "TA050043_M", "TA050595_M", "TA050631_M", "TA050127_M", "TA050769_M", "TA050712_M", "TA050715_M", "TA050716_M",
                                                                     "TA050678_M", "TA050785_M", "TA050809_M", "TA050793_M", "TA050777_M", "TA050825_M", "TA050817_M", "TA050798_M", "TA050394_M"))
S9_ID_levels <- dplyr::pull(T2005M_S9, ID)
T2005M_S9_tidy$ID <- factor(T2005M_S9_tidy$ID, levels = S9_ID_levels)

T2005M_S9_tidy$met_FTL_crt<- factor(T2005M_S9_tidy$met_FTL_crt)

ftl_plot9 <- ggplot(T2005M_S9_tidy, aes(x=ID, y=variable, fill=met_FTL_crt)) + geom_tile(color="white", size=0.5) +
  coord_equal() +
  labs(x="ID", y="Criteria") +
  theme_tufte(base_family="Helvetica") +
  theme(axis.ticks=element_blank()) + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
ftl_plot9 + scale_fill_discrete(name = "Met FTL Criteria", labels = c("No", "Yes"))
ggsave("TIAS2005_CRT_PLOT9.png")

# lines 451-500 of logical match data for TIAS 2005 FTL criteria

T2005M_S10 <- T2005M[451:500,]

T2005M_S10_tidy <- T2005M_S10 %>% tidyr::gather(variable, met_FTL_crt, 1:18)

T2005M_S10_tidy$variable <- factor(T2005M_S10_tidy$variable, levels =c("TA050042_M", "TA050043_M", "TA050595_M", "TA050631_M", "TA050127_M", "TA050769_M", "TA050712_M", "TA050715_M", "TA050716_M",
                                                                     "TA050678_M", "TA050785_M", "TA050809_M", "TA050793_M", "TA050777_M", "TA050825_M", "TA050817_M", "TA050798_M", "TA050394_M"))
S10_ID_levels <- dplyr::pull(T2005M_S10, ID)
T2005M_S10_tidy$ID <- factor(T2005M_S10_tidy$ID, levels = S10_ID_levels)

T2005M_S10_tidy$met_FTL_crt<- factor(T2005M_S10_tidy$met_FTL_crt)

ftl_plot10 <- ggplot(T2005M_S10_tidy, aes(x=ID, y=variable, fill=met_FTL_crt)) + geom_tile(color="white", size=0.5) +
  coord_equal() +
  labs(x="ID", y="Criteria") +
  theme_tufte(base_family="Helvetica") +
  theme(axis.ticks=element_blank()) + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
ftl_plot10 + scale_fill_discrete(name = "Met FTL Criteria", labels = c("No", "Yes"))
ggsave("TIAS2005_CRT_PLOT10.png")

# lines 501-550 of logical match data for TIAS 2005 FTL criteria

T2005M_S11 <- T2005M[501:550,]

T2005M_S11_tidy <- T2005M_S11 %>% tidyr::gather(variable, met_FTL_crt, 1:18)

T2005M_S11_tidy$variable <- factor(T2005M_S11_tidy$variable, levels =c("TA050042_M", "TA050043_M", "TA050595_M", "TA050631_M", "TA050127_M", "TA050769_M", "TA050712_M", "TA050715_M", "TA050716_M",
                                                                       "TA050678_M", "TA050785_M", "TA050809_M", "TA050793_M", "TA050777_M", "TA050825_M", "TA050817_M", "TA050798_M", "TA050394_M"))
S11_ID_levels <- dplyr::pull(T2005M_S11, ID)
T2005M_S11_tidy$ID <- factor(T2005M_S11_tidy$ID, levels = S11_ID_levels)

T2005M_S11_tidy$met_FTL_crt<- factor(T2005M_S11_tidy$met_FTL_crt)

ftl_plot11 <- ggplot(T2005M_S11_tidy, aes(x=ID, y=variable, fill=met_FTL_crt)) + geom_tile(color="white", size=0.5) +
  coord_equal() +
  labs(x="ID", y="Criteria") +
  theme_tufte(base_family="Helvetica") +
  theme(axis.ticks=element_blank()) + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
ftl_plot11 + scale_fill_discrete(name = "Met FTL Criteria", labels = c("No", "Yes"))
ggsave("TIAS2005_CRT_PLOT11.png")

# lines 551-600 of logical match data for TIAS 2005 FTL criteria

T2005M_S12 <- T2005M[551:600,]

T2005M_S12_tidy <- T2005M_S12 %>% tidyr::gather(variable, met_FTL_crt, 1:18)

T2005M_S12_tidy$variable <- factor(T2005M_S12_tidy$variable, levels =c("TA050042_M", "TA050043_M", "TA050595_M", "TA050631_M", "TA050127_M", "TA050769_M", "TA050712_M", "TA050715_M", "TA050716_M",
                                                                       "TA050678_M", "TA050785_M", "TA050809_M", "TA050793_M", "TA050777_M", "TA050825_M", "TA050817_M", "TA050798_M", "TA050394_M"))
S12_ID_levels <- dplyr::pull(T2005M_S12, ID)
T2005M_S12_tidy$ID <- factor(T2005M_S12_tidy$ID, levels = S12_ID_levels)

T2005M_S12_tidy$met_FTL_crt<- factor(T2005M_S12_tidy$met_FTL_crt)

ftl_plot12 <- ggplot(T2005M_S12_tidy, aes(x=ID, y=variable, fill=met_FTL_crt)) + geom_tile(color="white", size=0.5) +
  coord_equal() +
  labs(x="ID", y="Criteria") +
  theme_tufte(base_family="Helvetica") +
  theme(axis.ticks=element_blank()) + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
ftl_plot12 + scale_fill_discrete(name = "Met FTL Criteria", labels = c("No", "Yes"))
ggsave("TIAS2005_CRT_PLOT12.png")
    
# lines 601-650 of logical match data for TIAS 2005 FTL criteria

T2005M_S13 <- T2005M[601:650,]

T2005M_S13_tidy <- T2005M_S13 %>% tidyr::gather(variable, met_FTL_crt, 1:18)

T2005M_S13_tidy$variable <- factor(T2005M_S13_tidy$variable, levels =c("TA050042_M", "TA050043_M", "TA050595_M", "TA050631_M", "TA050127_M", "TA050769_M", "TA050712_M", "TA050715_M", "TA050716_M",
                                                                       "TA050678_M", "TA050785_M", "TA050809_M", "TA050793_M", "TA050777_M", "TA050825_M", "TA050817_M", "TA050798_M", "TA050394_M"))
S13_ID_levels <- dplyr::pull(T2005M_S13, ID)
T2005M_S13_tidy$ID <- factor(T2005M_S13_tidy$ID, levels = S13_ID_levels)

T2005M_S13_tidy$met_FTL_crt<- factor(T2005M_S13_tidy$met_FTL_crt)

ftl_plot13 <- ggplot(T2005M_S13_tidy, aes(x=ID, y=variable, fill=met_FTL_crt)) + geom_tile(color="white", size=0.5) +
  coord_equal() +
  labs(x="ID", y="Criteria") +
  theme_tufte(base_family="Helvetica") +
  theme(axis.ticks=element_blank()) + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
ftl_plot13 + scale_fill_discrete(name = "Met FTL Criteria", labels = c("No", "Yes"))
ggsave("TIAS2005_CRT_PLOT13.png")

# lines 651-700 of logical match data for TIAS 2005 FTL criteria

T2005M_S14 <- T2005M[651:700,]

T2005M_S14_tidy <- T2005M_S14 %>% tidyr::gather(variable, met_FTL_crt, 1:18)

T2005M_S14_tidy$variable <- factor(T2005M_S14_tidy$variable, levels =c("TA050042_M", "TA050043_M", "TA050595_M", "TA050631_M", "TA050127_M", "TA050769_M", "TA050712_M", "TA050715_M", "TA050716_M",
                                                                       "TA050678_M", "TA050785_M", "TA050809_M", "TA050793_M", "TA050777_M", "TA050825_M", "TA050817_M", "TA050798_M", "TA050394_M"))
S14_ID_levels <- dplyr::pull(T2005M_S14, ID)
T2005M_S14_tidy$ID <- factor(T2005M_S14_tidy$ID, levels = S14_ID_levels)

T2005M_S14_tidy$met_FTL_crt<- factor(T2005M_S14_tidy$met_FTL_crt)

ftl_plot14 <- ggplot(T2005M_S14_tidy, aes(x=ID, y=variable, fill=met_FTL_crt)) + geom_tile(color="white", size=0.5) +
  coord_equal() +
  labs(x="ID", y="Criteria") +
  theme_tufte(base_family="Helvetica") +
  theme(axis.ticks=element_blank()) + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
ftl_plot14 + scale_fill_discrete(name = "Met FTL Criteria", labels = c("No", "Yes"))
ggsave("TIAS2005_CRT_PLOT14.png")

# lines 701-745 of logical match data for TIAS 2005 FTL criteria

T2005M_S15 <- T2005M[701:745,]

T2005M_S15_tidy <- T2005M_S15 %>% tidyr::gather(variable, met_FTL_crt, 1:18)

T2005M_S15_tidy$variable <- factor(T2005M_S15_tidy$variable, levels =c("TA050042_M", "TA050043_M", "TA050595_M", "TA050631_M", "TA050127_M", "TA050769_M", "TA050712_M", "TA050715_M", "TA050716_M",
                                                                       "TA050678_M", "TA050785_M", "TA050809_M", "TA050793_M", "TA050777_M", "TA050825_M", "TA050817_M", "TA050798_M", "TA050394_M"))
S15_ID_levels <- dplyr::pull(T2005M_S15, ID)
T2005M_S15_tidy$ID <- factor(T2005M_S15_tidy$ID, levels = S15_ID_levels)

T2005M_S15_tidy$met_FTL_crt<- factor(T2005M_S15_tidy$met_FTL_crt)

ftl_plot15 <- ggplot(T2005M_S15_tidy, aes(x=ID, y=variable, fill=met_FTL_crt)) + geom_tile(color="white", size=0.5) +
  coord_equal() +
  labs(x="ID", y="Criteria") +
  theme_tufte(base_family="Helvetica") +
  theme(axis.ticks=element_blank()) + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1)) + 
  scale_fill_discrete(name = "Met FTL Criteria", labels = c("No", "Yes"))
ggsave("TIAS2005_CRT_PLOT15.png")

# viewing all heatmap plots for TIAS 2005 FTL criteria match, manually export as image with width 3000 height 2800 

ggarrange(ftl_plot1 + rremove("legend"), ftl_plot2 + rremove("legend") + rremove("y.title"), ftl_plot3 + rremove("legend") + rremove("y.title"), ftl_plot4 + rremove("legend"), 
          ftl_plot5 + rremove("legend") + rremove("y.title"), ftl_plot6 + rremove("legend") + rremove("y.title"), ftl_plot7 + rremove("legend"), ftl_plot8 + rremove("legend") + rremove("y.title"), 
          ftl_plot9 + rremove("legend") + rremove("y.title"), ftl_plot10 + rremove("legend"), ftl_plot11 + rremove("legend") + rremove("y.title"), ftl_plot12 + rremove("legend") + rremove("y.title"),
          ftl_plot13 + rremove("legend"), ftl_plot14 + rremove("legend") + rremove("y.title"), ftl_plot15 + rremove("y.title"), ncol = 3, nrow = 5) 

#####################
# TIAS 2007 
#####################

# remove objects
rm(list=ls())

TIAS2007 <- TIAS[!is.na(TIAS$TAS07),]

TIAS2005$CAT <- with(TIAS2005, ifelse(
  TA050042 == 1 & TA050043 == 1 & TA050595 %in% c("5", "0") & TA050631 %in% c("5", "0") & TA050127 == 3 & TA050769 < 60 & 
    TA050712 == 0 & TA050715 == 0 & TA050716 == 0 & TA050678 == 1 & TA050785 == 0 & TA050809 == 0 & TA050793 == 0 & TA050777 == 0 & TA050825 == 0 & 
    TA050817 == 0 & TA050798 == 0 & TA050394 %in% c("9", "10"), "FTL_05", "IAC_05"
  ))

table(TIAS2005$CAT)

TIAS2005 <- TIAS2005 %>% select(ID, CAT, TA050042, TA050043, TA050595, TA050631, TA050127, TA050769, TA050712, TA050715, TA050716, TA050678, 
                                TA050785, TA050809, TA050793, TA050777, TA050825, TA050817, TA050798, TA050394)

TIAS2005$TA050042_M <- TIAS2005$TA050042 == 1 
TIAS2005$TA050043_M <- TIAS2005$TA050043 == 1
TIAS2005$TA050595_M <- TIAS2005$TA050595 %in% c("5", "0")
TIAS2005$TA050631_M <- TIAS2005$TA050631 %in% c("5", "0")
TIAS2005$TA050127_M <- TIAS2005$TA050127 == 3
TIAS2005$TA050769_M <- TIAS2005$TA050769 < 60
TIAS2005$TA050712_M <- TIAS2005$TA050712 == 0 
TIAS2005$TA050715_M <- TIAS2005$TA050715 == 0 
TIAS2005$TA050716_M <- TIAS2005$TA050716 == 0 
TIAS2005$TA050678_M <- TIAS2005$TA050678 == 1
TIAS2005$TA050785_M <- TIAS2005$TA050785 == 0
TIAS2005$TA050809_M <- TIAS2005$TA050809 == 0
TIAS2005$TA050793_M <- TIAS2005$TA050793 == 0
TIAS2005$TA050777_M <- TIAS2005$TA050777 == 0
TIAS2005$TA050825_M <- TIAS2005$TA050825 == 0
TIAS2005$TA050817_M <- TIAS2005$TA050817 == 0
TIAS2005$TA050798_M <- TIAS2005$TA050798 == 0
TIAS2005$TA050394_M <- TIAS2005$TA050394 %in% c("9", "10")









      
