
# packages ----------------------------------------------------------------

library(plyr)
library(dplyr)
library(ggplot2)
library(lme4)
library(car)
library(lmerTest)
library(vegan)
library(tidyr)
library(lsmeans)
library(visreg)
library(MuMIn)



# load data ---------------------------------------------------------------

graminoid_data <- read.csv("Data/FACE_P0029_RA_UCOMGM_R_201209-201602.csv")       # data frame for graminoids
forb_data      <- read.csv("Data/FACE_P0029_RA_UCOMFB_R_201209-201602.csv")       # data frame for forbs
site_var       <- c("year", "ring", "co2", "plot")                                # vector for site variables
site_data      <- graminoid_data[ ,site_var]                                      # data frame for site
SppName_gram   <- setdiff(names(graminoid_data), site_var)                        # graminoid species names
SppName_forb   <- setdiff(names(forb_data), site_var)                             # forb species names
env_data       <- read.csv("Data/FACE_P0029_RA_ENVVARS_L1_201301-201512.csv")     # environmental variables (moisture, temperature and PAR)
soil_data      <- read.csv("Data/FACE_P0029_RA_SOILVARS_L1_201311-201602.csv")    # soil nutrient data (nitrate, ammonium and phosphoate)
sp_pfg         <- read.csv("Data/graminoid_pfg.csv")                              # graminoid species and and their corresponding plant functional groups




# analysis ----------------------------------------------------------------

options(na.action = "na.fail")      # change na.action setting for MuMIN::dredge
source("R/analysis_diversity.R")    # analysis on diversity indices
source("R/analysis_cover.R")        # analysis on cover of C3 and C4 graminoids for dominant and subordinate spp
source("R/analysis_C43r_soilNP.R")  # multiple regression analysis on C4:C3 ratios and soil nutrients
