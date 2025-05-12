# Script #6: Evaluate Species Distribution Model results

# Script to evaluate Species Distribution Model demo during ARSET training:
# Introduction to the Integration of Animal Tracking and Remote Sensing (Part
# 2).

# Contact: Morgan Gilmour, morgan.e.gilmour@nasa.gov

# May 2025

# Load packages ----

library(sf)
library(tidyr)
library(dplyr)

# See additional functions in code file that were adapted from Hazen et al. 2021
# paper. This script uses the function "pseudoR2.BRT()", "kfolds_eval_brt()",
# and "bhattacharyya.stat()".

# Source functions
source("C:/Users/Desktop/SDM_functions_code.R")

# If desired, you can download all functions from that paper from GitHub
# Navigate to github page: https://github.com/elhazen/PA-paper
# Then click Code --> Download ZIP

#
# Evaluate SDM ----
## a) R-squared (Explanatory power) ----

pseudoR2.BRT(BRT.bkgrd) #0.40
#
## b) AUC & TSS (Predictive skill) ----

# Area Under Receiver Operating Characteristic Curve (AUC) & True Test Statistic (TSS)

# Need presabs.bkgrd to be an sf-dataframe
presabs.bkgrd.sf<-presabs.bkgrd %>% 
  # Keep Long/Lat columns before cbinding them in coords argument (helpful for
  # later plotting)
  mutate(coordsX1=long,coordsX2=lat) %>% 
  sf::st_as_sf(x = .,
               coords=c("coordsX1","coordsX2"),
               crs="+proj=longlat +ellps=WGS84 +datum=WGS84")


# Calculated for BRT using 10-fold cross-validation
brt.kfold.bkgrd<-kfolds_eval_brt(dataInput = sf::st_set_geometry(presabs.bkgrd.sf,NULL),
                                 gbm.x = c(8, # BathymetryDepth
                                           10, # ln_chla
                                           7  # sst
                                 ),
                                 gbm.y = 11, # presabs
                                 lr = 0.005,
                                 tc = 5,
                                 bf = 0.75,
                                 nt = 2000)

# View results
brt.kfold.bkgrd

# Calculate mean AUC and TSS
mean(brt.kfold.bkgrd$AUC)
mean(brt.kfold.bkgrd$TSS)

#
## Save k-fold object ----

# BRT, k-fold
saveRDS(brt.kfold.bkgrd, file = "C:/Users/Desktop/Frigatebird_data_for_ARSET/Model_results/kfold.BRT.bkgrd.02May2025.rds")
# Saved most recently 18Aug2023
#