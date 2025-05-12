# Script #4: Run Boosted Regression Tree

# Script to run Boosted Regression Tree that will be used in Species
# Distribution Model demo during ARSET training: Introduction to the Integration
# of Animal Tracking and Remote Sensing (Part 2).

# Contact: Morgan Gilmour, morgan.e.gilmour@nasa.gov

# May 2025

# Load packages ----

library(sf)
library(dismo)
library(gbm)
library(tidyr)
library(dplyr)

# See additional functions in code file that were adapted from Hazen et al. 2021
# paper. This script uses the function "BRTtransformDataFrame()".

# Source functions
source("C:/Users/Desktop/SDM_functions_code.R")

# If desired, you can download all functions from that paper from GitHub
# Navigate to github page: https://github.com/elhazen/PA-paper
# Then click Code --> Download ZIP

#
# Run model ----

# The model will estimate how environmental conditions (covariates) predict
# frigatebird presence/absence via gbm.fixed()

# NOTE: Use "try()" so that error messages are retained
BRT.bkgrd<-try(BRT.lr005.bkgrd<-dismo::gbm.fixed(data=BRTtransformDataFrame(datafr=sf::st_set_geometry(presabs.bkgrd.sf,NULL)),
                                                 gbm.x=c(6, # BathymetryDepth
                                                         7, # ln_chla
                                                         8  # sst
                                                 ), 
                                                 gbm.y=5, # presabs
                                                 family="bernoulli",
                                                 tree.complexity=5, 
                                                 learning.rate = 0.005, 
                                                 n.trees = 2000, 
                                                 bag.fraction=0.75))
#
# Save BRT model ----

saveRDS(BRT.bkgrd, file = "C:/Users/Desktop/Frigatebird_data_for_ARSET/Model_results/BRT_bkgrd_02May2025.rds")
# Saved most recently 02May2025
#
# Plot BRT model ----

plot(BRT.bkgrd, i.var=1) # BathymetryDepth
plot(BRT.bkgrd, i.var=2) # ln_chla
plot(BRT.bkgrd, i.var=3) # sst
#
# Relative influence of variables ----

gbm::summary.gbm(object = BRT.bkgrd,
                 n.trees = 2000,
                 plotit = TRUE)
#