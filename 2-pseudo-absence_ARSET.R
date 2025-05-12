# Script #2: Simulate pseudo-absences

# Script to simulate background pseudo-absences for Species Distribution Model
# demo during ARSET training: Introduction to the Integration of Animal Tracking
# and Remote Sensing (Part 2).

# Contact: Morgan Gilmour, morgan.e.gilmour@nasa.gov

# May 2025

# Load packages ----

library(adehabitatLT)
library(ggplot2)
library(tidyr)
library(dplyr)

# See additional functions in code file that were adapted from Hazen et al. 2021
# paper. This script uses the function "createbackgroundabsence()".

# Source functions
source("C:/Users/Desktop/SDM_functions_code.R")

# If desired, you can download all functions from that paper from GitHub
# Navigate to github page: https://github.com/elhazen/PA-paper
# Then click Code --> Download ZIP

#
# Simulate pseudo-absences ----

# Function: createbackgroundabsence()

# Simulates background pts 10x per bird & randomly samples from these 10
# simulations to create background points per bird

## Prepare tracking dataframe ----

# Create list of tags to loop through
tagid<-unique(frigatebird.tracks$TagLocalIdentifier)

# Identify where the pseudo absences will go
out.dir<-"C:/Users/Desktop/Frigatebird_data_for_ARSET/pseudoabsences/"

# Need to change column names to match column names in function
tags<-frigatebird.tracks %>% 
  dplyr::select(id=TagLocalIdentifier,
                long=LocationLong,
                lat=LocationLat,
                dTime=Timestamp) %>% 
  # Add location class if you have Argos data; in our case, it's GPS
  mutate(lc="G")

glimpse(tags)
#
## Run createbackgroundabsence() in for-loop ----

# Run the for-loop to simulate the pseudoabsences

Sys.time() # Assess how long this step takes

for (tagid in unique(tags$id)){    
  
  graphics.off()
  
  #Simulate background
  sim.background.alldata <- createbackgroundabsence(tags, tagid, n.sim=10)  
}

Sys.time() # This took 1 hr 35 minutes

# This step will create a .csv file in your "out_dir" for each bird. The .csv
# contains lat/longs that were simulated for each point in your dataset, 10
# times (indicated by the "iteration" column).
#
## View presences and pseudo-absences ----

# Read in one pseudo-absence file to plot. e.g.
sim.background.alldata<-read.csv(file = "C:/Users/Desktop/Frigatebird_data_for_ARSET/pseudoabsences/backgroundpts_sim_eobs_9835.csv")

glimpse(sim.background.alldata)

ggplot()+
  geom_point(data = sim.background.alldata,
             aes(x=long,y=lat))+
  geom_point(data = frigatebird.tracks %>% 
               filter(TagLocalIdentifier=="eobs_9835"),
             aes(x=LocationLong,y=LocationLat),
             color="red")+
  facet_wrap(~iteration)
#
## Clean-up workspace ----
rm(tags); rm(out.dir)
#