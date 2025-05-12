# Script #3: Make presence-absence dataset

# Script to make presence-absence dataframe that will be used in Species
# Distribution Model demo during ARSET training: Introduction to the Integration
# of Animal Tracking and Remote Sensing (Part 2).

# Presences come from the frigatebird tracking dataset. Absences come from the
# background pseudo-absence points.

# Contact: Morgan Gilmour, morgan.e.gilmour@nasa.gov

# May 2025

# Load packages ----

library(purrr)
library(readr)
library(sf)
library(tidyr)
library(dplyr)

# See additional functions in code file that were adapted from Hazen et al. 2021
# paper. This script uses the function "makedataset()".

# Source functions
source("C:/Users/Desktop/SDM_functions_code.R")

# If desired, you can download all functions from that paper from GitHub
# Navigate to github page: https://github.com/elhazen/PA-paper
# Then click Code --> Download ZIP

#
# Load "presence" dataset ----

# These are the frigatebird tracks with the environmental covariates
load(file = "C:/Users/Desktop/Frigatebird_data_for_ARSET/frigatebird_tracks_evars_30April2025.rds")
# called "frigatebird.tracks"

#
## Prepare presence dataframe ----

# Rename columns to match what's required in the "makedatset()" function and
# subset to 3 birds.
frigatebird_tracks_3<-frigatebird.tracks %>% 
  # To save time for the demo, subset to three birds
  filter(TagLocalIdentifier %in% c("eobs_9829","eobs_9830","eobs_9835")) %>% 
  # Rename & select columns to match simulated background points dataframe
  dplyr::select(tag=TagLocalIdentifier,
                dTime=Timestamp,
                Date,
                long=LocationLong,
                lat=LocationLat,
                chla,
                sst,
                BathymetryDepth) %>% 
  # Add "iteration" column to match simulated background points dataframe
  mutate(iteration=0,
         # Calculate the log of chlorophyll
         ln_chla=log(chla))
#
# Load "absence" dataset ----

# These are the points you simulated with the function
# createbackgroundabsence(), which are stored as .csv files.

# Make a list of the .csv files to read
paths_bkgrd<-fs::dir_ls("C:/Users/Desktop/Frigatebird_data_for_ARSET/pseudoabsences/",
                        glob = "*csv",recurse = TRUE)

# Read in the .csv files and subset to the three birds listed above
frigatebird_bkgrd<-purrr::map_dfr(paths_bkgrd,data.table::fread,
                           .id = "csv_path",
                           colClasses ="character") %>% 
  # Re-convert columns to numeric, date, etc from characters
  readr::type_convert() %>% 
  dplyr::select(-csv_path) %>% 
  filter(ID %in% c("eobs_9829","eobs_9830","eobs_9835"))
#
## Append environmental covariates to background (absence) points ----

# Done offscreen; Follow steps in "1-append-evars_ARSET.R" script.
#
## Prepare absence dataframe ----

# Select columns so that absence dataframe matches the presence dataframe
frigatebird_bkgrd_3<-frigatebird_bkgrd %>% 
  dplyr::select(tag,
                dTime,
                MonthDate=Date,
                long,
                lat,
                chla,
                sst,
                BathymetryDepth,
                iteration) %>% 
  mutate(ln_chla=log(chla))

glimpse(frigatebird_bkgrd_3)
#
# Make the presence-absence dataset via makedataset() ----

# Uses presence & absence dataframes to create binary response variable
presabs.bkgrd<-makedataset(presence = frigatebird_tracks_3,
                           absencedata = frigatebird_bkgrd_3)

glimpse(presabs.bkgrd)

# Verify absence vs. presence points 
ggplot(data = presabs.bkgrd %>%  
         filter(tag %in% c("eobs_9829","eobs_9830","eobs_9835"),
                lat < 7))+
  geom_point(aes(long,lat,fill=factor(presabs)),
             pch=21,alpha=0.4)+
  scale_fill_viridis_d(c("absence","presence"))

# Need presabs.bkgrd to be an sf-dataframe
presabs.bkgrd.sf<-presabs.bkgrd %>% 
  # Keep Long/Lat columns before cbinding them in coords argument (helpful for
  # later plotting)
  mutate(coordsX1=long,coordsX2=lat) %>% 
  sf::st_as_sf(x = .,
               coords=c("coordsX1","coordsX2"),
               crs="+proj=longlat +ellps=WGS84 +datum=WGS84")

glimpse(presabs.bkgrd.sf)
# 
# Examine differences in environmental covariates between presence/absence points ----

# via Bhattacharyya's Coefficient

# Quantify the statistical independence of environmental niches of presences &
# pseudo-absences per variable.
bhattacharyya.stat(data=presabs.bkgrd)

# Context: 0 = Complete overlap; 1 = Very different
#
## Clean-up workspace ----

rm(paths_bkgrd); rm(frigatebird_bkgrd)
#