# Prep Script: Prep frigatebird tracks for ARSET exercise

# Script to prepare frigatebird tracking dataset for demo during ARSET training:
# Introduction to the Integration of Animal Tracking and Remote Sensing (Part
# 2).

# Contact: Morgan Gilmour, morgan.e.gilmour@nasa.gov

# May 2025

# Load packages ----

library(trakR) # Need to download from github, not CRAN: www.github.com/abfleishman/trakr

library(purrr)
library(tidyr)
library(dplyr)
#
# Download frigatebird data ----

# Frigatebird tracking data are from Gilmour et al 2024, https://www.doi.org/10.24431/rw1k8ez

# Navigate to https://www.doi.org/10.24431/rw1k8ez and download all files that
# begin with "GRFR" - there are seven GRFR files. Save the files as csv's.
#
# Load & prep frigatebird data ----

# Create a list of frigatebird csv files
paths_grfr_eobs<-fs::dir_ls("C:/Users/Desktop/Frigatebird_data_for_ARSET/",
                            glob = "*csv",recurse = TRUE)
# 
# Read files
# Create a column called csv_path that has the names assigned to it from the
# named attribute from the fs path object (the class of paths)
frigatebird.tracks<-purrr::map_dfr(paths_grfr_eobs,data.table::fread,
                                   .id = "csv_path",
                                   colClasses ="character") %>%
  # Re-convert columns to numeric, date, etc from characters
  readr::type_convert() %>%
  # Clean-up column names
  janitor::clean_names("upper_camel",abbreviations=c("ID")) %>%
  # Filter out GPS points from acceleration points
  filter(SensorType=="gps") %>%
  # Only keep Lat/Long/Timestamp/ID columns
  dplyr::select(Timestamp,LocationLong,LocationLat,TagLocalIdentifier) %>%
  # Remove NA's from Lat/Long
  filter(!is.na(LocationLat)) %>%
  # Add "Island" column for later use
  mutate(Island="Palmyra")

# Apply speed filter ----

# First, calculate Inter-point distances (unit=seconds)
frigatebird.tracks$InterPtDist<-trakR::InterpointDist(tracks = frigatebird.tracks,
                                                      ID = "TagLocalIdentifier",
                                                      lat = "LocationLat",
                                                      lon = "LocationLong")

# Second, calculate Inter-point time (unit=meters)
frigatebird.tracks$InterPtTime<-trakR::InterpointTime(tracks = frigatebird.tracks,
                                                      ID = "TagLocalIdentifier",
                                                      DateTime = "Timestamp")

# Third, calculate Inter-point speed (unit=km/hr)
frigatebird.tracks$Speed<-trakR::Speed(Dist = frigatebird.tracks$InterPtDist,
                                       Time = frigatebird.tracks$InterPtTime)

# Fourth, filter out points based on speed threshold
# 25 m/s (90 kmphr; speed filter from: Weimerskirch & Prudor 2019
# https://doi.org/10.1038/s41598-019-41481-x)
frigatebird.tracks<-frigatebird.tracks %>%
  filter(Speed<90)
#
# Calculate distance to nest island ----

# Define nest island location
palmyra<-data.frame(LocationLong=-162.072567,
                    LocationLat=5.879722,
                    Island="Palmyra")

frigatebird.tracks$Dist2Island<-trakR::AddDist2Colony(tracks = frigatebird.tracks,
                                                      dataLat = "LocationLat",
                                                      dataLon = "LocationLong",
                                                      CaptureSitesData = palmyra,
                                                      SiteName = "Island",
                                                      capLat = "LocationLat",
                                                      capLon = "LocationLong")

# Filter locations >5 km from Palmyra for analyses
frigatebird.tracks<-frigatebird.tracks %>%
  filter(Dist2Island > 5)

# Subset frigatebird_tracks to just the three to save time for demo
frigatebird.tracks<-frigatebird.tracks %>%
  filter(TagLocalIdentifier %in% c("eobs_9829","eobs_9830","eobs_9835"))
#
# Clean-up workspace ----

rm(paths_grfr_eobs); rm(palmyra)
#