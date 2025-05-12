# Script #1: Append environmental variables to animal tracks

# Script to demonstrate downloading and extracting remotely sensed environmental
# covariates for demo during ARSET training: Introduction to the Integration of
# Animal Tracking and Remote Sensing (Part 2).

# Contact: Morgan Gilmour, morgan.e.gilmour@nasa.gov

# May 2025

# Load packages ----
library(rerddap)
library(rerddapXtracto)
library(marmap)
library(ncdf4)
library(ggplot2)
library(tidyr)
library(dplyr)
#
# Chlorophyll-a example ----
## Download chlorophyll-a data (via rerddap R-pkg) ----

# Step 1: Determine spatial extent of frigatebird tracks
extent_to_download<-data.frame(Longitude=c(min(frigatebird.tracks$LocationLong),
                                           max(frigatebird.tracks$LocationLong)),
                               Latitude=c(min(frigatebird.tracks$LocationLat),
                                          max(frigatebird.tracks$LocationLat)))

# Step 2: Determine temporal extent of frigatebird tracks
dates_to_download<-data.frame(min_date=min(frigatebird.tracks$Timestamp),
                              max_date=max(frigatebird.tracks$Timestamp))

# Step 3: Set location to hold downloaded data
out_folder<-"C:/Users/Desktop/Frigatebird_evars/" 

# Set cache to out_folder
rerddap::cache_setup(full_path = out_folder,temp_dir = FALSE)

# Step 4: Examine metadata for chlorophyll-a dataset called "erdMBchlamday_LonPM180"
CHL<-rerddap::info("erdMBchlamday_LonPM180")
CHL

# Set dataset name
dataset_name<-"erdMBchlamday_LonPM180"

# Step 5: Download data
chl<-rerddapXtracto::rxtracto_3D(dataInfo = CHL,
                                 parameter = "chlorophyll",
                                 xcoord = c(min(extent_to_download$Longitude),
                                            max(extent_to_download$Longitude)),
                                 ycoord = c(min(extent_to_download$Latitude),
                                            max(extent_to_download$Latitude)),
                                 tcoord = c(dates_to_download$min_date,
                                            dates_to_download$max_date),
                                 zcoord = c(0,0),
                                 verbose = TRUE,
                                 cache_remove = FALSE)

# Step 6: Plot to verify download was successful
myFunc<- function(x) log(x) 
rerddapXtracto::plotBBox(chl,myFunc = myFunc)

# Convert to tidy format (dataframe)
chl_tidy<-rerddapXtracto::tidy_grid(chl)
chl_tidy

#
## Read in .nc files ----

# Set location of chlorophyll files
out_folder<-"C:/Users/Desktop/Frigatebird_evars/" 

# List all the chlorophyll files in your out_folder, e.g.
files<-list.files(path=out_folder,
                  pattern="chla",
                  full.names=TRUE)

# Open the chlorophyll files
chla.nc<-nc_open(files)
# Confirm that there's data
ncdf4::ncvar_get(chla.nc,varid = "chlorophyll")

# Create a raster "stack" of the chlorophyll data. Each layer is a date.
dat_chla<-lapply(files,function(x)raster::stack(x,varname="chlorophyll"))
dat_chla<-raster::stack(dat_chla)
#
## Make a table of dates to loop through ----

# List the available dates from the chlorophyll raster and make a "look-up
# table." In the for-loop below, you will match dates in the the tracking data
# to dates in the look-up table.

# Raster is indexed by names (which is the X-date; these are the names of the
# layers in the raster, where each layer is a date).

DateTable<-data.frame(xdate=names(dat_chla),
                      DateTime=lubridate::ymd_hms(gsub("X","",names(dat_chla)),
                                                  tz="UTC") %>%
                        lubridate::with_tz("UTC")) %>%
  # Because we're working with monthly chlorophyll data, round each date to a month
  mutate(MonthDate=lubridate::floor_date(DateTime,unit="1 month"))

# View the table we just created & verify xdate was rounded to month
glimpse(DateTable)

# Round the tracking data dates to a month to match the resolution of the
# look-up table.
frigatebird.tracks<-frigatebird.tracks %>% 
  mutate(MonthDate=as.Date(lubridate::round_date(Timestamp,unit="1 month"))) 

# Verify that MonthDate is the correctly-rounded Timestamp
head(frigatebird.tracks$Timestamp)
head(frigatebird.tracks$MonthDate)
#
## Append chlorophyll-a to each location along frigatebird tracks ----

# Get unique Dates from tracking data; you will loop through this vector
Dates<-frigatebird.tracks %>% 
  pull(MonthDate) # Returns vector
# Look at Dates vector, through which you will loop
head(Dates)

# Make new column where the chlorophyll data will go
frigatebird.tracks$chla<-NA

for(i in 1:length(Dates)){
  
  print(paste("i =",i,"; Date/Time =",Dates[i]))
  
  # This is the index for each unique "xdate" (aka each chla raster layer)      
  datex<-as.character(DateTable$xdate[DateTable$MonthDate==Dates[i]])
  
  # If the xdate is NA, skip it
  if(length(datex)<1|is.na(datex)) next
  
  # Subset the rows in frigatebird.tracks that match datex
  dat_subsetter<-which(frigatebird.tracks$MonthDate==Dates[i])
  
  # Create a SpatialPointsDataFrame out of the frigatebird.tracks. 
  # This enables us to extract the chlorophyll data from the raster that match
  # the specific dates in the frigatebird.tracks.
  pt<-data.frame(Longitude=frigatebird.tracks$LocationLong[dat_subsetter],
                 Latitude=frigatebird.tracks$LocationLat[dat_subsetter])
  # Assign a Coordinate Reference System to the frigatebird SpatialPointsDataFrame
  sp::coordinates(pt)<-cbind(frigatebird.tracks$LocationLong[dat_subsetter],
                         frigatebird.tracks$LocationLat[dat_subsetter])
  raster::crs(pt)<-sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  
  # Extract the chlorophyll data from the raster and append it to the
  # frigatebird.tracks dataframe.
  frigatebird.tracks$chla[dat_subsetter]<-raster::extract(dat_chla[[datex]],pt)
  
}

# Are there any NA's in the data?
table(is.na(frigatebird.tracks$chla)) #1242/72947 = 1.7% are NA's
#
## Plot frigatebird.tracks, colored by chlorophyll ----

ggplot(data = frigatebird.tracks %>% 
         filter(TagLocalIdentifier=="eobs_9830"))+
  geom_path(aes(x=LocationLong,y=LocationLat),
            linewidth=0.25)+
  geom_point(aes(x=LocationLong,y=LocationLat,fill=log(chla)),
             pch=21,size=5,alpha=0.6)+
  scale_fill_viridis_c()
#
## Clean up workspace ----

rm(files)
rm(DateTable); rm(Dates)
rm(chla.nc); rm(dat_chla)
#
# Bathymetric depth example ----
## Download bathymetric depth (via marmap R-pkg) ----

bathymetry<-marmap::getNOAA.bathy(lon1 = max(extent_to_download$Longitude),
                                  lon2 = min(extent_to_download$Longitude),
                                  lat1 = min(extent_to_download$Latitude),
                                  lat2 = max(extent_to_download$Latitude),
                                  antimeridian = FALSE, #Use TRUE if crossing International Dateline
                                  resolution = 4, #units=arc-minutes
                                  keep = TRUE,
                                  path = out_folder)

#
## Read in Bathymetry data ----

depth<-read.csv(file="C:/Users/Desktop/Frigatebird_data_for_ARSET/evars/marmap_coord_-164.6823675;3.1664833;-159.2554599;7.7768925_res_4.csv",
                header = TRUE)
#
## Prepare Bathymetry data ----

# Make it a raster
# First, make the .csv an object of class "bathy" (from R-pkg marmap)
depth<-marmap::as.bathy(depth)

# Second, verify it's the data that you want
plot(depth)

# Third, make it a raster to append it to tracks
depth.raster<-marmap::as.raster(depth) 

# Fourth, set all points >0 to NA (these are points on land, and therefore
# useless for our ocean study)
depth.raster[depth.raster >= 0]<-NA
#
## Append Bathymetric depth to each location along frigatebird tracks ----

# Initiate empty columns to populate
frigatebird.tracks$BathymetryDepth<-NA

# For-loop to attach bathymetry to frigatebird_tracks dataframe
Birds<-unique(frigatebird.tracks$TagLocalIdentifier)

# Need to use longitude range 0-360.
for(i in 1:length(Birds)){
  message(i)
  pt<-data.frame(longitude=frigatebird.tracks$LocationLong[frigatebird.tracks$TagLocalIdentifier==Birds[i]],
                 latitude=frigatebird.tracks$LocationLat[frigatebird.tracks$TagLocalIdentifier==Birds[i]])
  
  sp::coordinates(pt)<-cbind(longitude=frigatebird.tracks$LocationLong[frigatebird.tracks$TagLocalIdentifier==Birds[i]],
                             latitude=frigatebird.tracks$LocationLat[frigatebird.tracks$TagLocalIdentifier==Birds[i]])
  
  raster::crs(pt)<-paste("+proj=longlat +ellps=WGS84 +datum=WGS84")
  
  # Attach bathymetry metrics
  frigatebird.tracks$BathymetryDepth[frigatebird.tracks$TagLocalIdentifier==Birds[i]]<-raster::extract(x=depth.raster,y=pt)
}

# Are there any NA's in the data?
table(is.na(frigatebird.tracks$BathymetryDepth)) #239/72947 points are NA (0.03%)
#
## Plot frigatebird.tracks, colored by BathymetryDepth ----

ggplot(data = frigatebird.tracks %>% 
         filter(TagLocalIdentifier=="eobs_9830"))+
  geom_path(aes(x=LocationLong,y=LocationLat),
            linewidth=0.25)+
  geom_point(aes(x=LocationLong,y=LocationLat,fill=BathymetryDepth),
             pch=21,size=5,alpha=0.6)+
  scale_fill_viridis_c()
#
## Clean up workspace ----

rm(depth); rm(depth.raster); rm(Birds); rm(i)
#
# Save tracking data file because this just took a long time! ----
save(frigatebird.tracks,
     file = "C:/Users/Desktop/Frigatebird_data_for_ARSET/frigatebird_tracks_evars_30April2025.rds")
#