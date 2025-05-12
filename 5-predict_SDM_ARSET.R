# Script #5: Predict species habitat suitability

# Script to apply Boosted Regression Tree model to grid of unsampled region for
# demo during ARSET training: Introduction to the Integration of Animal Tracking
# and Remote Sensing (Part 2).

# Contact: Morgan Gilmour, morgan.e.gilmour@nasa.gov

# May 2025

# Load packages ----

library(sf)
library(gbm)
library(ggplot2)
library(tidyr)
library(dplyr)

#
# Apply BRT model across grid ----

# This will predict where frigatebirds may go or not go, based on environmental
# covariates via predict.gbm() 

# Uses BRT model & pres/abs grid to predict SDM across grid
#
## Make a sample grid ----

# Make a grid of the extent of sample area
presabs.grid<-expand.grid(long=seq(floor(min(presabs.bkgrd$long)),
                                   ceiling(max(presabs.bkgrd$long)),
                                   by=0.25),
                          lat=seq(floor(min(presabs.bkgrd$lat)),
                                  ceiling(max(presabs.bkgrd$lat)),
                                  by=0.25)) %>% 
  # Add columns so that dataframe matches the presence-absence dataframe
  mutate(MonthDate=as.Date("2022-06-01"),
         tag="MODEL_2022_01")

# Plot the grid to verify it covers the dimensions that you want
ggplot(data = presabs.grid)+
  geom_point(aes(x=long,y=lat))

## Add environmental variables to this grid ----

# Done offscreen; Follow steps in "1-append-evars_ARSET.R" script.
#
## Prepare grid with environmental variables for SDM ----

# Take log(chla)
presabs.grid<-presabs.grid %>% 
  mutate(ln_chla=log(chla))

# NOTE: May need to Scale and Center the environmental variables in the grid
presabs.grid<-presabs.grid %>% 
  mutate(BathymetryDepth.sc=scale(BathymetryDepth,scale=TRUE,center=TRUE),
         ln.Chl_a.sc=scale(log(Chl_a),scale=TRUE,center=TRUE),
         SST.sc=scale(SST,scale=TRUE,center=TRUE))
#
# Predict SDM across grid ----

BRT.bkgrd.preds<-gbm::predict.gbm(object = BRT.bkgrd, 
                                  newdata = presabs.grid,
                                  n.trees = BRT.bkgrd$gbm.call$best.trees, 
                                  type="response")
glimpse(BRT.bkgrd.preds)

# Add model predictions back into presabs.grid.bkgrd
presabs.grid<-presabs.grid %>% 
  bind_cols(as.data.frame(BRT.bkgrd.preds) %>% 
              rename(BRT.fit=BRT.bkgrd.preds))
#
## Save SDM results in presabs.grid ----

saveRDS("C:/Users/Desktop/Frigatebird_data_for_ARSET/Model_results/presabs.grid_07May2025.rds")
# Saved most recently 07May2025
#
## Make dataframes for plotting Marine Protected Area boundaries ----

pihmnmctrpts<-data.frame(Island="Palmyra",
                         Longitude=-162.072567,
                         Latitude=5.879722,
                         long360=(-162.072567+360))

palking.mpa<-data.frame(Longitude=c(-161.2008,-161.2008,-161.3077,-161.4228,
                                    -163.1878,-163.1878,-162.8876,-161.2008),
                        Latitude=c(7.243889,5.339722,5.188705,5.026111,
                                   5.026111,7.243889,7.243889,7.243889))

#
## Plot predicted results ----

ggplot(data=presabs.grid)+
  xlim(c(-164.75,-159.5))+
  ylim(c(3.25,7.75))+
  geom_tile(aes(x=long,y=lat,fill=BRT.fit))+
  scale_fill_viridis_c(option="viridis","Habitat suitability")+
  # Plot MPA & EEZ boundaries
  geom_polygon(data=palking.mpa,
               aes(x=Longitude,y=Latitude,group=group),
               fill=NA,col="black",lwd=0.75)+#PAL MPA shp
  # Add Palmyra point for context
  geom_point(data=pihmnmctrpts %>% filter(Island=="Palmyra"),
             aes(x=Longitude,y=Latitude),
             pch="*",size=5,color="black")+
  # Add PHIMNM Palmyra label
  geom_text(data=pihmnmctrpts %>% 
              filter(Island=="Palmyra"),
            aes(x=Longitude,y=Latitude-0.25),
            label = "Palmyra",
            size=4)+
  # Add PHIMNM label
  geom_text(data=pihmnmctrpts %>% 
              filter(Island=="Palmyra"),
            aes(x = -162.3, y = 4.75),
            label = "Pacific Islands Heritage\nMarine National Monument",
            size=4)+
  coord_equal(expand = FALSE)+
  theme(plot.title = element_text(hjust=0.9,size=14,color="white",
                                  margin=margin(b=-42)),
        axis.title = element_blank(),
        panel.background = element_rect(fill="white"),
        panel.border = element_rect(color="black",fill=NA),
        axis.text = element_text(color="black",size=12),
        plot.margin = unit(c(0,0,0,0), "cm"))
#