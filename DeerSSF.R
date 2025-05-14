################################################################X
#--------------------Step-selection analysis--------------------X
#--------------------------May 24, 2025-------------------------X
################################################################X

# Claire Teitelbaum <cteitelbaum@uga.edu>

# In this demo, we will use data provided by the 'amt' package to implement an SSF.
# Much of this is drawn from the 'amt' vignette on SSFs (https://cran.r-project.org/web/packages/amt/vignettes/p4_SSF.html)

######################### SETUP #################################

library(amt) #SSF functions in this package
library(sf); library(terra) #packages for spatial analysis of vector and raster data, respectively
library(lubridate); library(stringr) #deal with dates and characters
library(dplyr)
library(ggplot2)

######################### READ IN AND EXPLORE TELEMETRY DATA #################################
#load the "deer" data set from the amt package
data("deer")
#let's take a look at the data
head(deer) 
?deer #or we can look at it this way. this tells us the coordinate system (epsg:3035)
#notice that the data is pre-cleaned and formatted
#your column names and formats might differ

######################### EXPLORE DATA AND SIMULATE AVAILABLE STEPS #################################
plot(deer)

#sampling rate
summarize_sampling_rate(deer) #built-in function from amt package
#alternatively:
deer <- mutate(deer, dt = difftime(t_, lag(t_), units = "hours"))
summary(as.numeric(deer$dt))
hist(as.numeric(deer$dt))
ggplot(deer, aes(x = t_)) + geom_histogram(fill = "lightgrey", color = "black")

#get step lengths and turn angles by burst (deer data are pre-processed to include bursts)
?steps_by_burst
deer_steps <- steps_by_burst(deer)
head(deer_steps) #now we have "from" and "to" and data on steps (direction, step length, turn angle)

#simulate available steps
?random_steps
set.seed(10)
deer_random <- random_steps(deer_steps, n_control = 10)
head(deer_random)
View(deer_random)
plot(deer_random)

#simulate available steps without estimating a distribution
deer_random2 <- random_steps(deer_steps, 
                             rand_ta = runif(1000, 0, pi), 
                             rand_sl = runif(1000, min(deer_steps$sl_), max(deer_steps$sl_)))
plot(deer_random2)
#this looks unrealistic, so let's not use it, but remember you can play with these as needed


######################### GET REMOTE SENSING LAYERS #################################

#to identify bounding box for EarthData downloads, let's convert coordinates to lat/long
deer_geo <- transform_coords(deer, crs_from = 3035, crs_to = 4326)
plot(deer_geo)
bbox(deer_geo)
#use 9, 54, 10, 55 as bounding box
range(deer$t_)
#use 2008-03-20 to 2009-05-08
#transition to EarthData for data download: https://search.earthdata.nasa.gov/search 
#I placed these files in a subfolder called NDVI_MODIS

file_list <- paste0("NDVI_MODIS/",dir("NDVI_MODIS/"))
r1 <- sds(file_list[1]) #file name includes date of acquisition
r1 <- rast(r1)
names(r1)
plot(r1[[1]])

#read in all rasters
all_veg <- rast(file_list, subds = 1) #just read in the first layer ("250m 16 days NDVI")
#extract dates from file names
dates <- varnames(all_veg)
dates <- str_sub(dates, 10, 16)
dates <- as_date(dates, format = "%Y%j") #check out ?strptime. %Y = 4-digit year; %j = 3-digit day of year
#set times in the raster itself
time(all_veg) <- dates
#convert to NDVI values (from integers): https://lpdaac.usgs.gov/products/mod13q1v061/
all_veg <- all_veg*0.0001
all_veg[all_veg == -3000] <- NA
plot(all_veg[[1]])
all_veg <- all_veg*0.0001
plot(all_veg[[1]])

#MCD12Q1 (MODIS land cover)
file_list <- paste0("LC_MODIS/",dir("LC_MODIS/"))
r1 <- sds(file_list[1]) #file name includes date of acquisition
r1
names(r1)
#https://lpdaac.usgs.gov/products/mcd12q1v061/
plot(r1[1] %in% c(1:5,8)) #forests

#read in all rasters
all_lc <- rast(file_list, subds = 1) #just read in the first layer (LC_Type1)
#extract dates from file names
dates <- varnames(all_lc)
dates <- str_sub(dates, 10, 16)
dates <- as_date(dates, format = "%Y%j") #check out ?strptime. %Y = 4-digit year; %j = 3-digit day of year
#set times in the raster itself
time(all_lc) <- dates + months(6)


######################### EXTRACT ENVIRONMENT AT POINTS #################################
#raster and points must be in the same projection for extraction
all_veg_proj <- project(all_veg, "epsg:3035", method = "bilinear", use_gdal = T)
all_lc_proj <- project(all_lc, "epsg:3035", method = "near", use_gdal = T) #use nearest neighbor for categorical data
plot(all_veg_proj[[1]])
#crop for future use/visualization of actual study area
all_veg_proj_crop <- crop(all_veg_proj, bbox(deer_random))
all_lc_proj_crop <- crop(all_lc_proj, bbox(deer_random))
plot(all_veg_proj_crop[[1]])
writeRaster(all_veg_proj_crop, "NDVI_250m_16d_crop.tif")
writeRaster(all_lc_proj_crop, "LC_500m_crop.tif")
all_veg_proj_crop <- rast("NDVI_250m_16d_crop.tif")
all_lc_proj_crop <- rast("LC_500m_crop.tif")

?extract_covariates
deer_veg <- extract_covariates_var_time(deer_random, all_veg_proj, 
                                        max_time = days(8),
                                        where = "end",
                                        name_covar = "ndvi")
deer_lc <- extract_covariates_var_time(deer_random, all_lc_proj, 
                                        max_time = years(1),
                                        where = "end",
                                        name_covar = "landcover")
deer_env <- left_join(deer_veg, deer_lc)
names(deer_env)
ggplot(deer_env, aes(x = ndvi)) + geom_histogram(color = "black", aes(fill = case_))
summary(factor(deer_env$landcover))
deer_env <- mutate(deer_env, 
                   forest = as.integer(landcover %in% c(1:5,8)), 
                   crop = as.integer(landcover %in% c(12,14)))

######################### RUN STEP SELECTION ANALYSIS #################################
#check for collinearity
boxplot(deer_env$ndvi ~ deer_env$forest) 
boxplot(deer_env$ndvi ~ deer_env$crop) 

#check for missing data
mean(is.na(deer_env$ndvi)) 
mean(is.na(deer_env$crop))

?fit_clogit
ssf_deer <- fit_clogit(case_ ~ ndvi + crop + strata(step_id_), 
                       data = deer_env, model = T)
summary(ssf_deer)

######################### VISUALIZE STEP SELECTION RESULTS #################################

#Relative Strength of Selection
prediction_ndvi <- seq(min(deer_env$ndvi), max(deer_env$ndvi), length.out = 20)
prediction_ndvi <- bind_cols(ndvi = prediction_ndvi, crop = 0)
ref_loc <- bind_cols(ndvi = median(deer_env$ndvi[deer_env$case_==1]),
                     crop = 0)
rss_vals <- log_rss(ssf_deer, x1 = prediction_ndvi, x2 = ref_loc)
plot(rss_vals$df$log_rss ~ rss_vals$df$ndvi_x1, type = "l",
     xlab = "NDVI", ylab = "log-RSS")

#Mapping Relative Strength of Selection
prediction_ndvi <- bind_cols(ndvi = values(all_veg_proj_crop[[1]]), crop = 0)
ref_loc <- bind_cols(ndvi = median(values(all_veg_proj_crop[[1]])), crop = 0)
rss_vals <- log_rss(ssf_deer, x1 = prediction_ndvi, x2 = ref_loc)
prediction_raster <- all_veg_proj_crop[[1]]
values(prediction_raster) <- rss_vals$df$log_rss
plot(prediction_raster)
points(deer)

pred_raster_df <- prediction_raster %>%
  as.data.frame(xy = T) %>% rename(pred_val = 3)
deer_sf <- st_as_sf(deer, coords = c("x_","y_"), crs = "epsg:3035")
ggplot(deer_sf) + 
  geom_tile(data = pred_raster_df, aes(x = x, y = y, fill = pred_val)) +
  geom_sf(alpha = 0.25) +
  theme_classic() +
  scale_fill_gradient2("log-RSS") +
  xlab("Longitude") + ylab("Latitude")
ggsave("../logRSS_map.png", width = 6, height = 4)




