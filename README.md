

<center>

![](https://appliedsciences.nasa.gov/sites/default/files/styles/homepage_hero/public/2025-03/Website_Header_2.jpg.webp?itok=GMagiGhW)

# Introduction to the Integration of Animal Tracking and Remote Sensing

</center>

### **Overview**

Animals are sentinels of environmental change, and animal telemetry is a commonly used tool to quantify habitat use and help understand environmental changes. NASA data can be used to characterize the environmental parameters that infer the habitats that animals use. This training course will provide participants with an overview of animal tracking sensors, NASA’s history of animal tracking, [NASA’s Internet of Animals project](https://www.nasa.gov/nasa-earth-exchange-nex/new-missions-support/internet-of-animals/), and the types of remote sensing data that can be paired with animal telemetry. Because animal telemetry collects frequent animal location data, it is important to consider time-matched remote sensing in data analyses. In remote marine environments, for example, Level 3 and Level 4 products provide the most complete spatiotemporal coverage, such as OSCAR for ocean surface currents. 

Participants will then learn how to integrate telemetry and remote sensing data by applying a basic data standardization process to animal tracking data, visualizing the animals’ distribution via home ranges with utilization distributions, downloading remote sensing data, and characterizing animals’ habitats in a species distribution model to infer habitat use. The balance of tradeoffs (spatiotemporal mismatches; computational power and time) from pairing remotely sensed data with animal tracks will be discussed. Examples for both marine and terrestrial environments will be provided. 

### **Prerequisites**

-   [Fundamentals of Remote Sensing](https://appliedsciences.nasa.gov/get-involved/training/english/arset-fundamentals-remote-sensing "ARSET - Fundamentals of Remote Sensing")

-   This training assumes that participants will have some knowledge of animal behavior, ecological processes, basic statistics, a basic understanding of remote sensing, and know how to use the program R.

### **Objective**

By the end of this training attendees will be able to:

1.  Identify the types of animal tracking tags and sensors that are commonly used in animal tracking.

2.  Identify the types of remote sensing data and products that can be used for species distribution models and step-selection functions.

3.  Recognize the process for integrating remote sensing and animal tracking data in species distribution models and step selection functions to facilitate an understanding of animal movements in relation to their environment.

4.  Recognize key takeaways from examples of terrestrial and marine applications that inform and characterize animals’ habitats. 

### **Audience**

Primary target audience: Movement ecologists, natural resource managers\
Secondary target audience: Remote sensing scientists developing products usable by the primary audience

### **Course Format**

Two 90-minute parts, each including a 30 minute Q&A session

### **Sessions**

**Part 1: Introduction to Animal Tracking and Remote Sensing at NASA**

Tuesday, May 20, 2025

ARSET trainers: Juan Torres-Pérez\
Guest Instructors: Dr. Morgan Gilmour (NASA Ames Research Center), Claire Teitelbaum (Assistant Unit Leader, Georgia Cooperative Fish and Wildlife Research Unit)

-   Overview of animal tracking

-   How is remote sensing related to animal tracking?

-   Working with animal tracking data

-   Summary

-   Q&A session

**Part 2: Integration of Animal Tracking and Remote Sensing Data**

Thursday, May 22, 2025

ARSET trainers: Juan Torres-Pérez\
Guest Instructors: Dr. Morgan Gilmour (NASA Ames Research Center), Claire Teitelbaum (Assistant Unit Leader, Georgia Cooperative Fish and Wildlife Research Unit)

-   Integration of animal tracking and remote sensing: Case study (Marine)

-   Integration of animal tracking and remote sensing: Case study (Terrestrial)

-   Summary

-   Q&A session

------------------------------------------------------------------------

### In this repository

-	1-append_evars_ARSET.R
  -	Script to demonstrate downloading and extracting remotely sensed environmental covariates
-	2-pseudo-absence_ARSET.R
  -	Script to simulate background pseudo-absences for Species Distribution Model
-	3-make_presabs_dataset_ARSET.R
  -	Script to make presence-absence dataframe that will be used in Species Distribution Model
-	4-BRT_ARSET.R
  -	Script to run Boosted Regressoin Tree that will be used in Species Distribution Model
-	5-Predict_SDM_ARSET.R
  -	Script to apply Boosted Regression Tree model to grid of unsampled region
-	6-eval_SDM_ARSET.R
  -	Script to evaluate Species Distribution Model
-	Functions_for_animal_tracking_ARSET2025.R
  -	Functions used in ARSET training
-	Deer_SSF.R
  -	Script to run a step-selection function for a red deer. This script draws from the "amt" R package vignette on step-selection functions: https://cran.r-project.org/web/packages/amt/vignettes/p4_SSF.html
- LC_500m_crop.tif
  - MODIS MCD12Q1 Version 6.1 annual land cover product, cropped to the area of the deer data. See Deer_SSF.R for data processing steps. DOI: 10.5067/MODIS/MCD12Q1.006
- NDVI_250m_16d_crop.tif
  - MODIS MCD13Q1 16-day vegetation index product, cropped to the area of the deer data. See Deer_SSF.R for data processing steps. DOI: 10.5067/MODIS/MOD13Q1.061

------------------------------------------------------------------------
#### About NASA ARSET

ARSET offers online and in-person trainings for beginners and advanced practitioners alike. Trainings cover a range of datasets, web portals, and analysis tools and their application to air quality, agriculture, disasters, land, and water resource management. Since 2009, the program has reached more than 100,000 participants from 183 countries and more than 17,000 organizations worldwide.

[Find ARSET Trainings](https://appliedsciences.nasa.gov/get-involved/training)

<center>

![](https://sdghelpdesk.unescap.org/sites/default/files/2021-03/NASA%20ARSET_3.png)

</center>

------------------------------------------------------------------------

##### Citations

**This Training:**

(2025). *ARSET - Introduction to the Integration of Animal Tracking and Remote Sensing*. NASA Applied Remote Sensing Training Program (ARSET). <https://appliedsciences.nasa.gov/get-involved/training/english/arset-introduction-integration-animal-tracking-and-remote-sensing>

**Data Information:**
- Species Distribution Model functions adapted from Hazen et al. 2021 "Where did they not go? Considerations for generating pseudo-absences for telemetry-based habitat models." Movement Ecology 9:5 https://doi.org/10.1186/s40462-021-00240-2 and are available for download from https://github.com/elhazen/PA-paper 
- To search and access remotely sensed environmental covariates via NOAA ERDDAP: https://upwell.pfeg.noaa.gov/erddap/index.html 
- Chlorophyll-a dataset: data title: “erdMBchlamday_LonPM180”, https://coastwatch.pfeg.noaa.gov/infog/MB_chla_las.html
- Sea surface temperature dataset: data title: “jplMURSST41mday”, JPL MUR MEaSUREs Project. 2015. GHRSST Level 4 MUR Global Foundation Sea Surface Temperature Analysis. Ver. 4.1. PO.DAAC, CA, USA. DOI: https://doi.org/10.5067/GHGMR-4FJ04 
- Bathymetric depth dataset: NOAA National Centers for Environmental Information ETOPO2 database, https://www.doi.org/10.25921/fd45-gt74 


