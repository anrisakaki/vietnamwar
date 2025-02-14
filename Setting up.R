setwd("C:/Users/Anri Sakakibara/OneDrive/PhD Political Economy/Vietnam War/")
library(tidyverse)
library(haven)
library(sf)
library(fixest)
library(extrafont)
library(lubridate)
library(sjPlot)
library(sfheaders)
library(mgrs)
library(broom)
library(patchwork)
library(srvyr)
library(survey)
library(raster)
library(exactextractr)

rm(list=ls())

# VHLSS 

ho1_14 <- read_dta(file = "Full VHLSS/2014/Ho1.dta")
m1a_14 <- read_dta(file = "Full VHLSS/2014/Muc1A.dta")
m2a_14 <- read_dta(file = "Full VHLSS/2014/Muc2A.dta")
m4a_14 <- read_dta(file = "Full VHLSS/2014/Muc4A.dta")
wt14 <- read_dta(file = "Full VHLSS/2014/wt2014.dta")

ho1_16 <- read_dta(file = "Full VHLSS/2016/Household/Ho1.dta")
m1a_16 <- read_dta(file = "Full VHLSS/2016/Household/Muc1A.dta")
m2ab_16 <- read_dta(file = "Full VHLSS/2016/Household/Muc2AB.dta")
m4a_16 <- read_dta(file = "Full VHLSS/2016/Household/Muc4A.dta")
wt16 <- read_dta(file = "Full VHLSS/2016/Household/wt16.dta")

# Population and Housing Census 

phc <- read_dta(file = "ipumsi_00011.dta")

# Bombing data 
thor <- read.csv("datamil-vietnam-war-thor-data/datamil-vietnam-war-thor-data/thor_data_vietnam.csv")

weapons_dict <- read.csv("THOR_VIET_WEAPON_GLOSS.csv")

# Casualties data 

aad_folder <- "C:/Users/Anri Sakakibara/OneDrive/PhD Political Economy/Vietnam War/AAD"

casualties_csv_files <- list.files(path = aad_folder, pattern = "casualties(.*)csv$")

casualties_list <- lapply(casualties_csv_files, function(file) read.csv(file.path(aad_folder, file)))

cl_provcodes <- read.csv("AAD/cl_69.csv")

zones <- read.csv("AAD/Zone Codes.csv")

# Map 
geoid_folder <- "C:/Users/Anri Sakakibara/OneDrive/PhD Political Economy/Vietnam War/geoid"
geoid_files <- list.files(path = geoid_folder, pattern = "geo(.*)csv$")
geoid_list <- lapply(geoid_files, function(file) read.csv(file.path(geoid_folder, file)))

vnmap0 <- read_sf("Chloropleth Maps/VNShapefile/gadm36_VNM_0.shp")
vnmap1 <- read_sf("Chloropleth Maps/VNShapefile/gadm36_VNM_1.shp")
vnmap2 <- read_sf("Chloropleth Maps/VNShapefile/gadm36_VNM_2.shp")
vnmap3 <- read_sf("Chloropleth Maps/VNShapefile/gadm36_VNM_3.shp")
hcmtrail <- st_read("3-replication package/3-replication package/rawdata/shapefiles/HoChiMinhGeoreference.shp")
bases <- st_read("3-replication package/3-replication package/rawdata/shapefiles/AirBases-point.shp")

geo2_vn <- read_sf("geo2_vn2009_2019/geo2_vn2009_2019.shp")

altitude <- raster("VNM_alt_tif/VNM_alt.tif")
ruggedness <- raster("nunnpuga/tri.tif")
precipitation <- raster("prec_30s_bil/avg.tif")
