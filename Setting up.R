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
library(mapview)

rm(list=ls())

# DHC 
dhc_97 <- read_dta(file = "DHC/VNIR31FL.dta")

# Population and Housing Census 

phc <- read_dta(file = "ipumsi_00006.dta")

# Population 

prewar_ppn <- read.csv("prewar_ppn.csv")

prewar_area <- prewar_ppn

postwar_ppn <- read.csv("Male and Female Population 1976.csv")

# Bombing data 

bombs_district <- read_dta(file = "Corrigendum_Impact-bombing-Vietnam_Code-Data_2023-07-24/data/clean/district_bombing_corrected.dta")
bombs_province_miguel <- read_dta(file = "Corrigendum_Impact-bombing-Vietnam_Code-Data_2023-07-24/data/clean/province_bombing_corrected.dta")

thor <- read.csv("datamil-vietnam-war-thor-data/datamil-vietnam-war-thor-data/thor_data_vietnam.csv")

weapons_dict <- read.csv("THOR_VIET_WEAPON_GLOSS.csv")

# Casualties data 

aad_folder <- "C:/Users/Anri Sakakibara/OneDrive/PhD Political Economy/Vietnam War/AAD"

casualties_csv_files <- list.files(path = aad_folder, pattern = "casualties(.*)csv$")

casualties_list <- lapply(casualties_csv_files, function(file) read.csv(file.path(aad_folder, file)))

cl_provcodes <- read.csv("AAD/cl_69.csv")

zones <- read.csv("AAD/Zone Codes.csv")

# Young Lives 

yl_anthro <- read_dta("YL/stata/stata13/vietnam_r5/vnyc_anthro_anon/vn_r5_ychh_youngerchildanthroandppvt.dta")

# Map 

vnmap0 <- read_sf("Chloropleth Maps/VNShapefile/gadm36_VNM_0.shp")
vnmap1 <- read_sf("Chloropleth Maps/VNShapefile/gadm36_VNM_1.shp")
vnmap2 <- read_sf("Chloropleth Maps/VNShapefile/gadm36_VNM_2.shp")
vnmap3 <- read_sf("Chloropleth Maps/VNShapefile/gadm36_VNM_3.shp")
hcmtrail <- st_read("3-replication package/3-replication package/rawdata/shapefiles/HoChiMinhGeoreference.shp")
bases <- st_read("3-replication package/3-replication package/rawdata/shapefiles/AirBases-point.shp")
popcenter <-
  st_read("3-replication package/3-replication package/rawdata/shapefiles/ne_10m_populated_places.shp")

provarea <- read.csv("province_area.csv")

district <- read_dta(file = "Consistent 2019 to 1999 wards with 1999 districts.dta")

vn_old <- read_sf("Chloropleth Maps/vnm_huyen.shp")

 # Consistent District Boundaries 

mccaig_boundaries <- read_dta("Consistent 2019 to 1999 wards with 1999 districts.dta")
