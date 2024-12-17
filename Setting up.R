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

rm(list=ls())

# VHLSS 
inc_02 <- read_dta(file = "Full VHLSS/VHLSS 2002 N= 45,000/hhinc02.dta")
exp_02 <- read_dta(file = "Full VHLSS/VHLSS 2002 N= 45,000/hhexpe02.dta")
m1_02 <- read_dta(file = "Full VHLSS/VHLSS 2002 N= 45,000/muc1.dta") #DF containing age and sex
m2_02 <- read_dta (file = "Full VHLSS/VHLSS 2002 N= 45,000/muc2.dta") #DF containing educational attainment 
m3_02 <- read_dta (file = "Full VHLSS/VHLSS 2002 N= 45,000/muc3.dta") #DF containing work 
m5a_02 <- read_dta (file = "Full VHLSS/VHLSS 2002 N= 45,000/muc5a.dta") #DF containing work
m5c_02 <- read_dta(file = "Full VHLSS/VHLSS 2002 N= 45,000/muc5c1.dta")
m9_02 <- read_dta(file = "Full VHLSS/VHLSS 2002 N= 45,000/muc9.dta")

inc_04 <- read_dta(file = "Full VHLSS/2004/hhinc04.dta")
ho1_04 <- read_dta(file = "Full VHLSS/2004/ho1.dta")
ho2_04 <- read_dta(file = "Full VHLSS/2004/ho2.dta")
m123a_04 <- read_dta(file = "Full VHLSS/2004/m1_2_3a.dta") #DF containing age, sex, and educational attainment 
m4a_04 <- read_dta(file = "Full VHLSS/2004/m4a.dta") #DF containing work 
m4c1_04 <- read_dta(file = "Full VHLSS/2004/m4c1.dta") #DF containing work 

ttchung_06 <- read_dta(file = "Full VHLSS/2006/ttchung.dta")
inc_06 <- read_dta(file = "Full VHLSS/2006/hhinc06.dta")
m1a_06 <- read_dta(file = "Full VHLSS/2006/muc1a.dta") #DF containing age and sex 
m2a_06 <- read_dta(file = "Full VHLSS/2006/muc2a.dta") #DF containing educational attainment 
m4a_06 <- read_dta(file = "Full VHLSS/2006/muc4a.dta") #DF containing employment & housework 
m4c_06 <- read_dta(file = "Full VHLSS/2006/muc4c.dta")

m123a_08 <- read_dta(file = "Full VHLSS/2008/Hhold/muc123a.dta")
m4a_08 <- read_dta(file = "Full VHLSS/2008/Hhold/muc4a.dta")
m4c_08 <- read_dta(file = "Full VHLSS/2008/Hhold/muc4c1.dta")
wt08 <- read_dta(file = "Full VHLSS/2008/Hhold/weight08new4.dta")
ho_08 <- read_dta(file = "Full VHLSS/2008/Hhold/ho.dta")

m1a_10 <- read_dta(file = "Full VHLSS/2010/muc1a.dta")
m2a_10 <- read_dta(file = "Full VHLSS/2010/muc2a1.dta")
m4a1_10 <- read_dta(file = "Full VHLSS/2010/muc4a1.dta")
m4a2_10 <- read_dta(file = "Full VHLSS/2010/muc4a2.dta")
m4a3_10 <- read_dta(file = "Full VHLSS/2010/muc4a3.dta")
m4a4_10 <- read_dta(file = "Full VHLSS/2010/muc4a4.dta")
m4c_10 <- read_dta(file = "Full VHLSS/2010/muc4c1.dta")
wt10 <- read_dta(file = "Full VHLSS/2010/wt10.dta")
ho11_10 <- read_dta(file = "Full VHLSS/2010/ho11.dta")

m1a_12 <- read_dta(file = "Full VHLSS/2012/Muc1A.dta")
m2a1_12 <- read_dta(file = "Full VHLSS/2012/Muc2a1.dta")
wt12 <- read_dta(file = "Full VHLSS/2012/wt2012new.dta")
ho11_12 <- read_dta(file = "Full VHLSS/2012/ho11.dta")

ho1_14 <- read_dta(file = "Full VHLSS/2014/ho1.dta")
m1a_14 <- read_dta(file = "Full VHLSS/2014/Muc1A.dta")
m2a_14 <- read_dta(file = "Full VHLSS/2014/Muc2A.dta")
m4a_14 <- read_dta(file = "Full VHLSS/2014/Muc4a.dta")

m1a_16 <- read_dta(file = "Full VHLSS/2016/Household/Muc1A.dta")
m2ab_16 <- read_dta(file = "Full VHLSS/2016/Household/Muc2AB.dta")
m4a_16 <- read_dta(file = "Full VHLSS/2016/Household/Muc4A.dta")
wt16 <- read_dta(file = "Full VHLSS/2016/Household/wt16.dta")

m1a_18 <- read_dta(file = "Full VHLSS/2018/2 - Data/1 - Households/MUC1A.dta")
m2c_18 <- read_dta(file = "Full VHLSS/2018/2 - Data/1 - Households/MUC2V.dta")
m4a_18 <- read_dta(file = "Full VHLSS/2018/2 - Data/1 - Households/MUC4A.dta")

# Population and Housing Census 

phc <- read_dta(file = "ipumsi_00010.dta")

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

# Map 
geoid_folder <- "C:/Users/Anri Sakakibara/OneDrive/PhD Political Economy/Vietnam War/geoid"
geoid_files <- list.files(path = geoid_folder, pattern = "geo(.*)csv$")
geoid_list <- lapply(geoid_files, function(file) read.csv(file.path(geoid_folder, file)))

district09 <- read_sf("diaphanhuyen/Dia_phan_Huyen.shp")

vnmap0 <- read_sf("Chloropleth Maps/VNShapefile/gadm36_VNM_0.shp")
vnmap1 <- read_sf("Chloropleth Maps/VNShapefile/gadm36_VNM_1.shp")
vnmap2 <- read_sf("Chloropleth Maps/VNShapefile/gadm36_VNM_2.shp")
vnmap3 <- read_sf("Chloropleth Maps/VNShapefile/gadm36_VNM_3.shp")
hcmtrail <- st_read("3-replication package/3-replication package/rawdata/shapefiles/HoChiMinhGeoreference.shp")
bases <- st_read("3-replication package/3-replication package/rawdata/shapefiles/AirBases-point.shp")
popcenter <-
  st_read("3-replication package/3-replication package/rawdata/shapefiles/ne_10m_populated_places.shp")

geo2_vn <- read_sf("geo2_vn2009_2019/geo2_vn2009_2019.shp")
  
# Consistent District Boundaries 

mccaig_boundaries <- read_dta("Consistent 2019 to 1999 wards with 1999 districts.dta")
