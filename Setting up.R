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

rm(list=ls())

# VHLSS 
inc_02 <- read_dta(file = "Full VHLSS/VHLSS 2002 N= 45,000/hhinc02.dta")
m1_02 <- read_dta(file = "Full VHLSS/VHLSS 2002 N= 45,000/muc1.dta") #DF containing age and sex
m2_02 <- read_dta (file = "Full VHLSS/VHLSS 2002 N= 45,000/muc2.dta") #DF containing educational attainment 
m3_02 <- read_dta (file = "Full VHLSS/VHLSS 2002 N= 45,000/muc3.dta") #DF containing work 
m5a_02 <- read_dta (file = "Full VHLSS/VHLSS 2002 N= 45,000/muc5a.dta") #DF containing work

inc_04 <- read_dta(file = "Full VHLSS/2004/hhinc04.dta")
ho1_04 <- read_dta(file = "Full VHLSS/2004/ho1.dta")
ho2_04 <- read_dta(file = "Full VHLSS/2004/ho2.dta")
m123a_04 <- read_dta(file = "Full VHLSS/2004/m1_2_3a.dta") #DF containing age, sex, and educational attainment 
m4a_04 <- read_dta(file = "Full VHLSS/2004/m4a.dta") #DF containing work 
m10a_04 <- read_dta(file = "Full VHLSS/2004/m10a_e3.dta")

ttchung_06 <- read_dta(file = "Full VHLSS/2006/ttchung.dta")
inc_06 <- read_dta(file = "Full VHLSS/2006/hhinc06.dta")
m1a_06 <- read_dta(file = "Full VHLSS/2006/muc1a.dta") #DF containing age and sex 
m2a_06 <- read_dta(file = "Full VHLSS/2006/muc2a.dta") #DF containing educational attainment 
m4a_06 <- read_dta(file = "Full VHLSS/2006/muc4a.dta") #DF containing employment & housework 

m123a_08 <- read_dta(file = "Full VHLSS/2008/Hhold/muc123a.dta")
m4a_08 <- read_dta(file = "Full VHLSS/2008/Hhold/muc4a.dta")
wt08 <- read_dta(file = "Full VHLSS/2008/Hhold/weight08new4.dta")

m1a_10 <- read_dta(file = "Full VHLSS/2010/muc1a.dta")
m2a_10 <- read_dta(file = "Full VHLSS/2010/muc2a1.dta")
m4a1_10 <- read_dta(file = "Full VHLSS/2010/muc4a1.dta")
m4a2_10 <- read_dta(file = "Full VHLSS/2010/muc4a2.dta")
m4a3_10 <- read_dta(file = "Full VHLSS/2010/muc4a3.dta")
m4a4_10 <- read_dta(file = "Full VHLSS/2010/muc4a4.dta")
wt10 <- read_dta(file = "Full VHLSS/2010/wt10.dta")

m1a_12 <- read_dta(file = "Full VHLSS/2012/Muc1A.dta")
m2a1_12 <- read_dta(file = "Full VHLSS/2012/Muc2a1.dta")
wt12 <- read_dta(file = "Full VHLSS/2012/wt2012new.dta")

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

busid <- read_dta(file = "Full VHLSS/Business ids.dta")

# DHC 
dhc_97 <- read_dta(file = "DHC/VNIR31FL.dta")

# Population and Housing Census 

phc <- read_dta(file = "ipumsi_00006.dta")

# Enterprise Survey 

ec <- list.files("Enterprise Census", pattern = "^dn.*\\.dta$", full.names = TRUE)

ec_list <- lapply(ec, read_dta)

dn00 <- ec_list[[1]]
dn01 <- ec_list[[2]]
dn02 <- ec_list[[3]]
dn03 <- ec_list[[4]]
dn04 <- ec_list[[5]]
dn05 <- ec_list[[6]]
dn06 <- ec_list[[7]]
dn07 <- ec_list[[8]]
dn08 <- ec_list[[9]]
dn09 <- ec_list[[10]]
dn10 <- ec_list[[11]]
dn11 <- ec_list[[12]]
dn12 <- ec_list[[13]]
dn13 <- ec_list[[14]]
dn14 <- ec_list[[15]]
dn15 <- ec_list[[16]]
dn16 <- ec_list[[17]]
dn17 <- ec_list[[18]]
dn18 <- ec_list[[19]]

# Population 

prewar_ppn <- read.csv("prewar_ppn.csv")

prewar_area <- prewar_ppn

postwar_ppn <- read.csv("Male and Female Population 1976.csv")

sexratios <- read.csv("E02.09.csv")

ppn60 <- read.csv("ppn60.csv")

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
geoid_folder <- "C:/Users/Anri Sakakibara/OneDrive/PhD Political Economy/Vietnam War/geoid"
geoid_files <- list.files(path = geoid_folder, pattern = "geo(.*)csv$")
geoid_list <- lapply(geoid_files, function(file) read.csv(file.path(geoid_folder, file)))

vnmap0 <- read_sf("Chloropleth Maps/VNShapefile/gadm36_VNM_0.shp")
vnmap1 <- read_sf("Chloropleth Maps/VNShapefile/gadm36_VNM_1.shp")
vnmap2 <- read_sf("Chloropleth Maps/VNShapefile/gadm36_VNM_2.shp")
vnmap3 <- read_sf("Chloropleth Maps/VNShapefile/gadm36_VNM_3.shp")
hcmtrail <- st_read("3-replication package/3-replication package/rawdata/shapefiles/HoChiMinhGeoreference.shp")
bases <- st_read("3-replication package/3-replication package/rawdata/shapefiles/AirBases-point.shp")
popcenter <-
  st_read("3-replication package/3-replication package/rawdata/shapefiles/ne_10m_populated_places.shp")

vnmap2_09 <- read_sf("Chloropleth Maps/VNShapefile/geo2_vn2009/geo2_vn2009.shp")

provarea <- read.csv("province_area.csv")

vn_old <- read_sf("Chloropleth Maps/vnm_huyen.shp")

test <- read_sf("dia-gioi-hanh-chinh/Ranh_GioiVN2000.shp")

# Consistent District Boundaries 

mccaig_boundaries <- read_dta("Consistent 2019 to 1999 wards with 1999 districts.dta")
