setwd("C:/Users/Anri Sakakibara/OneDrive/PhD Political Economy/Vietnam War/")
library(tidyverse)
library(haven)
library(sf)
library(fixest)
library(extrafont)
library(lubridate)
library(sjPlot)

rm(list=ls())

# Population and Housing Census 

phc <- read_dta(file = "ipumsi_00005.dta")

# Population 

population <- list.files(pattern = "Provincial(.*)csv$")
population <- lapply(population, read.csv)

prov6306 <- population[[1]]
prov_m_ppn5763 <- population[[2]]
prov_m_ppn7680 <- population[[3]]
prov_ppn5776 <- population[[4]]
prov_ppn7680 <- population[[5]]

ppn79  <- read.csv("Male and Female Population 1979.csv")

# LFS 

LFS_2015 <- read_dta(file = "LFS_2015_final_full.dta")

# VHLSS 

vhlss06 <- list.files(pattern = "muc(.*)dta$")
vhlss06 <- lapply(vhlss06, read_dta)

m1_06 <- vhlss06[[1]]
m2a_06 <- vhlss06[[2]]
m3_06 <- vhlss06[[3]]
m4a_06 <- vhlss06[[4]]
m4ho_06 <- vhlss06[[5]] 
weights_vhlss <- read_dta("hhinc06.dta")

# Bombing data 

bombs_district <- read_dta(file = "Corrigendum_Impact-bombing-Vietnam_Code-Data_2023-07-24/data/clean/district_bombing_corrected.dta")
bombs_province <- read_dta(file = "Corrigendum_Impact-bombing-Vietnam_Code-Data_2023-07-24/data/clean/province_bombing_corrected.dta")

thor <- read.csv("datamil-vietnam-war-thor-data/datamil-vietnam-war-thor-data/thor_data_vietnam.csv")
thor_district <- read.csv("thor_district.csv")

weapons_dict <- read.csv("datamil-vietnam-war-thor-data/datamil-vietnam-war-thor-data/THOR_VIET_WEAPON_GLOSS.csv")

# Young Lives 

yl_anthro <- read_dta("YL/stata/stata13/vietnam_r5/vnyc_anthro_anon/vn_r5_ychh_youngerchildanthroandppvt.dta")

# Map 

vnmap0 <- read_sf("Chloropleth Maps/VNShapefile/gadm36_VNM_0.shp")
vnmap1 <- read_sf("Chloropleth Maps/VNShapefile/gadm36_VNM_1.shp")
vnmap2 <- read_sf("Chloropleth Maps/VNShapefile/gadm36_VNM_2.shp")
vnmap3 <- read_sf("Chloropleth Maps/VNShapefile/gadm36_VNM_3.shp")

district <- read_dta(file = "Consistent 2019 to 1999 wards with 1999 districts.dta")

# schools 

educ <- list.files(pattern = "educ_(.*)csv$")
educ <- lapply(educ, read.csv)

educ_65 <- educ[[1]]
educ_67 <- educ[[2]]
educ_72 <- educ[[3]]

 # Consistent District Boundaries 

mccaig_boundaries <- read_dta("Consistent 2019 to 1999 wards with 1999 districts.dta")
