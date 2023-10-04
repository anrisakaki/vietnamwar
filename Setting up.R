setwd("C:/Users/Anri Sakakibara/OneDrive/PhD Political Economy/Vietnam War/")
library(tidyverse)
library(haven)
library(sf)
library(fixest)
library(extrafont)
library(lubridate)

# VARHS 

##  SECTION 1A. HOUSEHOLD ROSTER, GENERAL CHARACTERISTICS OF HOUSEHOLD MEMBERS BELONGING TO HOUSEHOLD
varhs1_08 <- read_dta(file = "2008_new/Q1a_New.dta")
varhs1_10 <- read_dta(file = "2010_new/Q1_New_10.dta")
varhs1_12 <- read_dta(file = "2012_new/Q1_New_12.dta")
varhs1_14 <- read_dta(file = "2014_new/Q1_New_14.dta")
varhs1_16 <- read_dta(file = "2016_new/Q1_New_16.dta")

## SECTION 5. OCCUPATION, TIME USE, AND OTHER SOURCES OF INCOME

varhs5_08 <- read_dta(file = "2008_new/Q5_New.dta")
varhs5_10 <- read_dta(file = "2010_new/Q5_New_10.dta")
varhs5_10 <- read_dta(file = "2010_new/Q5_New_10.dta")
varhs5_12 <- read_dta(file = "2012_new/Q5_New_12.dta")
varhs5_14 <- read_dta(file = "2014_new/Q5_New_14.dta")
varhs5_16 <- read_dta(file = "2016_new/Q5_New_16.dta")

## SECTION 5A. 5A Wage/salary employment (Fill in for all individuals who answered yes to Q1A in section 5A)

varhs5a_08 <- read_dta(file = "2008_new/Q5a_New.dta")
varhs5a_10 <- read_dta(file = "2010_new/Q5a_New_10.dta")
varhs5a_10 <- read_dta(file = "2010_new/Q5a_New_10.dta")
varhs5a_12 <- read_dta(file = "2012_new/Q5a_New_12.dta")
varhs5a_14 <- read_dta(file = "2014_new/Q5a_New_14.dta")
varhs5a_16 <- read_dta(file = "2016_new/Q5a_New_16.dta")

## 10A GROUP MEMBERSHIP

varhs10_08 <- read_dta(file = "2008_new/Q10a_New.dta")
varhs10_10 <- read_dta(file = "2010_new/Q10a_New_10.dta")
varhs10_10 <- read_dta(file = "2010_new/Q10a_New_10.dta")
varhs10_12 <- read_dta(file = "2012_new/Q10a_New_12.dta")
varhs10_14 <- read_dta(file = "2014_new/Q10a_New_14.dta")
varhs10_16 <- read_dta(file = "2016_new/Q10a_New_16.dta")

# LFS 

LFS_2015 <- read_dta(file = "LFS_2015_final_full.dta")

# VHLSS 

vhlss06 <- list.files(pattern = "muc(.*)dta$")
vhlss06 <- lapply(vhlss06, read_dta)

m1_06 <- vhlss06[[1]]
m2a_06 <- vhlss06[[2]]
m4a_06 <- vhlss06[[3]]
weights_vhlss <- read_dta("hhinc06.dta")

# Bombing data 

bombs_district <- read_dta(file = "Corrigendum_Impact-bombing-Vietnam_Code-Data_2023-07-24/data/clean/district_bombing_corrected.dta")
bombs_province <- read_dta(file = "Corrigendum_Impact-bombing-Vietnam_Code-Data_2023-07-24/data/clean/province_bombing_corrected.dta")

thor <- read.csv("datamil-vietnam-war-thor-data/datamil-vietnam-war-thor-data/thor_data_vietnam.csv")

weapons_dict <- read.csv("datamil-vietnam-war-thor-data/datamil-vietnam-war-thor-data/THOR_VIET_WEAPON_GLOSS.csv")

# Map 

vnmap0 <- read_sf("Chloropleth Maps/VNShapefile/gadm36_VNM_0.shp")
vnmap1 <- read_sf("Chloropleth Maps/VNShapefile/gadm36_VNM_1.shp")
vnmap2 <- read_sf("Chloropleth Maps/VNShapefile/gadm36_VNM_2.shp")
vnmap3 <- read_sf("Chloropleth Maps/VNShapefile/gadm36_VNM_3.shp")
