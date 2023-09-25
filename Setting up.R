setwd("C:/Users/Anri Sakakibara/OneDrive/PhD Political Economy/Vietnam War/")
library(tidyverse)
library(haven)
library(sf)
library(fixest)

#  SECTION 1A. HOUSEHOLD ROSTER, GENERAL CHARACTERISTICS OF HOUSEHOLD MEMBERS BELONGING TO HOUSEHOLD
varhs1_08 <- read_dta(file = "2008_new/Q1a_New.dta")
varhs1_10 <- read_dta(file = "2010_new/Q1_New_10.dta")
varhs1_12 <- read_dta(file = "2012_new/Q1_New_12.dta")
varhs1_14 <- read_dta(file = "2014_new/Q1_New_14.dta")
varhs1_16 <- read_dta(file = "2016_new/Q1_New_16.dta")

# LFS 

LFS_2015 <- read_dta(file = "LFS_2015_final_full.dta")

