rm(list=ls())
library(countrycode)
library(data.table)
library(tidyverse)
library(ggrepel)
library(ggpubr)
library(here)
library(reldist)
library(latex2exp)
library(scales)
if (!require("icaeDesign")){
  devtools::install_github("graebnerc/icaeDesign")
}
Fulldata <- fread(here("data/macro_data_fig1-2_4_NEW.csv"))
#Fulldata <- dplyr::select(Fulldata, iso3c, year, gdp_real_pc_ppp)

#bind with eci
#setwd("~/Desktop/wiiw_Fiskalpolitik_Projektphase_3/Special Issue/R files/structural-corona-crisis-master/data/AMECO forecast April 2020")
eci <- fread(here("data/macro_data_fig1-2_4.csv"))

eci$ECI <- eci$eci_harv_hs
eci <- dplyr::select(eci, iso3c, year, ECI) 

eci$year <- as.character(eci$year)
Fulldata$year <- as.character(Fulldata$year)

Fulldata_merged <- dplyr::left_join(Fulldata, eci, by=c("iso3c" = "iso3c", "year" = "year"))

write.csv(Fulldata_merged, "macro_data_fig1-2_4_NEW_incl_eci.csv")
