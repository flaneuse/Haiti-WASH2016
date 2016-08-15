# Haiti DHS Activity Design -----------------------------------------
# Script to pull data from the Demographic and Health Surveys on variables
# related to water, sanitation, and hygiene (WASH)
# 
# Data are from the 2013 DHS, available at http://dhsprogram.com/what-we-do/survey/survey-display-436.cfm
#
# Laura Hughes, lhughes@usaid.gov, 15 August 2016
# With Patrick Gault (pgault@usaid.gov) and Tim Essam (tessam@usaid.gov)
#
# Copyright 2016 by Laura Hughes via MIT License
#
# -------------------------------------------------------------------------


# Import data -------------------------------------------------------------

# Load necessary packages
library(haven)
library(dplyr)

# Function to pull attribute (label, labels) data from Stata format
pullAttributes <- function(data) {
  
  metadata = lapply(data, function(x) attr(x, 'label'))
  metadata = data.frame(metadata)
  
  labels = lapply(data, function(x) attr(x, 'labels'))
  
  metadata = data.frame(var = colnames(metadata), varDescrip = t(metadata))
  
  df = mutate(metadata, varValues = labels)
  return(df)
}

# Read in the base module and the children's

#' DHS modules are as follows:
#' (from http://dhsprogram.com/data/Dataset-Types.cfm)
#' * HR: household data (household level description)
#' * PR: household list (inidividual roster)
#' * IR: women's module (one woman/hh)
#' * MR: men's module (one man/hh)
#' * CR: couple's module
#' * KR: children's module (ea. child < 5 y old)
#' * BR: birth data (birth history of all children) --> fertility / mortality rates
#' * WI: wealth index data (for data before 1990 to calcualte wealth index)

hh_raw = read_dta('~/Documents/USAID/Haiti/rawdata/Haiti_DHS2012/hthr61dt/HTHR61FL.DTA')
child_raw = read_dta('~/Documents/USAID/Haiti/rawdata/Haiti_DHS2012/htkr61dt/HTKR61FL.DTA')

hh_labels = pullAttributes(hh_raw)
child_labels = pullAttributes(child_raw)

hh = removeAttributes(hh_raw)
child = removeAttributes(child_raw)


# classify improved/ not improved -----------------------------------------

# toilets
toilet_types = attr(hh_raw$hv205, 'labels')
write.csv(toilet_types, '~/GitHub/Haiti-WASH2016/dataout/DHS_toilet_types.csv')

water_types = attr(hh_raw$hv201, 'labels')
write.csv(water_types, '~/GitHub/Haiti-WASH2016/dataout/DHS_water_types.csv')

# Pull diarrhea, water-sanitation access, geo data ------------------------
hh = hh %>% 
  select(hhid, 
         interview_month = hv006, interview_year = hv007,
         water_source = hv201, 
         time2water = hv204, 
         toilet_source = hv205,
         share_toilet = hv225, water_treatment = hv237, hv237a, hv237b, hv237c, hv237d, hv237e, hv237f, hv237x, hv237z, 
         owns_mobile = hv243a, 
         wealth_idx = hv270,
         province = shprov,
         munic =  shmunic,
         barang = shbarang,
         ea = shea,
         region = shregion,
         island_grp = shislgrp)

diarrhea =  hh_raw %>% 
  select(hhid, sh204m_01, sh204m_02, sh204m_03, sh204m_04, sh204m_05, sh204m_06, sh204m_07, sh204m_08, sh204m_09, sh204m_10, sh204m_11, sh204m_12) %>% 
  group_by(hhid) %>% 
  summarise_each(funs(mean))


# Figuring out Philippines geography --------------------------------------
# Island Group and Region  are the only ones labeled within the dataset and seems to be their primary reporting unit.
# island_grp: 3 [Should be Luzon, Visayas, and Mindanao]
# region: 17 [Wiki says 18 as of 2015 https://en.wikipedia.org/wiki/Regions_of_the_Philippines]
# munic: 48 [Wiki says 1490??!]
# province: 84 [Wiki says 81 provinces https://en.wikipedia.org/wiki/Provinces_of_the_Philippines]
# ea: 87
# barang: 116 [over 42,000? https://en.wikipedia.org/wiki/Barangay]



# df  %>% group_by(shmunic) %>% summarise(num=n()) %>% arrange(num)