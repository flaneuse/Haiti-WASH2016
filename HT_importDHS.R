# Haiti DHS Activity Design -----------------------------------------
# Script to pull data from the Demographic and Health Surveys on variables
# related to water, sanitation, and hygiene (WASH)
# 
# Data are from the 2012 DHS, available at http://dhsprogram.com/what-we-do/survey/survey-display-368.cfm
#
# Laura Hughes, lhughes@usaid.gov, 15 August 2016
# With Patrick Gault (pgault@usaid.gov) and Tim Essam (tessam@usaid.gov)
#
# Copyright 2016 by Laura Hughes via MIT License
#
# -------------------------------------------------------------------------


# Helper functions --------------------------------------------------------

# Function to pull attribute (label, labels) data from Stata format
pullAttributes <- function(data) {
  
  metadata = lapply(data, function(x) attr(x, 'label'))
  metadata = data.frame(metadata)
  
  labels = lapply(data, function(x) attr(x, 'labels'))
  
  metadata = data.frame(var = colnames(metadata), varDescrip = t(metadata))
  
  df = mutate(metadata, varValues = labels)
  return(df)
}

# Function to import shapefiles
importShp = function(workingDir = getwd(),
                     layerName) {
  library(rgdal)
  # Check that the layerName doesn't contain any extensions
  # Check that layerName exists within the wd
  
  # Log the current working directory, to change back at the end.
  currentDir = getwd()
  
  # Change directory to the file folder containing the shape file
  setwd(workingDir)
  
  # the dsn argument of '.' says to look for the layer in the current directory.
  rawShp = rgdal::readOGR(dsn = ".", layer = layerName)
}


# Import data -------------------------------------------------------------

# Load necessary packages
library(haven)
library(dplyr)



# Read in the base module and the children's

#' DHS modules are as follows:
#' (from http://dhsprogram.com/data/Dataset-Types.cfm)
#' * HR: household data (household level description)
#' * PR: household list (individual roster)
#' * IR: women's module (one woman/hh)
#' * MR: men's module (one man/hh)
#' * CR: couple's module
#' * KR: children's module (ea. child < 5 y old)
#' * BR: birth data (birth history of all children) --> fertility / mortality rates
#' * WI: wealth index data (for data before 1990 to calcualte wealth index)

# -- Import household data --
hh_raw = read_dta('~/Documents/USAID/Haiti/rawdata/Haiti_DHS2012/hthr61dt/HTHR61FL.DTA')

# -- Import children's data --
# NOTE: PR module should be used to reproduce DHS published stats for stunting, not KR (children's module).
child_raw = read_dta('~/Documents/USAID/Haiti/rawdata/Haiti_DHS2012/htpr61dt/HTPR61FL.DTA')

# -- Import coordinates of clusters --
geo_raw = importShp(workingDir = '~/Documents/USAID/Haiti/rawdata/Haiti_DHS2012/htge61fl/',
                layerName = 'HTGE61FL')

# Remove labels
hh_labels = pullAttributes(hh_raw)
child_labels = pullAttributes(child_raw)

hh = removeAttributes(hh_raw)
child = removeAttributes(child_raw)




# Pull diarrhea, water-sanitation access, geo data ------------------------
hh = hh %>% 
  select(hhid, 
         cluster_id = hv001,
         hh_num = hv002,
         sample_wt = hv005,
         urban = hv025,
         region = hv024,
         interview_month = hv006,
         water_source = hv201, 
         time2water = hv204, 
         toilet_source = hv205,
         share_toilet = hv225,
         num_share_toilet = hv238,
         # water_treatment = hv237, hv237a, hv237b, hv237c, hv237d, hv237e, hv237f, hv237x, hv237z, 
         wealth_idx = hv270) %>% 
  
  filter(region != 11) # Remove those households that were in camps, based on the assumption that they will be different than the rest of the population.


# classify improved/ not improved -----------------------------------------
# Toilets are defined as being 'improved' if they are one of the following types and aren't shared.
#                           toilet_type code        improved
#                         flush toilet   10        Improved
#          flush to piped sewer system   11        Improved
#                 flush to septic tank   12        Improved
#                 flush to pit latrine   13        Improved
#              flush to somewhere else   14        Improved
#              flush, don't know where   15        Improved
#                   pit toilet latrine   20        Improved
# ventilated improved pit latrine (vip)   21        Improved
#                pit latrine with slab   22        Improved
#                    composting toilet   41        Improved
#                          no facility   30 open defecation
#               no facility/bush/field   31 open defecation
#    pit latrine without slab/open pit   23      Unimproved
#                        bucket toilet   42      Unimproved
#               hanging toilet/latrine   43      Unimproved
#           toilet hanging (on stilts)   44      Unimproved
#               mobile chemical toilet   45      Unimproved
#                                other   96      Unimproved

# Water sources are defined as being 'improved' if they are one of the following types and can be reached in <= 30 minutes

# export types of toilets in survey
toilet_types = attr(hh_raw$hv205, 'labels')
write.csv(toilet_types, '~/GitHub/Haiti-WASH2016/dataout/DHS_toilet_types.csv')

water_types = attr(hh_raw$hv201, 'labels')
write.csv(water_types, '~/GitHub/Haiti-WASH2016/dataout/DHS_water_types.csv')

# read in Liz Jordan's classification of whether or not a toilet or water source is improved.
toilet_types = read.csv('~/GitHub/Haiti-WASH2016/dataout/DHS_toilet_classification.csv')


# clean and merge geodata -------------------------------------------------
geo = geo_raw@data %>% 
  select(cluster_id = DHSCLUST, departement = ADM1NAME, ADM1FIPSNA, urban = URBAN_RURA, lat = LATNUM, lon = LONGNUM) %>% 
  mutate(urban = ifelse(urban == 'U', 1, 
                        ifelse(urban == 'R', 0, NA)))

library(leaflet)

# quick map of cluster locations
leaflet(geo) %>% 
  addCircles(~lon, ~lat, radius = 1000) %>% 
  addProviderTiles("Thunderforest.Landscape")

