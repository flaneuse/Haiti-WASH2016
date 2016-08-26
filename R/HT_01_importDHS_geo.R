# Haiti DHS Activity Design -----------------------------------------
#
# HT_01_importDHS_geo.R: pull the geographic data
#
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

# Function to clean geodata from 2012 Haiti DHS


# Function setup ----------------------------------------------------------
# -- Set working directory where the raw DHS data are located --
# ! Note: should be changed to local location
# Used in files 01 (geo), 02 (hh), and 03 (individual children's data)
local_wd = '~/Documents/USAID/Haiti/rawdata/'

# -- Load necessary libraries --
# requires dplyr > 0.5

library(rgdal)
library(leaflet)
library(survey)
library(llamar)
library(haven)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)


# -- Function to import shapefiles --
importShp = function(workingDir = getwd(),
                     layerName) {
  # Check that the layerName doesn't contain any extensions
  # Check that layerName exists within the wd
  
  # Log the current working directory, to change back at the end.
  currentDir = getwd()
  
  # Change directory to the file folder containing the shape file
  setwd(workingDir)
  
  # the dsn argument of '.' says to look for the layer in the current directory.
  rawShp = rgdal::readOGR(dsn = ".", layer = layerName)
}


# Import coordinates of clusters ------------------------------------------
# NOTE!: There are 8 clusters within the DHS file which have NULL coordinates.
# These correspond to ~ 238 hh in the data set (excluding camps.)
# For the purposes of calculating averages at the departement level (Admin1),
# these households are *included* since they have 

# Raw data directly from DHS. Replaced by spatially joined data (below)
# geo_raw = importShp(workingDir = paste0(local_wd, 'Haiti_DHS2012/htge61fl/'),
                    # layerName = 'HTGE61FL')
# geo = geo_raw@data

# Raw DHS data was spatially joined with Haiti Admin2 and Admin3 boundaries in Esri. 
# Admin boundaries were provided by Joel Barthelemy, GIS specialist in Haiti
geo_raw = read_csv(paste0(local_wd, 'htGEO/2013_HTI_DHS_ClusterGEO.csv'))
geo = geo_raw


# Clean geodata -----------------------------------------------------------

# -- Select data, convert urban/rural to binaries --
geo = geo %>% 
  select(cluster_id = DHSCLUST, 
         admin1 = ADM1NAME, A1_PCode, # departement
         urban = URBAN_RURA, 
         lat = LATNUM, lon = LONGNUM,
         admin2 = A2_Name, A2_PCode, # arrondisment
         admin3 = A3_Name, A3_PCode # commune
         ) %>% 
  mutate(urban = ifelse(urban == 'U', 1, 
                        ifelse(urban == 'R', 0, NA)),
         # Fix lat/lon that are in the middle of the Atlantic
         lat = ifelse(lat == 0, NA, lat),
         lon = ifelse(lon == 0, NA, lon)
  )


# quick map ---------------------------------------------------------------

# quick map of cluster locations

paired = colorFactor("Paired", domain = NULL)

leaflet(geo) %>% 
  addCircles(~lon, ~lat, radius = 1000,
             color = ~paired(admin1)) %>% 
  addProviderTiles("Thunderforest.Landscape")

