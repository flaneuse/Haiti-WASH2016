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

# Function to clean geodata from 2012 Haiti DHS and to import basic shapefiles of Haiti administrative units


# Function setup ----------------------------------------------------------
# -- Set working directory where the raw DHS data are located --
# ! Note: should be changed to local location
# Used in files 01 (geo), 02 (hh), and 03 (individual children's data)
local_wd = '~/Documents/USAID/Haiti/rawdata/'

# -- Load necessary libraries --
# requires dplyr > 0.5

library(rgdal)
library(maptools)
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

shp2df = function(workingDir = getwd(),
                     layerName,
                     exportData = TRUE,
                     fileName = layerName,
                     labelVar = NA,
                     reproject = TRUE, projection = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") {
  
  # Check that the layerName doesn't contain any extensions
  # Check that layerName exists within the wd
  
  # Log the current working directory, to change back at the end.
  currentDir = getwd()
  
  # Change directory to the file folder containing the shape file
  setwd(workingDir)
  
  # the dsn argument of '.' says to look for the layer in the current directory.
  rawShp = rgdal::readOGR(dsn = ".", layer = layerName)
  
  if (reproject == TRUE) {
    # reproject the data
    projectedShp = spTransform(rawShp, CRS(projection))
  } else {
    projectedShp = rawShp
  }
  # pull out the row names from the data and save it as a new column called 'id'
  projectedShp@data$id = rownames(projectedShp@data)
  
  # Convert the shape polygons into a series of lat/lon coordinates.
  poly_points = ggplot2::fortify(projectedShp, region = "id")
  
  # Merge the polygon lat/lon points with the original data
  df = dplyr::left_join(poly_points, projectedShp@data, by = "id")
  
  
  # Pull out the centroids and the associated names.
  centroids = data.frame(coordinates(projectedShp)) %>% rename(long = X1, lat = X2)
  
  if (!is.na(labelVar)) {
    if (labelVar %in% colnames(projectedShp@data)) {
      # Merge the names with the centroids
      centroids = cbind(centroids, projectedShp@data[labelVar]) %>% rename_(label = labelVar)  # rename the column
    } else {
      warning("label variable for the centroids is not in the raw shapefile")
    }
  }
  
  # if the 'exportData' option is selected, save the lat/lon coordinates as a .csv
  if (exportData == TRUE) {
    write.csv(df, paste0(workingDir, "/", fileName, ".csv"))
    write.csv(centroids, paste0(workingDir, "/", fileName, "_centroids.csv"))
  }
  
  
  # Return the dataframe containing the coordinates and the centroids
  return(list(df = df, centroids = centroids))
}

# -- Function to import shapefiles and convert to a ggplot-able object --


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
         urban = URBAN_RURA, 
         lat = LATNUM, lon = LONGNUM,
         admin1 = ADM1NAME, A1_PCode, # departement
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




# Import Admin shapefiles -------------------------------------------------
admin1 = shp2df(workingDir = paste0(local_wd, 'Haiti_AdminBndry/'),
                    layerName = 'hti_polbnda_adm1_cnigs',
                    labelVar = 'A1_Name')

admin2 = shp2df(workingDir = paste0(local_wd, 'Haiti_AdminBndry/'),
                layerName = 'hti_polbnda_adm2_cnigs',
                labelVar = 'A2_Name')
