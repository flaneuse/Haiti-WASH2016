# Haiti DHS Activity Design -----------------------------------------
#
# HT_00_setupFncns.R: load packages and functions for analysis.
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

# -- Set aesthetics --
font_family = 'Lato'
font_weight = 'bold'


# Load libraries ----------------------------------------------------------


# -- Load necessary libraries --
# requires dplyr > 0.5

library(rgdal)
library(maptools)
library(leaflet)
library(ggmap)
library(survey)
library(llamar)
library(haven)
library(stringr)
library(broom)
library(ggplot2)
library(RColorBrewer)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(extrafont)
loadfonts()


# Import data (maps) -------------------------------------------------------------

# -- Function to import shapefiles --
read_shp = function(workingDir = getwd(),
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


# -- Function to import shapefiles and convert to a ggplot-able object --
shp2df = function(workingDir = getwd(),
                  layerName,
                  exportData = TRUE,
                  fileName = layerName,
                  getCentroids = TRUE,
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
  
  if (getCentroids == TRUE){
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
  } else {
    # if the 'exportData' option is selected, save the lat/lon coordinates as a .csv
    if (exportData == TRUE) {
      write.csv(df, paste0(workingDir, "/", fileName, ".csv"))
    }
    
    
    # Return the dataframe containing the coordinates and the centroids
    return(df)
  }
}


# Clean data --------------------------------------------------------------

# -- Function to pull attribute (label, labels) data from Stata format --
pullAttributes <- function(data) {
  
  metadata = lapply(data, function(x) attr(x, 'label'))
  metadata = data.frame(metadata)
  
  labels = lapply(data, function(x) attr(x, 'labels'))
  
  metadata = data.frame(var = colnames(metadata), varDescrip = t(metadata))
  
  df = mutate(metadata, varValues = labels)
  return(df)
}

# -- Remove attribute information from object --
removeAttributes <- function(data) {
  data <- lapply(data, function(x) {
    attr(x, "labels") <- NULL
    x
  })
  data <- lapply(data, function(x) {
    attr(x, "label") <- NULL
    x
  })
  data <- lapply(data, function(x) {
    attr(x, "class") <- NULL
    x
  })
  data <- lapply(data, function(x) {
    attr(x, "levels") <- NULL
    x
  })
  data = data.frame(data)
}


# Calculate Variables -----------------------------------------------------

# -- Wrapper to survey::svyby --
# Calculates point estimates with standard errors, weighted by sample weights
calcPtEst = function(var, # What you want to average
                     by_var, # variable 
                     design, # survey design object (containing sampling procedure)
                     df, # main data frame containing raw data
                     omit_NA = TRUE,
                     ci_factor = 2) {
  
  # Calculate point estimate and standard error.
  pt_est = svyby(as.formula(paste0('~', var)), 
                 by = as.formula(paste0('~', by_var)),
                 design = design,
                 svymean,
                 na.rm = omit_NA)
  # Convert to data frame, if not already
  pt_est = as.data.frame(pt_est)
  
  # Calculate CI and upper and lower bounds.
  # Default is to use 95% CI (1.96)
  pt_est = pt_est %>%
    mutate_(.dots = setNames(var, 'avg')) %>% # Create a copy of the average value named 'avg'
    mutate(ci = se * ci_factor,
           ub = avg + ci,
           lb = avg - ci) %>% 
    arrange(desc(avg))
  
  # Calculate sample size and unweighted avg.
  if(omit_NA == TRUE) {
    # Exclude missing values
    n = hh %>% 
      filter_(paste0('!is.na(', var,')')) %>% 
      group_by_(by_var) %>% 
      summarise_(.dots = list(N = 'n()', 
                              unweighted_avg = paste0('mean(', var, ')')))
  } else{
    n = hh %>% 
      group_by_(by_var) %>% 
      summarise_(.dots = list(N = 'n()', 
                              unweighted_avg = paste0('mean(', var, ')')))
  }
  
  # Merge the two together
  pt_est = full_join(pt_est, n)
  
  return(pt_est)
}

# Plot Maps --------------------------------------------------------------------
plotMap = function(df, 
                   admin0, # base map
                   clipping_mask = admin0, # clipping mask (us. country)
                   lakes = NA, # inland water
                   bounding_x = c(-74.48040, -71.62215 * 0.988), 
                   bounding_y = c(17.66170, 20.49101),
                   fill_var = 'id',
                   exportPlot = FALSE, 
                   fileName = 'map.pdf', 
                   stroke_width = 0.075,
                   stroke_colour = grey75K,
                   fill_scale = NA,
                   fill_limits = NA,
                   bg_fill = '#f6f8fb',#d3dceb', # water #ebf0f9
                   lakes_fill = '#0067b9', # inland water
                   base_fill = grey15K, # underlying country
                   title = NA,
                   plotWidth = 6, plotHeight = 6) {
  
  p = ggplot(df, aes(x = long, y = lat, group = group)) + 
    
    # -- base fill the country --
    geom_polygon(fill = base_fill, data = admin0) +
    geom_path(colour = stroke_colour, size = stroke_width*3,
              data = admin0) +
    
    # -- choropleth over regions --
    geom_polygon(aes_string(fill = fill_var)) +
    geom_path(colour = stroke_colour, size = stroke_width) +
    
    # -- Admin 0 outline (for clipping if needed) --
    geom_path(colour = stroke_colour, size = stroke_width*3,
              data = clipping_mask) +
    
    coord_equal() +
    
    # -- themes --
    theme_void() + 
    theme(rect = element_rect(fill = '#ffffff', colour = '#ffffff', size = 0, linetype = 1),
          panel.background = element_rect(fill = bg_fill))
  
  
  # -- add lakes and inland water --
  if(!is.na(lakes)) {
    p = p +
      geom_polygon(fill = lakes_fill, data = lakes) 
  }
  
  # -- add title --
  if(!is.na(title)){
    p = p +
      ggtitle(title)
  }
  
  # -- scale color by fill_scale to create choropleth --
  if(!is.na(fill_scale)) {
    if(is.na(fill_limits)) {
      fill_limits = c(0, 1)
    }
    p = p +  
      scale_fill_gradientn(colours = brewer.pal(9, fill_scale), 
                           limits = fill_limits)    
  }
  
  # -- resize by bounding_box --
  if(!is.na(bounding_x) & !is.na(bounding_y)) {
    p = p +
      coord_equal(xlim = bounding_x, ylim = bounding_y)
  }
  
  # -- export ==
  if (exportPlot == TRUE) {
    ggsave(filename = fileName, 
           width = plotWidth, height = plotHeight, units = "in",
           bg = "transparent", 
           paper = "special", useDingbats = FALSE, compress = FALSE, dpi = 300)
  }
  
  return(p)
  
}




# Dot plot ----------------------------------------------------------------

pairGrid = function (df,
                     savePlots = TRUE,
                     width_plot = 6, height_plot = 6
                     ) {
  
  
  
  # -- Save the main plot --
  
  if (savePlots){
    ggsave(fileMain, 
           plot = mainPlot,
           width = width_plot, height = height_plot,
           bg = 'transparent',
           paper = 'special',
           units = 'in',
           useDingbats=FALSE,
           compress = FALSE,
           dpi = 300)
  }
  
  
}



