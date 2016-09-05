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
font_light = 'Lato Light'
font_normal = 'Lato'


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
library(forcats)
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
                   clipping_mask, # clipping mask (us. country)
                   centroids = NA,
                   centroids_var = 'region_name',
                   lakes = NA, # inland water
                   bounding_x = c(-74.555, -71.128),
                   bounding_y = c(17.431, 20.295),
                   # bounding_x = c(-74.561420581, -71.130578441), 
                   # bounding_y = c(17.47410627, 20.346439224),
                   plot_base = TRUE,
                   fill_var = 'id',
                   exportPlot = FALSE, 
                   fileName = 'map.pdf', 
                   stroke_width = 0.075,
                   stroke_colour = grey75K,
                   size_label = 4,
                   label_y = 0.05,
                   fill_scale = NA,
                   fill_limits = NA,
                   bg_fill = '#f6f8fb',#d3dceb', # water #ebf0f9
                   lakes_fill = '#0067b9', # inland water
                   base_fill = grey15K, # underlying country
                   font_normal = 'Lato',
                   font_light = 'Lato Light',
                   # alpha = 0.7, 
                   title = NA,
                   plotWidth = 10.75, plotHeight = 9) {
  
  if(plot_base == TRUE){
    p = ggplot(df, aes(x = long, y = lat, group = group)) + 
      
      # -- base fill the country --
      geom_polygon(fill = base_fill, data = admin0) +
      geom_path(colour = stroke_colour, size = stroke_width*3,
                data = admin0) +
      # -- themes --
      theme_void() + 
      theme(rect = element_rect(fill = '#ffffff', colour = '#ffffff', size = 0, linetype = 1),
            legend.position = c(0.2, 0.7),
            panel.background = element_rect(fill = bg_fill))
    
  } else {
    p = ggplot(df, aes(x = long, y = lat, group = group)) +
      # -- themes --
      theme_void() + 
      theme(legend.position = c(0.2, 0.7))
  } 
  
  # -- choropleth over regions --
  p = p + geom_polygon(aes_string(fill = fill_var)) +
    geom_path(colour = stroke_colour, size = stroke_width) +
    
    # -- Admin 0 outline (for clipping if needed) --
    geom_path(colour = stroke_colour, size = stroke_width*3,
              data = clipping_mask) +
    
    coord_equal() +
    
    
    
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
  
  # -- Add labels at centroids --
  if(!is.na(centroids)) {
    # define color based on value
    # colours = df %>% 
    # mutate(case_when(df[[fill_var]] > mean(df[[fill_var]]) ~ '#ffffff',
    # TRUE ~ 'black'))
    
    df_avg =  df %>% 
      group_by_(centroids_var) %>% 
      summarise_(var_pct = paste0('llamar::percent(mean(', fill_var,'))'))
    
    centroids = left_join(centroids, df_avg, by = c('label' = centroids_var))
    
    p = p +
      geom_text(aes(label = label, x = long, y = lat, group = 1),
                size = size_label,
                colour = brewer.pal(9, fill_scale)[9],
                family = font_normal,
                data = centroids) +
      geom_text(aes(label = var_pct, x = long, y = lat, group = 1),
                size = size_label,
                colour = brewer.pal(9, fill_scale)[9],
                family = font_light,
                nudge_y = -label_y,
                data = centroids)
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
           scale = (8.9555/8.1219),
           bg = "transparent", 
           paper = "special", useDingbats = FALSE, compress = FALSE, dpi = 300)
  }
  
  return(p)
  
}




# Dot plot ----------------------------------------------------------------

pairGrid = function (df,
                     # Variable names within df
                     y_var = 'region_name', # string for the y variable
                     avg_var = 'avg', # string for average point
                     lb_var = 'lb', # string for lower bound of CI
                     ub_var = 'ub', # string for upper bound of CI
                     # Percent labels
                     sizePct = 3,
                     label_offset = 0.06,
                     # Average point
                     sizeDot = 2.5,
                     stroke_colour = grey90K,
                     colorDot = 'YlGnBu',
                     fill_limits = c(0,1),
                     # S.E. bars
                     colorSE = grey10K,
                     alphaSE = 1,
                     size_SE = 1,
                     # Comparison for average
                     incl_comparison = TRUE,
                     comp_avg = 'admin1_avg',
                     comp_lb = 'admin1_lb',
                     comp_ub = 'admin1_ub',
                     alpha_comp = 0.3,
                     fill_comp = grey30K,
                     colour_comp = grey70K,
                     stroke_comp = 0.25,
                     # Facet var
                     facet_var = 'admin1',
                     n_col = 1,
                     # Title
                     title = NA,
                     # Save files
                     savePlots = TRUE,
                     file_name = 'plot.pdf',
                     width_plot = 5, height_plot = 10
) {
  
  # -- Reorder the dots --
  df = df %>% 
    # Remove any NA values
    ungroup() %>% 
    filter_(paste0('!is.na(', y_var, ')')) %>% 
    arrange_(paste0(avg_var))
  
  df[[y_var]] = factor(df[[y_var]], levels = df[[y_var]])
  
  # Set limits for the comparison values
  y_min = 0
  y_max = 8
  
  
  # -- General plot --
  p = ggplot() +
    # Error bars
    # Can't use geom_pointrange b/c want to independently control colors of fills.
    geom_segment(aes_string(x = lb_var, xend = ub_var, y = y_var, yend = y_var),
                 data = df,
                 colour = colorSE, size = size_SE) +
    
    # Average point
    geom_point(aes_string(x = avg_var, y = y_var, 
                          fill = avg_var),
               data = df,
               size = sizeDot,
               shape = 21,
               colour = stroke_colour) +
    geom_text(aes_string(x = avg_var, y = y_var,
                         label = paste0('llamar::percent(', avg_var,')'), colour = avg_var),
              size = sizePct,
              data = df,
              nudge_x = label_offset) +
    # Color dots
    scale_fill_gradientn(colours = brewer.pal(9, colorDot),
                         limits = fill_limits) +
    scale_colour_gradientn(colours = brewer.pal(9, colorDot),
                           limits = fill_limits) +
    # Convert to percentages
    scale_x_continuous(labels = scales::percent) +
    # coord_flip() +
    theme_xgrid() +
    theme(axis.title.x = element_blank(),
          axis.text.y = element_text(size = 8), 
          strip.text = element_text(size = 9, family = font_normal, colour = grey75K))
  
  # -- Facet if indicated --
  if(!is.na(facet_var)) {
    p = p + 
      facet_wrap(as.formula(paste0('~', facet_var)), scales ='free_y', ncol = n_col)
  }
  
  # -- Draw the underlying comparison --
  # Note: needs to be sent to the back in AI after making
  if(incl_comparison == TRUE){
    p = p +
      geom_rect(aes_string(xmin = comp_lb, xmax = comp_ub, ymin = y_min, ymax = y_max),
                fill = fill_comp,
                alpha = alpha_comp, 
                # Plots only once so don't have too many objects
                data = df %>% group_by_(facet_var, comp_lb, comp_ub, comp_avg) %>% summarise(n())) +
      geom_segment(aes_string(x = comp_avg, xend = comp_avg, y = y_min, yend = y_max),
                   colour = colour_comp,
                   size = stroke_comp,
                   data = df %>% group_by_(facet_var, comp_lb, comp_ub, comp_avg) %>% summarise(n()))
  } 
  
  # -- Add title --
  if(!is.na(title)) {
    p = p +
      ggtitle(title)
  }
  
  # -- Save the main plot --
  if (savePlots){
    ggsave(file_name, 
           plot = p,
           width = width_plot, height = height_plot,
           bg = 'transparent',
           paper = 'special',
           units = 'in',
           useDingbats=FALSE,
           compress = FALSE,
           dpi = 300)
  }
  
  return(p)
  
}




# Urban/rural pair grid dot plot ------------------------------------------

ur_pairGrid = function(df, 
                       fill_scale = 'YlGnBu',
                       fill_limits){
  
  ggplot(df, aes(x = avg, y = region,
                 fill = avg, shape = urban)) +
    geom_segment(aes(x = lb, xend = ub, y = region, yend = region), alpha = 0.2) +
    geom_point(size = 4, colour = grey90K) +
    scale_shape_manual(values = c('urban' = 22, 'rural' = 21)) +
    scale_fill_gradientn(colours = brewer.pal(9, fill_scale), 
                         limits = fill_limits) + 
    theme_xgrid()
}

