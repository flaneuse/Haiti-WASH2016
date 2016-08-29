# Haiti DHS Activity Design -----------------------------------------
#
# HT_08_cholera.R: Plot hot spots of cholera activity
#
# Script to pull data from the Demographic and Health Surveys on variables
# related to water, sanitation, and hygiene (WASH)
# 
# Data are from UNICEF's classification of prevalence of cholera
#
# Laura Hughes, lhughes@usaid.gov, 15 August 2016
# With Patrick Gault (pgault@usaid.gov) and Tim Essam (tessam@usaid.gov)
#
# Copyright 2016 by Laura Hughes via MIT License
#
# -------------------------------------------------------------------------

# Previous dependencies ---------------------------------------------------
# `HT_01_importDHS_geo.R` are meant to be run first.  The following are dependencies in thoses files:

# * admin0-2, etc.: shapefiles containing geographic polygons of Haiti + basemaps
# * libraries


# Set colors and other basics ---------------------------------------------
colour_toilet = 'YlGn'


# Import data -------------------------------------------------------------

# List of priority communes for cholera interventions, as identified by UNICEF
cholera = read_excel('~/Documents/USAID/Haiti/rawdata/Haiti_cholera_UNICEF/Cholera Response Commune and Total Sanitation Campaign August 2016.xlsx', skip = 1)


# List of cholera hotspot cities, where infections tend to occur.
chol_cities = read_csv('~/Documents/USAID/Haiti/rawdata/Haiti_cholera_UNICEF/Haiti_cholera_hotspotcities_UNICEF_2016-08-29.csv', col_names = FALSE)

chol_cities = data.frame(city = t(chol_cities))


# Clean data --------------------------------------------------------------

cholera = cholera %>% 
  # Remove blank / rows or ones w/ coded data.
  filter(!is.na(Category)) %>% 
# Encode Category based on   
  mutate(prevalence = factor(Category, 
                             levels = c('C', 'B' ,'A'),
                             labels = c('third priority', 'second priority', 'first priority')))


# Merge commune-level data with maps --------------------------------------
admin3

