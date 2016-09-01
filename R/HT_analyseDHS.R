# Haiti DHS Activity Design -----------------------------------------
#
# HT_R: wrapper function that calls all lower-level functions
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


# Load necessary packages and functions -----------------------------------
source('~/GitHub/Haiti-WASH2016/R/HT_00_setupFncns.R')

# Get raw data ------------------------------------------------------------

# -- Geographic data --
source('~/GitHub/Haiti-WASH2016/R/HT_01_importDHS_geo.R')

# -- Household-level DHS data --
source('~/GitHub/Haiti-WASH2016/R/HT_02_importDHS_hh.R')


# Analyse DHS data --------------------------------------------------------

# -- Calculation of Improved Latrine percentages --
source('~/GitHub/Haiti-WASH2016/R/HT_04_improvedToilets.R')


# -- Calculation of Improved Water percentages --
source('~/GitHub/Haiti-WASH2016/R/HT_05_improvedWater.R')
