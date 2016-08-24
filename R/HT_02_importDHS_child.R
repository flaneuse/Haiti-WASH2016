# Haiti DHS Activity Design -----------------------------------------
# Script to pull data from the Demographic and Health Surveys on variables
# related to water, sanitation, and hygiene (WASH)
# 
# Data are from the 2012 DHS, available at http://dhsprogram.com/what-we-do/survey/survey-display-368.cfm
#
# Laura Hughes, lhughes@usaid.gov, 15 August 2016
# with Patrick Gault (pgault@usaid.gov) and Tim Essam (tessam@usaid.gov)
#
# File 02: pulling children's module
#
# Copyright 2016 by Laura Hughes via MIT License
#
# -------------------------------------------------------------------------

# -- Import children's data --
# NOTE: PR module should be used to reproduce DHS published stats for stunting, not KR (children's module).
child_raw = read_dta('~/Documents/USAID/Haiti/rawdata/Haiti_DHS2012/htpr61dt/HTPR61FL.DTA')

# Remove attribute labels.
child = removeAttributes(child_raw)
child_labels = pullAttributes(child_raw)