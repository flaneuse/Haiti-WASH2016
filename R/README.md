# README

## Haiti DHS Activity Design
Script to pull data from the Demographic and Health Surveys on variables related to water, sanitation, and hygiene (WASH)

Data are from the 2012 DHS, available at http://dhsprogram.com/what-we-do/survey/survey-display-368.cfm

Laura Hughes, lhughes@usaid.gov, 15 August 2016
with Patrick Gault (pgault@usaid.gov) and Tim Essam (tessam@usaid.gov)


Copyright 2016 by Laura Hughes via MIT License

## Order of files to be read:
For the scripts to be reproducible, they should be run in the following order. In certain cases, the inputs from previous files
are used in subsequent.  These should be noted at the top of each of the scripts.
* `HT_01_importDHS_geo.R`: imports geographic coordinates of clusters and admin units
* `HT_02_importDHS_hh.R`: imports household-level data
* `HT_03_importDHS_child.R`: imports individual children's data
* `HT_04_improvedToilets.R`: calculates percentage of improved toilets geographically across Haiti
* `HT_05_improvedWater.R`: calculates percentage of improved drinking water geographically across Haiti
* `HT_06_stunting.R`: calculates percentage of child stunting geographically across Haiti
* `HT_07_diarrhea.R`: calculates percentage of diarrheal prevalence in children geographically across Haiti

## Individual files
### `HT_01_importDHS_geo.R`
* loads in all necessary packages
* sets working directory for location of raw data
* imports geographically-offset household cluster locations
* imports shapefiles of administrative units of Haiti.


### `HT_02_importDHS_hh.R`
* imports household-level data from DHS using module HR
* pulls relevant variables for future analysis
* mildly cleans data (removing NAs, recoding variables based on codebook, converting real numbers by their scaling factors)
* merges hh data with geographic data
* sets up survey design to apply sampling weights

### `HT_03_importDHS_child.R`
* imports individual-level children's data from DHS using module KR
* pulls relevant variables for future analysis
* mildly cleans data (removing NAs, recoding variables based on codebook, converting real numbers by their scaling factors)
* merges hh data with geographic data

### `HT_04_improvedToilets.R`
* imports classification of toilets as being improved or unimproved
* 
