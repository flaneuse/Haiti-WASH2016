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


# Load libraries ----------------------------------------------------------
# requires dplyr > 0.5

# Load necessary packages
library(llamar)
library(haven)
library(dplyr)
library(leaflet)
library(ggplot2)
library(survey)

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



# Remove labels
hh_labels = pullAttributes(hh_raw)

hh = removeAttributes(hh_raw)




# Pull diarrhea, water-sanitation access, geo data ------------------------
hh = hh %>% 
  # -- Select relevant vars --
  select(hhid, 
         cluster_id = hv001,
         hh_num = hv002,
         sample_wt = hv005,
         prim_sampling_unit = hv021, # Usually same as cluster number and/or ultimate area unit, but may differ if multistage sampling used
         sample_strata = hv022, # Groupings of primary sampling units for clustering errors by region
         urban = hv025,
         region = shregnew, # Note: using shregnew, not hv024, since it breaks Ouest into Aire Metropolitaine et Reste-Ouest
         interview_month = hv006,
         hv201, # water and toilet vars
         hv204,
         hv205,
         hv225,
         hv238,
         # water_treatment = hv237, hv237a, hv237b, hv237c, hv237d, hv237e, hv237f, hv237x, hv237z, 
         wealth_idx = hv271) %>% # hv271 is the decimal value; hv270 is the quintile.  No breakdown by urban/rural
  # Create copies to avoid overriding data
  mutate(time2water = hv204,
         water_source = hv201,
         toilet_source = hv205,
         share_toilet = hv225,
         num_share_toilet = hv238) %>% 
  # -- Remove hh in camps --
  # based on the assumption that they will be different than the rest of the population.
  # 2012 survey included ~ 1330 hh living in post-earthquake camps.
  filter(region != 12) 


# Sample numbers ----------------------------------------------------------

# Aire metropolitaine: 1,761
# Autres villes: 2 462 
# Urban: 4 223
# Rural: 7 628
# Camps: 1 330 
# Total: 13 181

# recode data -------------------------------------------------------------
# - Translate codes to strings
# - Remove coded NA values
# - Divide numbers by their scaling factor, since DHS hates decimal points.

region_codes = data.frame(code = attr(hh_raw$shregnew, 'labels')) 
region_codes = region_codes %>% 
  mutate(region = row.names(region_codes))

hh = hh %>% 
  mutate(
    # -- divide sample weight by 1e6 -- ("Sample weight is an 8 digit variable with 6 implied decimal places" -- Recode5 manual)
    sample_wt = sample_wt / 1e6,
    
    # -- divide wealth index by 1e5 --  ("Wealth index factor score (5 decimals)")
    wealth_idx = wealth_idx / 1e5,
    
    # -- convert urban to binary --
    urban = ifelse(urban == 1, 1, 
                   ifelse(urban == 2, 0, NA)),
    
    # -- decode regional names --
    region_name = plyr::mapvalues(hh$region, from = region_codes$code, to = region_codes$region),
    
    # -- recode NA values -- (all codes from the Recode5 Map)
    water_source = na_if(water_source, 99),
    
    time2water = case_when(hh$time2water == 999 ~ NA_integer_, 
                           is.na(hh$time2water) ~ NA_integer_,
                           hh$time2water == 996 ~ as.integer(0), # 996 == "on premise"; redefining as 0 minutes.
                           TRUE ~ hh$time2water),
    
    toilet_source = na_if(toilet_source, 99),
    
    share_toilet = na_if(share_toilet, 9), # recoding both "missing" and "don't know" to be NA values.
    
    num_share_toilet = na_if(num_share_toilet, 98), # recoding both "missing" and "don't know" to be NA values.
    num_share_toilet = na_if(num_share_toilet, 99)
  )

# -- Checks that recoding doesn't change the # of values --
# hh %>% group_by(hv201) %>% summarise(n())
# hh %>% group_by(water_source) %>% summarise(n())
# 11   12   13   14   31   32   33   34   41   42   43   51   61   62   71   72   96   99 
# 136  358  502 2367   85  520   66  433  820 3594  183  220   95  195  572 1649   38   18

# table(hh$time2water)
# table(hh$hv204)
# qplot(data = hh, y = time2water, x = hv204) 

# hh %>% group_by(hv205, toilet_source) %>% summarise(n())

# hh %>% group_by(hv225, share_toilet) %>% summarise(n())

# hh %>% group_by(hv238, num_share_toilet) %>% summarise(n())


# classify improved/ not improved -----------------------------------------
# DHS claims to use WHO definitions for sanitation; similar to those provided by Dr. Elizabeth Jordan, 
# WHO/UNICEF Joint Monitoring Programme for Water Supply and Sanitation: http://www.wssinfo.org/definitions-methods/watsan-categories/

# NOTE! DHS classifies bottled water (eau en bouteille, petit vendeur d'eau) as being improved.
# HOWEVER-- both WHO (JMP) and USAID WASH experts default to assuming the bottled water is unimproved unless proven otherwise.
# If the bottled water is consumed out of preference and the household has access to an improved water source for cooking and 
# personal hygiene, the bottled water can be deemed "improved." In the absence of that issue, it's assumed to be unimproved.
# Unfortunately, the DHS didn't collect info on water sources for non-drinking water.
# Therefore, we are going AGAINST the DHS report and classifying bottled water as unimproved.


# -- WATER --
# Export type of water sources in survey.
water_types = attr(hh_raw$hv201, 'labels')
write.csv(water_types, '~/GitHub/Haiti-WASH2016/dataout/DHS_water_types.csv')

# read in Liz Jordan's classification of whether or not a toilet or water source is improved (see below in comments)
water_types = read.csv('~/GitHub/Haiti-WASH2016/dataout/DHS_water_classification.csv')
impr_water_codes = unlist(water_types %>% filter(improved == 1) %>% select(code))
unimpr_water_codes = unlist(water_types %>% filter(improved == 0) %>% select(code))

# Water sources are defined as being 'improved' if they are one of the following types and can be reached in <= 30 minutes
#                                           watersource code improved
#                                         piped water   10        1
#                                 piped into dwelling   11        1
#                                  piped to yard/plot   12        1
#                             piped from the neighbor   13        1
#                                public tap/standpipe   14        1
#                                     tube well water   20        1
#                               tube well or borehole   21        1
#                             protected well  to yard   31        1
#                               others protected well   32        1
#                                    protected spring   41        1
#                                           rainwater   51        1
#                           dug well (open/protected)   30        0
#                            unprotected well to yard   33        0
#                  public and others unprotected well   34        0
#                                       surface water   40        0
#                                  unprotected spring   42        0
# river/dam/lake/ponds/stream/canal/irrigation channel   43        0
#                                        tanker truck   61        0
#                                cart with small tank   62        0
#                                       bottled water   71        0
#                              sales company of water   72        0
#                                               other   96        0

hh = hh %>% 
  mutate(
    # -- straight classification of whether the source is improved --
    improved_water = ifelse(water_source %in% impr_water_codes, 1,
                            ifelse(water_source %in% unimpr_water_codes, 0, NA)),
    # -- improved source + <= 30 min. to acquire --
    # time2water = 996 == "on premises"; assumed to be < 30 min.
    impr_water_under30min = ifelse(is.na(time2water) | is.na(improved_water), NA,
                                   ifelse(time2water <= 30  & improved_water == 1, 1, 0))
  )

# -- TOILETS --
# export types of toilets in survey
toilet_types = attr(hh_raw$hv205, 'labels')
write.csv(toilet_types, '~/GitHub/Haiti-WASH2016/dataout/DHS_toilet_types.csv')

# read in Liz Jordan's classification of whether or not a toilet or water source is improved (see below in comments)
toilet_types = read.csv('~/GitHub/Haiti-WASH2016/dataout/DHS_toilet_classification.csv')
impr_toilet_codes = unlist(toilet_types %>% filter(improved == 'Improved') %>% select(code))
unimpr_toilet_codes = unlist(toilet_types %>% filter(improved == 'Unimproved') %>% select(code))
od_codes =  unlist(toilet_types %>% filter(improved == 'open defecation') %>% select(code))

# Toilets are defined as being 'improved' if they are one of the following types and aren't shared.
# Note: DHS defines mobile chemical toilets as improved (presumably b/c they treat the waste)
# Only 13 hh outside the camps use them.

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


# Checking that DHS variable "share_toilet" is correct binary for whether only one household uses toilet:
# hh %>% group_by(share_toilet, num_share_toilet) %>% summarise(n())

hh = hh %>% 
  mutate(
    # -- straight classification of whether the source is improved --
    toilet_type = case_when(hh$toilet_source %in% impr_toilet_codes ~ 'improved',
                            hh$toilet_source %in% unimpr_toilet_codes ~ 'unimproved',
                            hh$toilet_source %in% od_codes ~ 'open defecation',
                            TRUE ~ NA_character_))

hh = hh %>% 
  mutate(
    # -- improved source + unshared --
    improved_toilet = case_when(is.na(hh$toilet_type) ~ NA_real_,
                                (hh$toilet_type == 'improved' & hh$share_toilet == 0) ~ 1, # improved, unshared
                                (hh$toilet_type == 'improved' & hh$share_toilet == 1) ~ 0, # improved, shared
                                hh$toilet_type %in% c('unimproved', 'open defecation') ~ 0, # unimproved or open defecation
                                TRUE ~ NA_real_),
    impr_toilet_type = case_when(is.na(hh$toilet_type) ~ NA_character_,
                                 (hh$toilet_type == 'improved' & hh$share_toilet == 0) ~ 'improved-unshared', # improved, unshared
                                 (hh$toilet_type == 'improved' & hh$share_toilet == 1) ~ 'improved-shared', # improved, shared
                                 hh$toilet_type == 'unimproved' ~ 'unimproved', # unimproved
                                 hh$toilet_type == 'open defecation' ~ 'open defecation', # open defecation
                                 TRUE ~ NA_character_)
  )

# -- Quick summary tables --
hh %>% group_by(toilet_type, improved_toilet) %>% summarise(n = n()) %>% ungroup() %>% mutate(pct = percent(n/sum(n), ndigits = 1))

hh %>% group_by(impr_toilet_type) %>% summarise(n = n()) %>%  mutate(pct = percent(n/sum(n), ndigits = 1))

hh %>% group_by(region_name, improved_toilet) %>% summarise(n = n()) %>% ungroup() %>%  group_by(region_name) %>% mutate(pct = n/sum(n)) %>% filter(improved_toilet == 1) %>% ungroup() %>% arrange(desc(pct))



# Apply sampling weights --------------------------------------------------

DHSdesign = svydesign(id = ~prim_sampling_unit, strata = ~sample_strata, weights = ~sample_wt, data = hh)
summary(DHSdesign)

svymean(~improved_toilet, DHSdesign, na.rm = TRUE)
svymean(~improved_water, DHSdesign, na.rm = TRUE)

# clean and merge geodata -------------------------------------------------
# -- Import coordinates of clusters --
geo_raw = importShp(workingDir = '~/Documents/USAID/Haiti/rawdata/Haiti_DHS2012/htge61fl/',
                    layerName = 'HTGE61FL')

# -- Select data, convert urban/rural to binaries --
geo = geo_raw@data %>% 
  select(cluster_id = DHSCLUST, departement = ADM1NAME, ADM1FIPSNA, urban = URBAN_RURA, lat = LATNUM, lon = LONGNUM) %>% 
  mutate(urban = ifelse(urban == 'U', 1, 
                        ifelse(urban == 'R', 0, NA)),
         # Fix lat/lon that are in the middle of the Atlantic
         lat = ifelse(lat == 0, NA, lat),
         lon = ifelse(lon == 0, NA, lon)
  )

# -- merge in geocoordinates to hh --
hh = left_join(hh, geo, by = c("cluster_id", "urban"))

# quick map of cluster locations
leaflet(hh) %>% 
  addCircles(~lon, ~lat, radius = 1000) %>% 
  addProviderTiles("Thunderforest.Landscape")

