# Haiti DHS Activity Design -----------------------------------------
# Script to pull data from the Demographic and Health Surveys on variables
# related to water, sanitation, and hygiene (WASH)
# 
# Data are from the 2012 DHS, available at http://dhsprogram.com/what-we-do/survey/survey-display-368.cfm
#
# Laura Hughes, lhughes@usaid.gov, 15 August 2016
# with Patrick Gault (pgault@usaid.gov) and Tim Essam (tessam@usaid.gov)
#
# Copyright 2016 by Laura Hughes via MIT License
#
# -------------------------------------------------------------------------


# Previous dependencies ---------------------------------------------------
# `HT_01_importDHS_geo.R` is meant to be run first.  The following are dependencies in that file:

# * local_wd: string containing the location where the raw DHS data are saved locally
# * geo: dataframe with the geographic coordinates of the clusters
# * all necessary libraries



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

# Remove attribute information from object
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
hh_raw = read_dta(paste0(local_wd, 'Haiti_DHS2012/hthr61dt/HTHR61FL.DTA'))


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
         sample_strata22 = hv022, # Groupings of primary sampling units for clustering errors by region
         sample_strata23 = hv023, # Groupings of primary sampling units for clustering errors by region
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

# NOTE: DHS treats NA values (missing, no response, didn't know, etc.) as a third category in most cases.
# In this analysis, all those values are converted to NA and *removed* from the analysis.
# Since you have no information on whether the variable should be one thing or another, I believe they
# should be ignored to provide a more realistic estimate of the value, based on the available data.
# Numbers may therefore be slightly different than the DHS-published numbers.

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



# Set up sampling weights --------------------------------------------------

DHSdesign = svydesign(id = ~prim_sampling_unit, strata = ~sample_strata22, weights = ~sample_wt, data = hh)
summary(DHSdesign)


# Merge in geocoordinates -----------------------------------------------------
# Object geo is defined in file HT_01_importDHS_geo.R, which pulls in the offset
# lat/lon coordinates of the survey clusters.

# -- merge in geocoordinates to hh --
hh = left_join(hh, geo, by = c("cluster_id", "urban"))