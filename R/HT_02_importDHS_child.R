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


# Import data -------------------------------------------------------------

# -- Import children's data --
# NOTE: PR module should be used to reproduce DHS published stats for stunting, not KR (children's module).
child_raw = read_dta('~/Documents/USAID/Haiti/rawdata/Haiti_DHS2012/htkr61dt/HTKR61FL.DTA')
# Diarrhea is asked in women's module, not personal records.

# Remove attribute labels.
child = removeAttributes(child_raw)
child_labels = pullAttributes(child_raw)


# Select important data ---------------------------------------------------

ch = child %>% 
  # -- Select relevant vars --
  select(caseid, 
         cluster_id = v001,
         hh_num = v002,
         sample_wt = v005,
         prim_sampling_unit = v021, # Usually same as cluster number and/or ultimate area unit, but may differ if multistage sampling used
         sample_strata = v022, # Groupings of primary sampling units for clustering errors by region
         urban = v025,
         region = sregnew, # Note: using shregnew, not hv024, since it breaks Ouest into Aire Metropolitaine et Reste-Ouest
         interview_month = v006,
         wealth_idx = v191, # hv271 is the decimal value; hv270 is the quintile.  No breakdown by urban/rural
         # age_months = hc1, # in months, PR module
         age_months = hw1,
         age_yrs = b8,
         b4,
         # sex = hc27, # PR module
         # eligible = hv120, #PR module
         diarrhea = h11,
         hw5,
         hw13 # result of height/weight measurement (whether or not measured)
         
  ) %>% 
  # -- Select only children under 5 --
  filter(age_yrs < 5) %>% # Removes NA for ages
  mutate(sex = b4,
         stunting_eligible = hw13 == 0,
         stunting_score = hw5) # whether the child's height/weight was measured; 0 == measured.

# Sample sizes ------------------------------------------------------------

# recode data -------------------------------------------------------------
# - Translate codes to strings
# - Remove coded NA values
# - Divide numbers by their scaling factor, since DHS hates decimal points.

region_codes = data.frame(code = attr(child_raw$sregnew, 'labels')) 
region_codes = region_codes %>% 
  mutate(region = row.names(region_codes))

ch = ch %>% 
  mutate(
    # -- divide sample weight by 1e6 -- ("Sample weight is an 8 digit variable with 6 implied decimal places" -- Recode5 manual)
    sample_wt = sample_wt / 1e6,
    
    # -- divide wealth index by 1e5 --  ("Wealth index factor score (5 decimals)")
    wealth_idx = wealth_idx / 1e5,
    
    # -- convert urban to binary --
    urban = ifelse(urban == 1, 1, 
                   ifelse(urban == 2, 0, NA)),
    
    # -- decode regional names --
    region_name = plyr::mapvalues(ch$region, from = region_codes$code, to = region_codes$region),
    
    # -- decode sex --
    sex = case_when(ch$sex == 1 ~ 'male',
                    ch$sex == 2 ~ 'female',
                    TRUE ~ NA_character_)
    
    # -- recode NA values -- (all codes from the Recode5 Map)
    
  )