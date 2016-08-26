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

# Previous dependencies ---------------------------------------------------
# `HT_01_importDHS_geo.R` is meant to be run first.  The following are dependencies in that file:

# * local_wd: string containing the location where the raw DHS data are saved locally
# * geo: dataframe with the geographic coordinates of the clusters
# * all necessary libraries


# Import data -------------------------------------------------------------

# -- Import children's data --
# NOTE: PR module should be used to reproduce DHS published stats for stunting, not KR (children's module).
child_raw = read_dta('~/Documents/USAID/Haiti/rawdata/Haiti_DHS2012/htpr61dt/HTPR61FL.DTA')

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
         b4, # sex
         # sex = hc27, # PR module
         # eligible = hv120, #PR module
         h11,
         hw70, # Using WHO height/age calculation
         hw13 # result of height/weight measurement (whether or not measured)
         
  ) %>% 
  # -- Select only children under 5 --
  filter(age_yrs < 5, # Removes NA for ages
         region != 12) %>% # remove camps 
  mutate(sex = b4,
         diarrhea = h11,
         stunting_eligible = hw13 == 0,
         stunting_score = hw70) # whether the child's height/weight was measured; 0 == measured.

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
                    TRUE ~ NA_character_),
    
    # -- recode diarrheal incidence --
    diarrhea = case_when(ch$diarrhea %in% c(1, 2) ~ 1,
                         ch$diarrhea == 0 ~ 0,
                         TRUE ~ NA_real_), # don't know, missing, not applicable
    
    # -- recode height/age measurements --
    # Reset 9996 (height out of plausible limits), 9997 (age out of plausible limits), 9998 (flagged cases), 9999 (missing) to be missing
    stunting_score = if_else(stunting_score < 9996, stunting_score / 100, false = NA_real_, missing = NA_real_)
  )

# -- Check recoding was done properly --
table(ch$sex, ch$b4, useNA = 'ifany')

table(ch$diarrhea, ch$h11, useNA = 'ifany')

ch %>% filter(hw70 > 600) %>% group_by(stunting_score, hw70) %>% summarise(n())
qplot(data = ch, x = hw70/100, y = stunting_score) + coord_cartesian(xlim = c(-6, 6))


# Classify stunting -------------------------------------------------------
ch = ch %>% 
  mutate(stunted = if_else(stunting_score < -2, 1, 0, NA_real_),
         sev_stunted = if_else(stunting_score < -3, 1, 0, NA_real_))


# Quick table -------------------------------------------------------------
ch  %>% filter(!is.na(stunting_score))  %>% group_by(urban) %>% summarise(mean(sev_stunted), mean(stunted), mean(stunting_score), n())

ch  %>% filter(!is.na(stunting_score))  %>% group_by(region_name) %>% summarise(sev = percent(mean(sev_stunted)), 
                                                                                stunt = percent(mean(stunted)), 
                                                                                score = mean(stunting_score), n())


# Calculate stunting values -----------------------------------------------
# 4045 kids eligible to be measured and were
# 3984 kids with valid stunting measurements
stunted = ch %>% 
  filter(stunting_eligible == 1, !is.na(stunting_score))

# Set up the design of the DHS sampling frame
DHSdesign = svydesign(id = ~prim_sampling_unit, strata = ~sample_strata, weights = ~sample_wt, data = stunted)
summary(DHSdesign)

svymean(~stunted, DHSdesign, na.rm = TRUE)

svyby(~stunted, by = ~region_name, DHSdesign, svymean, deff, level = 0.95, na.rm = TRUE)
