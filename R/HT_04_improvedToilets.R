# Haiti DHS Activity Design -----------------------------------------
#
# HT_04_improvedToilets: calculate improved latrine percentages
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

# Previous dependencies ---------------------------------------------------
# `HT_00_setupFncns.R`, `HT_01_importDHS_geo.R`, `HT_02_importDHS_hh.R` are meant to be run first.  
# The following are dependencies in thoses files:

# * All necessary functions and packages
# * hh: dataframe with household-level indicators
# * admin1-2: shapefiles containing geographic polygons of Haiti


# Set colors and other basics ---------------------------------------------
colour_toilet = 'YlOrBr'

# classify improved/ not improved -----------------------------------------
# DHS claims to use WHO definitions for sanitation; similar to those provided by Dr. Elizabeth Jordan, 
# WHO/UNICEF Joint Monitoring Programme for Water Supply and Sanitation: http://www.wssinfo.org/definitions-methods/watsan-categories/

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
# Only 13 hh outside the camps use them.  Using Liz's definitions (against DHS)

# -- Improved sanitation definitions --
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

# -- Reclassify toilets as being improved or not. --
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

# Set up sampling weights --------------------------------------------------
DHSdesign = svydesign(id = ~prim_sampling_unit, strata = ~sample_strata, weights = ~sample_wt, data = hh)
summary(DHSdesign)

# Apply sampling weights

# -- National-level stats (sans camps) --
toilet_natl = svymean(~improved_toilet, DHSdesign, na.rm = TRUE)

# -- Recalculating DHS-published stats --
# dhs_region                        improved_toilet         se
# aire metropolitaine (without camp)       0.3782652 0.02486723
      # autres villes (without camp)       0.3808763 0.02206386
              # rural (without camp)       0.1962384 0.01383396
toilet_dhs = svyby(~improved_toilet, by = ~dhs_region, design = DHSdesign, svymean, na.rm = TRUE)

# -- By Admin1 + Port-au-Prince --
toilet_admin1_PaP = calcPtEst('improved_toilet', by_var = 'region_name', design = DHSdesign, df = hh)

# -- By Admin1 --
toilet_admin1 = calcPtEst('improved_toilet', by_var = 'admin1', design = DHSdesign, df = hh)

# -- By Admin2 --
toilet_admin2 = calcPtEst('improved_toilet', by_var = 'admin2', design = DHSdesign, df = hh)

# -- By EA (for kriging) --
# Weights have no effect on EAs, but useful way to summarise by EA
toilet_ea = calcPtEst('improved_toilet', by_var = 'cluster_id', design = DHSdesign, df = hh)


# Admin1 map --------------------------------------------------------------
# NOTE!!!! Keep track of this, cuz it'll save you HOURS of debugging.
# Since Aire Métropolitaine is charming and French, it has an accent.  
# This means that its UTF-8 encoding doesn't like to play nicely with the
# DHS spellings of the names, even though THEY'RE EXACTLY THE SAME.
# Encodings are the worst.
# Anyway, turns out that if you flip the way the merge works, tidyr seems to translate the 
# string into the same type of encoding (I think) and they merge properly.
# Point being: put the hh variable FIRST (as in the data data), NOT the geographic data frame.
# left_join(geo_data, dhs_data) doesn't work, but right_join(dhs_data, geo_data) does
# Hopefully, this is only an issue because the DHS strips accents from their data frame but leaves them in for spatial data set.
haiti_polygons = right_join(toilet_admin1_PaP, dhs_geo$df, by = c('region_name' = 'DHSREGFR'))

# -- AVERAGE --
plotMap(haiti_polygons, 
        fill_var = 'improved_toilet',
        fill_scale = colour_toilet,
        fill_limits = c(0.05, 0.45))

# -- LOWER BOUND --
plotMap(haiti_polygons, 
        fill_var = 'lb',
        fill_scale = colour_toilet,
        fill_limits = c(0.05, 0.45))

# -- UPPER BOUND --
plotMap(haiti_polygons, 
        fill_var = 'ub',
        fill_scale = colour_toilet,
        fill_limits = c(0.05, 0.45))

# Admin2 map --------------------------------------------------------------
haiti_polygons = left_join(admin2$df, toilet_admin2, by = c('NAME_2' = 'admin2'))

# -- AVERAGE --
plotMap(haiti_polygons, 
        fill_var = 'improved_toilet',
        fill_scale = colour_toilet,
        fill_limits = c(0, 0.55))

# -- LOWER BOUND --
plotMap(haiti_polygons, 
        fill_var = 'lb',
        fill_scale = colour_toilet,
        fill_limits = c(0.0, 0.55))

# -- UPPER BOUND --
plotMap(haiti_polygons, 
        fill_var = 'ub',
        fill_scale = colour_toilet,
        fill_limits = c(0.0, 0.5))
