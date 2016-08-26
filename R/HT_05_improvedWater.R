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
# `HT_01_importDHS_geo.R`, `HT_02_importDHS_hh.R` are meant to be run first.  The following are dependencies in thoses files:

# * hh: dataframe with household-level indicators
# * admin1-2: shapefiles containing geographic polygons of Haiti

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

# Set up sampling weights --------------------------------------------------
DHSdesign = svydesign(id = ~prim_sampling_unit, strata = ~sample_strata22, weights = ~sample_wt, data = hh)
summary(DHSdesign)

svymean(~improved_water, DHSdesign, na.rm = TRUE)