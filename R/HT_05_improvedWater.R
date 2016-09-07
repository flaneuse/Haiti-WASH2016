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

# * hh: dataframe with household-level indicators
# * admin1-2: shapefiles containing geographic polygons of Haiti


# Set colors and other basics ---------------------------------------------
colour_water = 'YlGnBu'

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


# To isolate the effect of the bottled water, create classificaiton if improved, unimproved, or bottled.
bottled_codes = c(71, 72)
unimpr_water_codes_bottled = setdiff(unimpr_water_codes, bottled_codes)

# To double check published stats, using the DHS-classification for bottled water (as improved).
impr_water_codes_dhs = c(impr_water_codes, bottled_codes)
unimpr_water_codes_dhs = setdiff(unimpr_water_codes, bottled_codes)



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
                                   ifelse(time2water <= 30  & improved_water == 1, 1, 0)),
    # -- DHS classification -- 
    impr_water_dhs = ifelse(water_source %in% impr_water_codes_dhs, 1,
                                  ifelse(water_source %in% unimpr_water_codes_dhs, 0, NA)),
    
    # -- bottled water --
    impr_water_type =  ifelse(water_source %in% impr_water_codes, 'improved',
                              ifelse(water_source %in% unimpr_water_codes_bottled, 'unimproved',
                                     ifelse(water_source %in% bottled_codes, 'bottled',
                                     NA)))
  )

# Quick check bottled is correct:
# hh  %>% group_by(impr_water_type) %>% summarise(n())

# Set up sampling weights --------------------------------------------------
DHSdesign = svydesign(id = ~prim_sampling_unit, strata = ~sample_strata, weights = ~sample_wt, data = hh)
summary(DHSdesign)


# Apply sampling weights --------------------------------------------------


# Double-checking can replicate DHS numbers
# Off by ~ 0.1% because I removed NAs.  Otherwise, seems to check out.
water_dhs = calcPtEst('impr_water_dhs', by_var = 'dhs_region', design = DHSdesign, df = hh)

# -- By Admin1 + Port-au-Prince --
water_admin1_PaP = calcPtEst('impr_water_under30min', by_var = 'region_name', design = DHSdesign, df = hh)
# water_admin1_PaP = calcPtEst('improved_water', by_var = 'region_name', design = DHSdesign, df = hh)

# -- By Admin1 --
water_admin1 = calcPtEst('impr_water_under30min', by_var = 'admin1', design = DHSdesign, df = hh)

# -- By Admin2 --
water_admin2 = calcPtEst('impr_water_under30min', by_var = 'admin2', design = DHSdesign, df = hh)
# Merge in Admin1 names
water_admin2 = left_join(water_admin2, admin2_names, by = 'admin2')
# Merge in Admin1-level stats
water_admin1_sum = water_admin1 %>% 
  select(admin1, admin1_avg = avg, admin1_lb = lb, admin1_ub = ub)
water_admin2 = left_join(water_admin2, water_admin1_sum, by = 'admin1')
# Resort order of admin1 based on the admin1 averages.  Best (most improved toilet) == top.
water_admin2$admin1 = factor(water_admin2$admin1, levels = water_admin1$admin1)


# Admin2 dot plot ---------------------------------------------------------

pairGrid(water_admin2, savePlots = F, y_var = 'admin2')

# Admin1 map --------------------------------------------------------------
haiti_polygons = right_join(water_admin1_PaP, dhs_geo$df, by = c('region_name' = 'DHSREGFR'))

plotMap(haiti_polygons,          
        admin0 = hispaniola,         
        clipping_mask = admin0, 
        centroids = dhs_geo$centroids,
        fill_var = 'impr_water_under30min',
        fill_scale = colour_water,
        fill_limits = colour_limits,
        plot_base = FALSE,
        exportPlot = TRUE,
        fileName = '~/Creative Cloud Files/MAV/Projects/Haiti_WASH-PAD_2016-09/exported_R/HTI_imprwater30_adm1.pdf')


# Export data to krig surface ---------------------------------------------
# For krigging, unnecessary to apply weights to each EA (since same)
write_csv(hh, '~/Documents/USAID/Haiti/dataout/HT_DHS2012_imprsanitation_2016-08-29.csv')



# Admin2 map --------------------------------------------------------------
haiti_polygons = left_join(admin2$df, water_admin2, by = c('NAME_2' = 'admin2'))

# -- AVERAGE --
plotMap(haiti_polygons,          
        admin0 = hispaniola,         
        clipping_mask = admin0, 
        centroids = admin2$centroids,
        fill_var = 'impr_water_under30min',
        centroids_var = 'NAME_2',
        fill_scale = colour_toilet,
        fill_limits = colour_limits, 
        plot_base = TRUE,
        exportPlot = FALSE)

# -- dot plot --
pairGrid(water_admin2, 
         y_var = 'admin2',
         colorDot = colour_water,
         savePlots = TRUE,
         fill_limits = colour_limits,
         file_name =  '~/Creative Cloud Files/MAV/Projects/Haiti_WASH-PAD_2016-09/exported_R/HTI_imprwater_adm2dot.pdf')



# urban/rural splot -------------------------------------------------------

urb_rural_imprwater = calcPtEst('impr_water_under30min', by_var = 'region_urban', design = DHSdesign, df = hh)

urb_rural_imprwater = urb_rural_imprwater %>% 
  separate(region_urban, c('region', 'urban'), sep = '_and_') %>% 
  ungroup() %>% 
  arrange(region,urban) %>% 
  mutate(lagged = lag(avg))

ur_order_h2o = urb_rural_imprwater %>% 
  filter(urban == 'urban') %>% 
  arrange(avg)

urb_rural_imprwater$region = factor(urb_rural_imprwater$region, c(ur_order_h2o$region))



ur_pairGrid(urb_rural_imprwater, fill_scale = 'YlGnBu', fill_limits = colour_limits,
            file_name = '~/Creative Cloud Files/MAV/Projects/Haiti_WASH-PAD_2016-09/exported_R/HTI_UR_water30.pdf')
