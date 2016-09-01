# Haiti DHS Activity Design -----------------------------------------
#
# HT_08_cholera.R: Plot hot spots of cholera activity
#
# Script to pull data from the Demographic and Health Surveys on variables
# related to water, sanitation, and hygiene (WASH)
# 
# Data are from UNICEF's classification of prevalence of cholera
#
# Laura Hughes, lhughes@usaid.gov, 15 August 2016
# With Patrick Gault (pgault@usaid.gov) and Tim Essam (tessam@usaid.gov)
#
# Copyright 2016 by Laura Hughes via MIT License
#
# -------------------------------------------------------------------------

# Previous dependencies ---------------------------------------------------
# `HT_00_setupFncns.R`, `HT_01_importDHS_geo.R` are meant to be run first.  
# * The following are dependencies in thoses files:

# * admin0-2, etc.: shapefiles containing geographic polygons of Haiti + basemaps
# * libraries


# Set colors and other basics ---------------------------------------------
colour_cholera = 'YlGn'
size_point = 2.5

# Import data -------------------------------------------------------------

# List of priority communes for cholera interventions, as identified by UNICEF
# Raw data file cleaned up to match commune names by hand; in separate column ('commune' instead of 'communes')
cholera = read_excel('~/Documents/USAID/Haiti/rawdata/Haiti_cholera_UNICEF/Cholera Response Commune and Total Sanitation Campaign August 2016.xlsx', 
                     sheet = 2, skip = 1)


# List of cholera hotspot cities, where infections tend to occur.
chol_cities = read_csv('~/Documents/USAID/Haiti/rawdata/Haiti_cholera_UNICEF/Haiti_cholera_hotspotcities_UNICEF_2016-08-29.csv')



# Clean data --------------------------------------------------------------

cholera = cholera %>% 
  # Remove blank / rows or ones w/ coded data.
  filter(!is.na(Priority)) %>% 
  # Encode Category based on 2016 UNICEF classification
  mutate(prevalence2016 = factor(`Cholera Response (Mid-term Plan)`, 
                                 levels = c('C', 'B' ,'A'),
                                 labels = c('third priority', 'second priority', 'first priority')),
         prevalence2014 = ifelse(Priority == 'Phase 1', 2,
                                 ifelse(Priority == 'Phase 2', 1, NA)),
         commune = str_trim(commune),
         departement = str_trim(`Departement `))

# Merge City with Haiti
chol_cities = chol_cities %>% mutate(loc = paste0(city, ', Haiti'))

# Geocode cholera cities --------------------------------------------------
chol_cities = geocode(chol_cities$loc, messaging = TRUE, output = 'more')

# Visually check they're okay

info_popup <- paste0("<strong>Dep.: </strong>", 
                     chol_cities$administrative_area_level_1,
                     "<br><strong>city: </strong> <br>",
                     chol_cities$address)

leaflet(chol_cities) %>% 
  addCircles(~lon, ~lat, radius = 4000, opacity =  1,
             color = ~paired(administrative_area_level_1),
             popup = info_popup) %>% 
  addProviderTiles("Thunderforest.Landscape")


# Merge commune-level data with maps --------------------------------------
# Only highlights affected communes
# cholera_map = left_join(cholera, admin3$df, by = c('commune', 'departement'))

cholera_map = full_join(cholera, admin3$df, by = c('commune', 'departement'))

# Cholera prevalence map --------------------------------------------------

p = plotMap(cholera_map, 
            admin0 = hispaniola,
            clipping_mask = admin0,
            fill_var = 'prevalence2014',
            fill_scale = colour_cholera,
            fill_limits = c(0.5, 3.5),
            plot_base = FALSE,
            exportPlot = TRUE,
            fileName =  '~/Creative Cloud Files/MAV/Haiti_WASH-PAD_2016-09/exported_R/HTI_cholera_choro.pdf'
            )

p +
  geom_point(aes(x = lon, y = lat, group = 1),
             size = size_point, data = chol_cities, 
             colour = brewer.pal(9, colour_cholera)[9],
             fill = brewer.pal(9, colour_cholera)[7],
             shape = 21) +i
  geom_text(aes(x = lon, y = lat, group = 1,
                label = locality), 
            family = 'Lato',
            colour = brewer.pal(9, colour_cholera)[8],
            data = chol_cities) +
  scale_fill_gradientn(colours = brewer.pal(9, colour_cholera), 
                       limits = c(0.5, 3.5), na.value = grey15K) +
  theme(legend.position = 'none')

ggsave(filename = '~/Creative Cloud Files/MAV/Haiti_WASH-PAD_2016-09/exported_R/HTI_cholera_choro.pdf', 
       width = 10.75, height = 9, units = "in",
       bg = "transparent", 
       scale = (8.9555/8.1219),
       paper = "special", useDingbats = FALSE, compress = FALSE, dpi = 300)
