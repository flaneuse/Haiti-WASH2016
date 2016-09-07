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
cholera = read_excel(paste0(local_wd, 'Haiti_cholera_UNICEF/Cholera Response Commune and Total Sanitation Campaign August 2016.xlsx'), 
                     sheet = 2, skip = 1)


# List of cholera hotspot cities, where infections tend to occur.
chol_cities = read_csv(paste0(local_wd, 'Haiti_cholera_UNICEF/Haiti_cholera_hotspotcities_UNICEF_2016-08-29.csv'))

# Cholera by Admin1 -------------------------------------------------------
# Data are from PAHO/WHO, contributed by UN OCHA to HDX Humanitarian Data Exchange
# https://data.humdata.org/dataset/7b7bde8e-cf30-4d11-ab58-dd5091156d81

chol_admin1 = read_excel(paste0(local_wd, 'Haiti_cholera-cases-per-month-and-per-region-since-2015_PAHO_Hdx.xlsx'))

chol_byMonth = read_excel(paste0(local_wd, 'Haiti_cholera-cases-per-month-since-2010_PAHO_Hdx.xlsx'), col_types = c('date', 'numeric', 'numeric'))

pop_admin1 = read_excel(paste0(local_wd, 'Haiti_population_Admin1_IHSI2015POP.xlsx')) %>% 
  select(Departement = Dept, pop = POP_Total)


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

# Merge cholera by admin1 with departemental population; calc rate
# recode pop Grande-Anse name
pop_admin1 = pop_admin1 %>% 
  mutate(Departement = ifelse(Departement == 'Grande Anse', 'Grande-Anse', Departement))

chol_admin1 = left_join(chol_admin1, pop_admin1, by = 'Departement')

chol_admin1 = chol_admin1 %>% 
  mutate(rate = Cases/pop)

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
            fileName =  '~/Creative Cloud Files/MAV/Projects/Haiti_WASH-PAD_2016-09/exported_R/HTI_cholera_choro.pdf'
            )

p +
  geom_point(aes(x = lon, y = lat, group = 1),
             size = size_point, data = chol_cities, 
             colour = brewer.pal(9, colour_cholera)[9],
             fill = brewer.pal(9, colour_cholera)[7],
             shape = 21) +
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


# Cholera by month, region --------------------------------------------------------
x = chol_admin1 %>% group_by(Departement) %>% 
  summarise(cases = sum(Cases, na.rm = TRUE), 
            deaths = sum(Deaths, na.rm = TRUE),
            rate = signif(max(rate, na.rm = TRUE), digits = 2)) %>% 
  mutate(fc = rate/0.00059) %>% 
  arrange(desc(rate))

x = full_join(admin1$df, x, by = c('A1_Name' = 'Departement'))
plotMap(x, 
        admin0 = hispaniola,
        clipping_mask = admin0,
        fill_var = 'cases',
        fill_scale = colour_cholera,
        fill_limits = c(0, 20000),
        plot_base = FALSE,
        exportPlot = FALSE,
        fileName =  '~/Creative Cloud Files/MAV/Projects/Haiti_WASH-PAD_2016-09/exported_R/HTI_cholera_choro.pdf'
)

# Cases by Dept. over time
ggplot(chol_admin1, aes(x = Month, y = Cases)) +
  geom_line() + 
  facet_wrap(~ Departement)

# Rate of cases by Dept. over time
ggplot(chol_admin1, aes(x = Month, y = rate)) +
  geom_line() + 
  facet_wrap(~ Departement)

ggplot(chol_byMonth, aes(x = Month, y = `Monthly cases`)) +
  geom_line()
