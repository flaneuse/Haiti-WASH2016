# Haiti DHS Activity Design -----------------------------------------
#
# HT_01_importDHS_geo.R: pull the geographic data
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

# Function to clean geodata from 2012 Haiti DHS and to import basic shapefiles of Haiti administrative units


# Previous dependencies ---------------------------------------------------
# `HT_00_setupFncns.R` is meant to be run first.  
# The following are dependencies in that file:

# * all custom functions
# * all necessary libraries



# Import coordinates of clusters ------------------------------------------
# NOTE!: There are 8 clusters within the DHS file which have NULL coordinates.
# These correspond to ~ 238 hh in the data set (excluding camps.)
# For the purposes of calculating averages at the departement level (Admin1),
# these households are *included* since they have 

# NOTE!: Administrative units in Haiti are sort of confusing. Both communes
# and arrondissements are commonly referred to as being Admininstrative boundary 2,
# depending on who you talk to.
# In this analysis:
# -- ADMIN1 = DÉPARTEMENT --
# -- ADMIN2 = ARRONDISSEMENT --
# -- ADMIN3 = COMMUNE --

# Raw data directly from DHS. Replaced by spatially joined data (below)
# geo_raw = read_shp(workingDir = paste0(local_wd, 'Haiti_DHS2012/htge61fl/'),
# layerName = 'HTGE61FL')
# geo = geo_raw@data

# Raw DHS data was spatially joined with Haiti Admin2 and Admin3 boundaries in Esri. 
# Admin1 and 3 boundaries were provided by Joel Barthelemy, GIS specialist in Haiti
# Admin2 boundary from GADM (arrondissement)


# Admin2 spatial join (Arrondissements)
geo_admin2 = read_shp(workingDir = paste0(local_wd, 'htGEO/'), 
                      layerName = '2013_HTI_DHSClusterGeoGADM')
geo_admin2 = geo_admin2@data

# Admin3 spatial join (Communes)
geo_admin3 = read_csv(paste0(local_wd, 'htGEO/2013_HTI_DHS_ClusterGEO.csv'))

# Merge Admin2 and Admin3 data
geo = full_join(geo_admin2, geo_admin3,
                by = c("FID_HTGE61", "DHSID", "DHSCC", "DHSYEAR", "DHSCLUST", "CCFIPS", "ADM1FIPS", "ADM1FIPSNA", "ADM1SALBNA", "ADM1SALBCO", "ADM1DHS", "ADM1NAME", "DHSREGCO", "DHSREGNA", "SOURCE", "URBAN_RURA", "LATNUM", "LONGNUM", "ALT_GPS", "ALT_DEM", "DATUM"))


# Clean geodata -----------------------------------------------------------

# -- Select data, convert urban/rural to binaries --
geo = geo %>% 
  select(cluster_id = DHSCLUST, 
         urban = URBAN_RURA, 
         lat = LATNUM, lon = LONGNUM,
         admin1 = ADM1NAME, A1_PCode, # departement
         admin2 = NAME_2, ID_2, # arrondisment
         admin3 = A2_Name, A2_PCode # commune
  ) %>% 
  mutate(urban = ifelse(urban == 'U', 1, 
                        ifelse(urban == 'R', 0, NA)),
         # Fix lat/lon that are in the middle of the Atlantic
         lat = ifelse(lat == 0, NA, lat),
         lon = ifelse(lon == 0, NA, lon)
  )


# quick map ---------------------------------------------------------------

# quick map of cluster locations

paired = colorFactor("Paired", domain = NULL)

leaflet(geo) %>% 
  addCircles(~lon, ~lat, radius = 1000,
             color = ~paired(admin1)) %>% 
  addProviderTiles("Thunderforest.Landscape")




# Import Admin shapefiles -------------------------------------------------
# From Joel and the CNIGS, http://haitidata.org/layers/

# 10 Admin1 units + Port-au-Prince metro area
dhs_geo = shp2df(workingDir = paste0(local_wd, 'Haiti_DHS2012/shps/'),
                 layerName = 'sdr_subnational_boundaries',
                 labelVar = 'DHSREGFR')

# All island
hispaniola = shp2df(workingDir = paste0(local_wd, 'Haiti_AdminBndry/'),
                    layerName = 'hti_boundaries_international_cnigs_polygon',
                    getCentroids = FALSE)
# Filter out inland water
hispaniola = hispaniola %>% filter(id %in% c('1', '10', '11', '12', '2', '27', '28', '6', '5'),
                                   piece == '1')

leaflet(hispaniola) %>%
  addCircles(~long, ~lat, radius = 1000,
             color = ~paired(id)) %>%
  addProviderTiles("Thunderforest.Landscape")

# Country border
admin0 = shp2df(workingDir = paste0(local_wd, 'Haiti_AdminBndry/'),
                layerName = 'hti_polbnda_adm0_cnigs',
                getCentroids = FALSE)

# Lakes
lakes = shp2df(workingDir = paste0(local_wd, 'Haiti_AdminBndry/'),
               layerName = 'hti_topo_lakes_polygon_092008',
               getCentroids = FALSE)

# Departements
admin1 = shp2df(workingDir = paste0(local_wd, 'Haiti_AdminBndry/'),
                layerName = 'hti_polbnda_adm1_cnigs',
                labelVar = 'A1_Name')

# Arrondissements
admin2 = shp2df(workingDir = paste0(local_wd, 'Haiti_AdminBndry/'),
                layerName = 'HTI_arrondissement_gadm',
                labelVar = 'NAME_2')

# Communes (from 2012)
admin3 = shp2df(workingDir = paste0(local_wd, 'Haiti_AdminBndry/'),
                layerName = 'hti_polbnda_adm2_cnigs',
                labelVar = 'A2_Name')

admin3$df$commune = str_trim(admin3$df$A2_Name)
admin3$df$departement = str_trim(admin3$df$A1_Name)


# Lookup table for Admin1-3 -----------------------------------------------
admin2_names = admin2$df %>% group_by(admin1 = NAME_1, admin2 = NAME_2) %>%
  summarise(n = n()) %>% select(-n) %>% 
  ungroup() %>% 
  mutate(admin1 = ifelse(admin1 == "L'Artibonite", 'Artibonite', as.character(admin1)))
