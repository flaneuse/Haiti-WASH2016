# README

## Haiti DHS Activity Design
Script to pull data from the Demographic and Health Surveys on variables related to water, sanitation, and hygiene (WASH)

Data are from the 2012 DHS, available at http://dhsprogram.com/what-we-do/survey/survey-display-368.cfm

Laura Hughes, lhughes@usaid.gov, 15 August 2016
with Patrick Gault (pgault@usaid.gov) and Tim Essam (tessam@usaid.gov)


Copyright 2016 by Laura Hughes via MIT License

## Order of files to be read:
For the scripts to be reproducible, they should be run in the following order. In certain cases, the inputs from previous files
are used in subsequent.  These should be noted at the top of each of the scripts.
* `HT_00_setupFncns.R`: loads packages and creates reusable functions
* `HT_01_importDHS_geo.R`: imports geographic coordinates of clusters (DHS) and admin units (nat'l)
* `HT_02_importDHS_hh.R`: imports household-level data (DHS)
* `HT_03_importDHS_child.R`: imports individual children's data (DHS)
* `HT_04_improvedToilets.R`: calculates percentage of improved toilets geographically across Haiti (DHS)
* `HT_05_improvedWater.R`: calculates percentage of improved drinking water geographically across Haiti (DHS)
* `HT_06_stunting.R`: calculates percentage of child stunting geographically across Haiti (DHS)
* `HT_07_diarrhea.R`: calculates percentage of diarrheal prevalence in children geographically across Haiti (DHS)
* `HT_08_cholera.R`: hotspots of cholera infection (UNICEF)

## Individual files
### `HT_00_setupFncns.R`
* loads in all necessary packages
* sets working directory for location of raw data

### `HT_01_importDHS_geo.R`
* imports geographically-offset household cluster locations
* imports shapefiles of administrative units of Haiti and relevant basemaps


### `HT_02_importDHS_hh.R`
* imports household-level data from DHS using module HR
* pulls relevant variables for future analysis
* mildly cleans data (removing NAs, recoding variables based on codebook, converting real numbers by their scaling factors)
* merges hh data with geographic data

### `HT_03_importDHS_child.R`
* imports individual-level children's data from DHS using module KR
* pulls relevant variables for future analysis
* mildly cleans data (removing NAs, recoding variables based on codebook, converting real numbers by their scaling factors)
* merges hh data with geographic data

### `HT_04_improvedToilets.R`
* imports classification of toilets as being improved or unimproved
* calculates improved latrine percentage by geography
* merges into geographic object to make choropleths

### `HT_05_improvedWater.R`
* imports classification of drinking water source as being improved or unimproved
* calculates improved drinking water percentage by geography
* merges into geographic object to make choropleths

### `HT_08_cholera.R`
* imports locations of cholera outbreaks from UNICEF priority-areas
* georeferences and merges to a mapping dataset

## Individual products
All maps and visualizations:

### `01_HTI_.pdf`

### Adobe Illustrator cleanup of maps
#### Basemap
* Apply blue border with Gaussian blur to Hispaniola to simulate water (9 pt., 40 px. blur, #89a3d1); background #f6f8fb
* Apply white border (25 pt) along edge of bounding box with 30 px. Gaussian blur
* Country outline: 0.25 pt. 90% K
* Admin1 boundaries: 0.5 pt. #ffffff

#### Raster layers
* Export from ArcMap as .ai: 720 dpi, RGB, "Vectorize layers with bitmap markers/fill", "Convert Marker Shapes to Polygons"
* Group raster slices
* Clip image to Admin0 polygon
* Apply "multiply" transparency to raster layer (100% transparency)
* Apply drop shaadow to the entire raster layer (clipping mask + raster): 75% multiply, 90% K, x = 0.02 in., y = 0.02 in., blur = 0.02 in.
* nudge; bounding box at x = 0.0014", y = 0.126"

#### Choropleths
* nudge to x = 0.2625" y = 0.7902" (upper left corner)

#### Export to .pdf
* AI preset High Quality Print with "PDF/X-4:2010" (under "standard")

#### Dot plots
* Symbol strokes to 0.25"
* Apply drop shaadow to the shapes: 75% multiply, 90% K, x = 0.01 in., y = 0.01 in., blur = 0.01 in.
* round corners of square 0.02"