* Haiti DHS Activity Design -----------------------------------------
* Script to reproduce estimates created in R code. Script focuses on files
* 
*
* Data are from the 2012 DHS, available at http://dhsprogram.com/what-we-do/survey/survey-display-368.cfm
*
* Tim Essam, tessam@usaid.gov, 30 August 2016
* with Patrick Gault (pgault@usaid.gov) and Laura Hughes (lhughes@usaid.gov)
*
* Copyright 2016 by Tim Essam via MIT License
*
* -------------------------------------------------------------------------

clear 
capture log close 

* Set up working directories
global pathin "C:/Users/Tim/Documents/Haiti_DHS2012/Datain"
global pathout "C:/Users/Tim/Documents/Haiti_DHS2012/Dataout"
global pathStata "C:/Users/Tim/Documents/GitHub/Haiti-WASH2016/Stata"
global pathgit "C:/Users/Tim/Documents/GitHub/Haiti-WASH2016"

cd $pathin
use "$pathin/hthr61dt/HTHR61FL.DTA", clear

*** Extract key variables for analysis; Using the HT_02_importDHS_hh.R file
#delimit ;
local relvars  "cluster_id hh_num sample_wt prim_sampling_unit sample_strata 
			    dhs_region urban region interview_month wealth_idx
				time2water water_source toilet_source share_toilet num_share_toilet";
				
local dhsvars  "hv001 hv002 hv005 hv021 hv022 
				shlocnew3 hv025 shregnew hv006 hv271
			    hv204 hv201 hv205 hv225 hv238";
#delimit cr				
				
local n : word count `relvars'

forvalues i = 1/`n' {
	local a : word `i' of `relvars'
	local b : word `i' of `dhsvars'
	clonevar `a' = `b'
	di in yellow "`a'     =  `b'"
	}
*end

* Deflate variables that need it
replace sample_wt = sample_wt / 1e6
replace wealth_idx = wealth_idx / 1e5

* Rebase urban variable
ren urban urbantmp
recode urbantmp (1 = 1 "Urban")(2 = 0 "Rural"), gen(urban)

*** Reproduce variables in HT_04_improvedToilets.R ***
* Clean up 999s 998s and other oddities to make sense
replace water_source = . if inlist(water_source, 99)

* 998 = don't know and is assumed to be missing; 996 is on-premise so making it 0
replace time2water = . if inlist(time2water, 999, 998)
replace time2water = 0 if inlist(time2water, 996) 

replace toilet_source = . if inlist(toilet_source, 99)
replace share_toilet  = . if inlist(share_toilet, 9)
replace num_share_toilet = . if inlist(num_share_toilet, 98, 99)
replace num_share_toilet = 10 if inlist(num_share_toilet, 95)

* Classify toilets based on whether they are improved or not
recode toilet_source (10 11	12 13 14 15	20 21 22 41 = 0 "Improved") /*
*/ (23 42 43 44 45 96 = 1 "Unimproved")(30 31 = 2 "Open defication"), gen(toilet_type)

g improved_toilet = 0
replace improved_toilet = 1 if toilet_type == 0 & share_toilet == 0
replace improved_toilet = 0 if toilet_type == 0 & share_toilet == 1
replace improved_toilet = 0 if inlist(toilet_type, 1, 2) 

tab toilet_type improved_toilet, mi

* Survey set the data to account for complex sampling design and get correct se's
svyset [pw = sample_wt], psu(prim_sampling_unit) strata(sample_strata)

svy:mean improved_toilet

svy:mean improved_toilet, over(region )

*** Reproduce variables and point estimates in HT_05_improvedWater.R ***
* Process water types
g byte improved_water = inlist(water_source, 10, 11, 12, 13, 14, 20, 21, 31, 32, 41, 51)
replace improved_water = . if water_source == .
tab improved_water if region != 12

g byte impr_water_under30min = (time2water <= 30 & improved_water == 1)
replace impr_water_under30min = . if inlist(., time2water, improved_water)

* DHS definition includes bottled water as an improved water source
g byte impr_water_dhs = inlist(water_source, 10, 11, 12, 13, 14, 20, 21, 31, 32, 41, 51, 71, 72)
replace impr_water_dhs = . if water_source == .

* Bottled water
g impr_water_type = .
replace impr_water_type = 1 if inlist(water_source, 10, 11, 12, 13, 14, 20, 21, 31, 32, 41, 51)
replace impr_water_type = 3 if inlist(water_source, 71, 72)
replace impr_water_type = 2 if inlist(water_source, 30, 33, 34, 40, 42, 43, 61, 62, 96)
la def wat_type 1 "improved" 2 "unimproved" 3 "bottled"
la val impr_water_type wat_type

tab impr_water_type if region!=12

* Survey design is already set so no need to set again
svy:mean impr_water_dhs, over(dhs_region)

svy:mean impr_water_under30min, over(region)
svy:mean improved_water, over(region)

capture log close

