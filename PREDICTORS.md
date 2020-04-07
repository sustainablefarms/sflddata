# Predictors for Biodiversity Models

<!-- vim-markdown-toc GFM -->

* [Include in Future Model Exploration](#include-in-future-model-exploration)
	* [Geology/Elevation](#geologyelevation)
	* [Vegetation Properties at Site](#vegetation-properties-at-site)
	* [Vegetation Coverage Fractions Nearby](#vegetation-coverage-fractions-nearby)
	* [Spatial Configuration of Vegetation Cover](#spatial-configuration-of-vegetation-cover)
	* [Land Use](#land-use)
	* [Ground Observations](#ground-observations)
	* [Other](#other)
	* [Things that will probably need proxies, extra modelling or ignoring](#things-that-will-probably-need-proxies-extra-modelling-or-ignoring)
		* [Known Proxy / Method to Estimate](#known-proxy--method-to-estimate)
		* [Unknown how to estimate or proxy](#unknown-how-to-estimate-or-proxy)
			* [Unrelated to Fencing](#unrelated-to-fencing)
			* [Related to Fencing](#related-to-fencing)
* [For now, do not include](#for-now-do-not-include)
* [Included in Late 2019 Model m3_0](#included-in-late-2019-model-m3_0)
* [Included in Early 2020 Model m4_1](#included-in-early-2020-model-m4_1)

<!-- vim-markdown-toc -->


*"Much of the variation (>70%) in bird responses was explained by 3 factors: remnant native‐vegetation attributes (native grassland, scattered paddock trees, patches of remnant native woodland); presence or absence of planted native trees; and the size and shape of tree plantings."* [https://doi.org/10.1111/j.1523-1739.2008.00924.x]


## Include in Future Model Exploration
 
### Geology/Elevation
+ Terrain Wetness Index (TWI) average over 500m radius disc  *HighPriority: related to vegetation struture, suggested by Mason*
  + Has more importance in drier climates
  + average of 1km-wide square (100 ha) has an impact on habitat structure [https://doi.org/10.1007/s10980-015-0193-5]. Details of calculation in the paper referenced next [https://doi.org/10.1007/s10980-011-9665-4].
  + average TWI in a 500m buffer (a proxy for productivity of the site) [https://doi.org/10.1007/s10980-011-9665-4]   (interacts with Noisy Miner)
     + calculated from a 20m ground sample distance (GSD) DEM. Using ANUDEM algorithm.
  + Available from CSIRO [here](https://www.clw.csiro.au/aclep/soilandlandscapegrid/ProductDetails-LandscapeAttributes.html)

+ Elevation (highly correlated with TWI in [https://doi.org/10.1007/s10980-015-0193-5]).
  + A 0.01 degree resolution DEM is available on NCI for registered use (see [here](https://geonetwork.nci.org.au/geonetwork/srv/eng/catalog.search#/metadata/514968b2-9d47-4b73-a42f-907e1c32231e)) but this is equivalent to a 5 - 10km resolution.
  + The Shuttle Radar Topography Mission (SRTM) has 30m (ish) resolution.
Product information [here](https://doi.org/10.5066/F7PR7TFT).
It can be downloaded in 1 second tiles from NASA's Earth Explorer.
 A derivative can be used directly through WMS and other serving methods from Geoscience Australia's national 1-second DEM [here](http://gaservices.ga.gov.au/site_9/rest/services/DEM_SRTM_1Second/MapServer). 

+ Aspect (based on DEM). Useful for habitat structure [https://doi.org/10.1007/s10980-015-0193-5]
  + Available from CSIRO [here](https://www.clw.csiro.au/aclep/soilandlandscapegrid/ProductDetails-LandscapeAttributes.html)

+ Geodiversity (not soil maps though - as spatial variation of soil is very high). Mentioned by David L. __UnknownAccessibility__

+ Lithology fertility [https://doi.org/10.1007/s10980-015-0193-5] __UnknownAccessibility__
  + Natural soil fertility is the same thing I think - used here [https://doi.org/10.1111/aec.12414]

+ Presence/absence/type/size of granite inselbergs (matters to lizards [https://doi.org/10.1111/j.1442-9993.2009.02092.x] __UnknownAccessibility__

+ Land Capability  __UnknownAccessibility__

### Vegetation Properties at Site
+ GrowthType (Remnant/regrowth/planting)   *HighPriority: heavily related to vegetation struture*
  + Remnant vs regrowth (vs planting?) has an effect, but it is mixed depending on species [https://doi.org/10.1016/j.biocon.2019.05.015]
  + Age of planting/regrowth? __HeavyProcessing__
  + plantings are not a refuge from Noisy Miners. Much fewer bird species live in plantings than old growth [https://doi.org/10.1111/ddi.12444]
  + Initially could use the on-ground data, but later might be able to use landsat to detect anything younger than the 1980s. I expect someone would have created a map of these GrowthTypes somewhere. __UnkownLocation__
     + does not appear to be part of the PCT labels below
     + 1988 maps of tree cover might be available through NSW's [SLATS WoodyCover change map](https://datasets.seed.nsw.gov.au/dataset/nsw-slats-landsat-woody-change-derived-vector-database-1988-2010f5add) and the [Woody Extent + Folliage Projective Cover 2008 map](https://datasets.seed.nsw.gov.au/dataset/landsat-woody-extent-and-foliage-projective-cover-fpc-ver-2-1-25m-20087355d). The maps of change are quite sparse and it would only be useful for planting/regrowth of locations *cleared* since 1988. 
     + 1988 maps might already be calculated at 25m resolution: files of that name are in: http://dapds00.nci.org.au/thredds/catalog/ub8/au/treecover/catalog.html
  + Strong effect on detection [Lindenmayer et al, 2020](https://www-sciencedirect-com.virtual.anu.edu.au/science/article/pii/S0006320719302976).

+ Plant Community Type (PCT). 25m GSD. Updated at different dates for different regions.
  + Is only native vegetation
  + Has detailed descriptions (e.g. "River red gum-sedge dominated very tall open forest in frequently flooded forest wetland along major rivers and floodplains in south-western NSW"), __Keith Class__ (e.g. Inland Riverine Forest) and __Keith Form__ (e.g. Forested Wetland). More information also available (e.g. numerical predictions of class)
  + [https://doi.org/10.1016/j.biocon.2012.02.026] found Floodplain Transition Woodland, Inland Floodplain Woodland, Riverine Plain Woodland, Riverine Sandhill Woodland has impacts on bird assemblage.
  + Available [here](https://www.environment.nsw.gov.au/vegetation/state-vegetation-type-map.htm)
  + Non-native conifers could be detected by their absence in the PCT, but, with work could also be detected from satellite data.

+ transitional stock route region (TSR) [https://doi.org/10.1016/j.biocon.2012.02.026]
__UnknownAccessibility__ 

+ Gross Primary Productivity (GPP)
  + median across all years
  + median difference of min GPP and max GPP across years
  + difference to mean at each time (correlated with m1b_residual)
  + average over 500m radius disc?
  + summer GPP (or GPP residuals) and winter GPP residuals.
     + Summer will pick up native veg and lucerne.
     + Spring will pick up everything else.
  + GPP of nearby cleared pixel __NeedResearch__
     + measures farming practise only
     + detecting non-forested pixels might be hard
          + might have to reprojected and resample everything to the NCAS data
  + Data location described [here](http://wald.anu.edu.au/australias-environment/#Download).

+ rate of woody cover change (increase, decrease, stable)


### Vegetation Coverage Fractions Nearby 
+ Tree cover: the fraction of a 500m buffer of the site that is tree cover  *HighPriority: heavily related to bird abundance and easy to measure*
  + using the NCAS forest map (Furby?)
  + possibly centred on the 100m point of a each transect [https://doi.org/10.1098/rspb.2019.0114]
  + 500m radius is used in other studies [https://doi.org/10.1016/j.biocon.2009.07.009]
  + ANUWALD have a percent woody cover map

+ Area of tree cover binned to 1000ha and 10000ha resolution
  + corresponds to buffer distances of approximately 3km and 10km.
  + suggested by [https://doi.org/10.1111/ddi.12145], with some confusion on whether it is native vegetation or not - predominantly it is native
  + [https://doi.org/10.1371/journal.pone.0097029] similar but used the term 'woody vegetation' consistently

+ Landsat-derived fractional cover index (Foliage Projection Cover) from the OEH (now in the department of planning, industry and environment).
  + mean of a 500m buffer
  + could be highly correlated to tree cover __NeedResearch__ 

+ ALOS Woody Biomass (derived from RADAR). 50m GSD. Static (year of 2009). Related paper suggests it may not include understory.
  + could be highly correlated with other tree cover data
  + But could add really valuable radar-derived information



+ GPP within 500m buffer
   + Consider separating GPP + FMC of cropping from that of woody veg
   + __NeedResearch__
     + what versions of GPP shall be used?

+ Percent PV, NVP and bare soild within 500 metres:
  + Photosynthetic Vegetation (PV)
  + Non-Photosynthetic Vegetation (NPV)
  + Bare Soil (BS) 
  + Availabiity:
     + all of the above at 500m resolution derived from MODIS. See [https://www.agriculture.gov.au/abares/aclump/land-cover/ground-cover-monitoring-for-australia]  
     + Also at 25m resolution through Geoscience Australia's Digital Earth Australia: Fractional Cover (percentages of Bare Soil, Photosynthetic Vegetation and Non-Photosynthetic Vegetation) The derivation of PV/NPV/BS is described [here](https://d28rz98at9flks.cloudfront.net/79676/Fractional_Cover_FC25_v1_5.PDF
and [here](http://data.auscover.org.au/xwiki/bin/view/Product+pages/Landsat+Seasonal+Fractional+Cover) 
and the underlying science [here](https://www.sciencedirect.com/science/article/pii/S0034425715000395) 


+ Area of native grass within 500m of a site (see https://doi.org/10.1016/j.biocon.2009.07.009).  __NeedResearch__  *HighPriority: related to bird abundance and I think will be easy to obtain*
  + This could be possible to determine using summer indications of bare ground, and woody veg. 
  + Will need a way to ignore lucerne though 
  + Might be available in the PCT map __UnknownAccessibility__
  + Was highly weighted in a principle component that described 50% of the variation in birds [https://doi.org/10.1111/j.1523-1739.2008.00924.x]


### Spatial Configuration of Vegetation Cover
+ Number of paddock trees within 500m [https://doi.org/10.1016/j.biocon.2009.07.009] *HighPriority: regarded as a keystone feature for biodiversity and should be easy to measure*
  + paddock tree populations are declining with time, but mostly due to fire? [https://doi.org/10.1111/aec.12414]
  + mostly canopy is 10m - 25m in diameter
  + these trees generally contain features that take centuries to develop naturally [https://doi.org/10.1111/aec.12414]
  + was highly weighted in a principle component that explained over 50% of variation in birds [https://doi.org/10.1111/j.1523-1739.2008.00924.x]
  + get paddock trees by __NeedResearch__ __HeavyProcessing__
     + thresholding tree canopy patches to below 40mx40m (say)
     + paddock trees at time of the 2011 5m GSD NSW tree canopy map [here](http://data.auscover.org.au/xwiki/bin/view/Product+pages/nsw+5m+woody+extent+and+fpc)  could give a good approximation
     + Tree canopy maps derived more recently from SPOT data would be really nice
     + age from historical maps or assume very old __NeedResearch__

+ Patch size containing site - used in [https://doi.org/10.1016/j.biocon.2009.07.009].  (could require a lot of work processing though) __HeavyProcessing__

+ width of planting has an effect on birds [https://doi.org/10.1111/rec.12676]. But a frog paper found it didn't matter much to frogs [https://doi.org/10.1016/j.agee.2019.05.003].
  + [https://doi.org/10.1007/s10531-016-1140-8] found it only had an effect on plantings, not old growth (or natural regrowth too i guess)
  + __NeedResearch__ __HeavyProcessing__
     + how to teach a computer what the 'width' of a remnant is

+ Edge index of remnant vegetation [https://doi.org/10.1111/j.1523-1739.2008.00924.x]

+ Configuration indices based on statistical models of landscape generation
  + (spatial) covariance estimated from a --- metre region
  + mean of 'contact distance' estimated from a --- metre region
  + Suggest low priority. Would be interesting to test as they are related to my PhD and I can easily calculate them. But a good chance they won't improve biodiversity predictions much
  + __NeedResearch__ __HeavyProcessing__

+ Structural Connectivity
  + affected persistence and colonisation, but not occupancy (at start of study). [Lindenmayer et al 2020](https://www-sciencedirect-com.virtual.anu.edu.au/science/article/pii/S0006320719302976).

### Land Use
+ % Cropland within 100 ha (using NSW's 2011 Land use map) or the CLUM [https://doi.org/10.1007/s10980-015-0193-5] 

+ % cover of Catchment-scale Land Use and Management (CLUM) classes with 500m radius
  + December 2018 update (updates to Burdekin in QLD, NSW, Vic, WA)
  + Classes given by Australian Land Use and Management (ALUM) Classification version 8
  + data available [here](https://www.agriculture.gov.au/abares/aclump/land-use/catchment-scale-land-use-of-australia-update-december-2018) on 11 December 2019.
  + appears to only have primary and secondary class levels for locations we are training with

+ Inhouse-developed proxy for grazing pressure using GPP and rainfall
  + m1b_residual or better
  + at site or at non-woody veg pixel nearby, or average in 500m buffer

+ Gross Primary Productivity (GPP) of nearby cleared pixel __NeedResearch__
  + median across all years
  + median difference of min GPP and max GPP across years
  + difference to mean at each time (correlated with m1b_residual)
  + summer GPP (or GPP residuals) and winter GPP residuals.
     + Summer will pick up native grass (and any trees) and lucerne.
     + Spring will pick up everything else.
   + measures farming practise only
   + detecting non-forested pixels might be hard
     + might have to reprojected and resample everything to the NCAS data

### Ground Observations
+ midstorey veg cover
+ overstory veg cover
+ Noisy Miners

### Other

+ Fuel Moisture Content (FMC)

+ Rainfall (mean annual rainfall and the difference to the last 12 months of actual rainfall)
  + average rainfall useful for frogs and interacts with continuous vs rotational grazing [https://doi.org/10.1016/j.agee.2019.05.003]
  + wetter years increase amount of smaller birds and decrease amount of larger birds [https://doi.org/10.1111/gcb.14524]. *May be mediated through GPP*
  + [https://doi.org/10.1111/ddi.12874] used the last 12 months of rainfall
  + Found after high rainfall for nomadic species in old growth woodland, that the presence halved in climatically dry sites but more than doubled in climatically wet sites [https://doi.org/10.1111/ddi.12874]. Suggests including interaction with TWI, rainfall, mean annual rainfall, GrowthType and more.
  + There could be a better time frame than 12 months __NeedResearch__

+ Maximum Temperature: mean annual maximum temperature and maximum temperature of last 12 months [https://doi.org/10.1111/ddi.12874] __UnknownAccessibility__

+ Distance to water. For frogs [https://doi.org/10.1016/j.agee.2019.05.003]
  + maps of dams could be pretty hard to get __MaybeUnobtainable__ __UnknownAccessibility__

+ Predictors that use spectral heterogeneity in the spirit of the 'Spectral Variation Hypothesis': 
  + __suggest one of these__
      + NVDA say, because 25m resolution would be better for this than the 250m resolution of GPP.
      + Could be highly correlated with GrowthType
  +  [these will make it easier for a few academics to compare the quality of their methods to ours]
  + spatial heterogeneity of NDVI using 25m resolution [Use Rao's Q index?]
  + spatial heterogeneity of GPP. [Use Rao's Q index?]
  + spatial heterogeneity of raw spectral data

+ distance to large nature reserves 
  + can start with very low resolution calculation of this. If it proves important then increase the resolution (could do it quick by linearly interpolating the distance map will work quite well)
  + There would be literature and experience to inform this. Martin probably already knows. __NeedsResearch__

+ last fire (if there was one?) __UnknownAccessibility__
  + impacts paddock tree populations [https://doi.org/10.1111/aec.12414]  (so may be important if fires have come through since last high resolution map)
  + with a fire:
    + consolidated woody vegetation increased by 2.3% from 2005 to 2011, within observation plots affected by wildfire [https://doi.org/10.1111/aec.12414]
    + scattered tree population  decreased by 20%  (paddock trees?)
  + with no fire:
    + scattered trees increased by 5.3%
    + consolidated woody vegetation increased by 22.5%.

+ Longitude and latitude
    + shape of dependence? (first-order, second-order terms?)
    + mean hours of daylight instead of latitude?
    + mean annual rainfall and elevataion instead of longitude?

+ broad climate classes

### Things that will probably need proxies, extra modelling or ignoring

#### Known Proxy / Method to Estimate
+ Leaf litter is randomly distributed about a mean given by native vegetation cover  (and an intercept) [https://doi.org/10.1007/s10980-015-0193-5]
  + It is important [https://doi.org/10.1016/j.biocon.2009.07.009]

+ Canopy depth
  + mean predicted by TWI + Lithology Fertility + Aspect + 1 [https://doi.org/10.1007/s10980-015-0193-5]


#### Unknown how to estimate or proxy

##### Unrelated to Fencing
+ time since underplanting / patch expansion both using tube stock (e.g. positive effects were often 6 - 8 years time lagged in [https://doi.org/10.1111/aec.12622])
  + quadratic interaction effects between survey year and time since enhancement (see appendix of [https://doi.org/10.1111/aec.12622]) for the model used.
  + estimated impact of time since enhancement is substantial (e.g. 0.05 at 0 years to 0.5 after 14 years) [https://doi.org/10.1111/aec.12622]. However 95% credible intervals for these estimates were very large.
  + only three species responded to the increase in vegetation cover within the study period [https://doi.org/10.1111/ddi.12444]

+ Eucalypt die-back [https://doi.org/10.1016/j.biocon.2009.07.009]
  + Could use time-series of woody veg and classify locations into increasing/declining/stable 

+ Surrounding landuse (cropping, annual pasture, native annual, exotic, native pasture). Different species respond to these things [https://doi.org/10.1016/j.biocon.2009.07.009]. *What is 'annual pasture', and 'native annual'?*
  + pasture ==> grazed, or land for grazing
  + cropping vs grazing vs mixed?

+ Windiness (from BOM)?
  + Wind is important in detection. See 2020 paper by [Lindenmayer et al.](https://www-sciencedirect-com.virtual.anu.edu.au/science/article/pii/S0006320719302976)
  +  Average windiness could be correlated to TWI.

+ rotational vs continuous grazing __MaybeUnobtainable__
  + matters to frogs [https://doi.org/10.1016/j.agee.2019.05.003]

##### Related to Fencing
+ Fencing [https://doi.org/10.1016/j.biocon.2012.02.026]
+ Predictions of Noisy Miner (use as a predictor)
  +  the main driver of bird distribution patterns, affecting 65% of the studied species, including 10 species of conservation concern. [https://doi.org/10.1111/ddi.12444]
  + influence on other birds interacts with the average TWI in a 500m buffer (a proxy for productivity of the site) [https://doi.org/10.1007/s10980-011-9665-4]

+ cover of native grass tussocks, moss, lichen, and scarcity of annual grasses
  + Amount of leaf litter, the cover of native grass tussocks, moss and lichen cover, and a scarcity of annual grasses, were important. [https://doi.org/10.1016/j.biocon.2009.07.009]
  + Native tussocks; exotic tussocks [https://doi.org/10.1111/rec.12676]. Modelled (transformed) using a zero-inflated beta distribution.

+ Mistletoe proxies [https://doi.org/10.1016/j.biocon.2009.07.009, 10.1371/journal.pone.0097029]
  + Many species were more likely to occupy patches with abundant mistletoe
  + Could be well summarised by fenced-off woody veg

+ Uncontrolled grazing, controlled grazing or no grazing of the woody patch was found to be very important [https://doi.org/10.1111/rec.12676]
  + could relate to fencing if each site recorded controlled grazing vs no grazing at all


+ Understory
  + Dead trees, logs, mid-sized trees, canopy depth are related to some information that can be remote sensed [https://doi.org/10.1007/s10980-015-0193-5]
  + Mid-storey used in [https://doi.org/10.1111/rec.12676]. Modelled (transformed) using a beta distribution (why?).

+ Hollow-bearing trees. They were significantly important to bird communities in [https://doi.org/10.1371/journal.pone.0097029]. 
  + Can be predicted somewhat by Regrowth + fencing + aspect x regrowth + cropland + aspect x cropland + aspect (quai-poisson distribution about this mean). [https://doi.org/10.1007/s10980-015-0193-5] *is non-linear/gam required for this?*


## For now, do not include
+ Broad-scale landscape texture (categorised possibly non-quantitatively)
  + useful if branching into predictions outside agricultural regions [https://doi.org/10.1111/j.1472-4642.2007.00411.x]
  + suspect all the other predictors above will incorporate this idea of landscape texture within farmlands

+ Whether site is at an intersection of planting strips
  + Used by this paper [https://doi.org/10.1007/s10980-007-9156-9]
  + seems very hard to quantify automatically and will be highly correlated with the area of woody veg within 500m


+ Percent native vegetation cover within 100 ha [https://doi.org/10.1007/s10980-015-0193-5]
  + Use the PCT map?
  + Won't use as HIGHLY correlated with tree cover __check__

+ Average woody cover within a 500m radius
  + Developed by Albert. It uses the NCAS map but excludes less than 20% canopy. There are other differences too.
  + Won't use because the NCAS map appears to have smoother transitions over time than this map (particularly around the year 2011 I think).
Percent native vegetation cover within 100 ha [https://doi.org/10.1007/s10980-015-0193-5]

+ Average patch size in a neighbourhood/numbers of patches in a neighbourhood was not important in one study [https://doi.org/10.1111/ddi.12145]


+ Summer reflectance by Steve Prado (?) for summer grass information  (only confounding crops would be lucen)
  + idea captured by using summer GPP, so won't follow up the idea more broadly.

+ NDVI (but highly correlated to with GPP)
   + would be good to include later from a scientific perspective: would NVDI do just as well as GPP?

+ Commodities map closely related to the CLUM __MaybeLater__
  + details available [here](https://www.agriculture.gov.au/abares/aclump/land-use/catchment-scale-land-use-of-australia-commodities-update-december-2018)
  + a lot of our study region appears to have no classification in the broad commodity type map provided

+ LIDAR __UnknownAccessibility__ __HeavyProcessing__ __MaybeLater__
  + there is a 5m resolution national coverage DEM derived from lidar by Geoscience Australia [here](http://gaservices.ga.gov.au/site_9/rest/services/DEM_LiDAR_5m/MapServer)
+ RADAR
  + already used in the ALOS woody biomass map
  + __UnknownAccessibility__ __HeavyProcessing__ __MaybeLater__

## Included in Late 2019 Model m3_0
+ Date
+ Gross Primary Productivity (GPP) at site
  + mean across all years
  + difference to mean at each time
+ Fuel Moisture Content (FMC)
+ Woody cover within a 500m radius of each site (as a proportion of area of disc with 500m radius)
  + The woody cover map used was developed by Albert and estimate the canopy density, rather than presence/absence. It uses the NCAS map I believe but does a lot more.
  + 500m radius was used in other studies [https://doi.org/10.1016/j.biocon.2009.07.009]

## Included in Early 2020 Model m4_1
+ Date
+ Gross Primary Productivity (GPP) at site
  + mean across all years
+ Woody cover within a 500m radius of each site (as a proportion of area of disc with 500m radius)
  + The woody cover map used was developed by Albert. It used the NCAS map but excludes less than 20% canopy. There are other differences too.
  + 500m radius was used in other studies [https://doi.org/10.1016/j.biocon.2009.07.009]
+ An inhouse-developed proxy for grazing pressure using GPP and rainfall 'm1b_resid'

