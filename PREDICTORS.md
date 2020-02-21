# Predictors that We Might Consider Investigating

*"Much of the variation (>70%) in bird responses was explained by 3 factors: remnant native‐vegetation attributes (native grassland, scattered paddock trees, patches of remnant native woodland); presence or absence of planted native trees; and the size and shape of tree plantings."* [https://doi.org/10.1111/j.1523-1739.2008.00924.x]

## Included in Late 2019 Model m3_0
+ Gross Primary Productivity (GPP)
  + mean across all years
  + difference to mean at each time
+ Fuel Moisture Content (FMC)
+ Average woody cover within a 500m radius of each site
  + Developed by Albert. It uses the NCAS map but excludes less than 20% canopy. There are other differences too.
  + 500m radius is used in other studies [https://doi.org/10.1016/j.biocon.2009.07.009]

## Included in Early 2020 Model m4_1
+ Gross Primary Productivity (GPP)
  + mean across all years
+ Average woody cover within a 500m radius of each site
  + Developed by Albert. It uses the NCAS map but excludes less than 20% canopy. There are other differences too.
  + 500m radius is used in other studies [https://doi.org/10.1016/j.biocon.2009.07.009]
+ An inhouse-developed proxy for grazing pressure using GPP and rainfall 'm1b_resid'


## Consider Including 
 
### Geology/Elevation
+ Terrain Wetness Index (TWI) (or similar)
  + An index that is time-invariant.
  + Has more importance in drier climates
  + Has an impact on habitat structure [https://doi.org/10.1007/s10980-015-0193-5]
  + average TWI in a 500m buffer (a proxy for productivity of the site) [https://doi.org/10.1007/s10980-011-9665-4]   (interacts with Noisy Miner)
+ Elevation (highly correlated with TWI in [https://doi.org/10.1007/s10980-015-0193-5])
+ Aspect (based on DEM). Useful for habitat structure [https://doi.org/10.1007/s10980-015-0193-5]
+ Geodiversity (not soil maps though - as spatial variation of soil is very high). Mentioned by David L. __UnknownLocation__
+ Lithology fertility [https://doi.org/10.1007/s10980-015-0193-5] __UnknownLocation__
  + Natural soil fertility is the same thing I think - used here [https://doi.org/10.1111/aec.12414]
+ Presence/absence/type/size of granite inselbergs (matters to lizards [https://doi.org/10.1111/j.1442-9993.2009.02092.x] __UnknownLocation__

### Vegetation Type (at point)
+ Type of forest (remnant, regrowth, planting).
  + Initially could use the on-ground data, but later might be able to use landsat to detect anything younger than the 1980s. Papers from SustainableFarms suggest that remnant vs regrowth (vs planting?) has an effect, but that it is mixed depending on species, and on whether site occupancy, site persistence or site colonisation.[https://doi.org/10.1016/j.biocon.2019.05.015]
  + Age of planting/regrowth? __HeavyProcessing__
  + plantings are not a refuge from Noisy Miners. Much fewer bird species live in plantings than old growth [https://doi.org/10.1111/ddi.12444]

+ Plant Community Type (PCT). 25m GSD. Updated at different dates for different regions.
  + [https://doi.org/10.1016/j.biocon.2012.02.026] found Floodplain Transition Woodland, Inland Floodplain Woodland, Riverine Plain Woodland, Reverine Sandhill Woodland has impacts on bird assemblage.
  + Available [here](https://www.environment.nsw.gov.au/vegetation/state-vegetation-type-map.htm)

+ transitional stock route region (TSR) [https://doi.org/10.1016/j.biocon.2012.02.026]
__UnknownLocation__ 

### Vegetation Cover and Configuration
+ Fraction of tree cover within a 500m buffer of each site 
  + using the NCAS forest map (Furby?)
  + possibly centred on the 100m point of a each transect [https://doi.org/10.1098/rspb.2019.0114]
  + 500m radius is used in other studies [https://doi.org/10.1016/j.biocon.2009.07.009]
+ Area of tree cover in 1000ha and 10000ha resolution too 
  + suggested by [https://doi.org/10.1111/ddi.12145], with some confusion on whether it is native vegetation or not - predominantly it is native
  + [https://doi.org/10.1371/journal.pone.0097029] similar but used the term 'woody vegetation' consistently

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

+ GPP within 500m buffer
   + Consider separating GPP + FMC of cropping from that of woody veg
   + __NeedResearch__
     + what versions of GPP shall be used?

+ NDVI (but highly correlated to with GPP)

+ Percent PV, NVP and bare soild within 500 metres:
  + Photosynthetic Vegetation (PV)
  + Non-Photosynthetic Vegetation (NPV)
  + Bare Soil (BS) 
  + Availabiity:
     + all of the above at 500m resolution derived from MODIS. See [https://www.agriculture.gov.au/abares/aclump/land-cover/ground-cover-monitoring-for-australia]  
     + Also at 25m resolution through Geoscience Australia's Digital Earth Australia: Fractional Cover (percentages of Bare Soil, Photosynthetic Vegetation and Non-Photosynthetic Vegetation) The derivation of PV/NPV/BS is described [here](https://d28rz98at9flks.cloudfront.net/79676/Fractional_Cover_FC25_v1_5.PDF
and [here](http://data.auscover.org.au/xwiki/bin/view/Product+pages/Landsat+Seasonal+Fractional+Cover) 
and the underlying science [here](https://www.sciencedirect.com/science/article/pii/S0034425715000395) 


+ Landsat-derived fractional cover index (Foliage Projection Cover) from the OEH (now in the department of planning, industry and environment).
  + mean of a 500m buffer
  + __NeedResearch__ (could be highly correlated to tree cover etc)

+ Area of native grass within 500m of a site (see https://doi.org/10.1016/j.biocon.2009.07.009).  __NeedResearch__
  + This could be possible to determine using summer indications of bare ground, and woody veg. 
  + Will need a way to ignore lucerne though 
  + Might be available in the PCT map

+ ALOS Woody Biomass (derived from RADAR). 50m GSD. Static (year of 2009). Related paper suggests it may not include understory.



## Spatial Configuration of Vegetation Cover
+ Number of paddock trees within 500m [https://doi.org/10.1016/j.biocon.2009.07.009]
  + paddock tree populations are declining with time, but mostly due to fire? [https://doi.org/10.1111/aec.12414]
  + mostly canopy is 10m - 25m in diameter
  + these trees generally contain features that take centuries to develop naturally [https://doi.org/10.1111/aec.12414]
  + __NeedResearch__ __HeavyProcessing__
  + get paddock trees by 
     + thresholding tree canopy patches to below 40mx40m (say)
     + paddock trees at time of the 2011 5m GSD NSW tree canopy map [here](http://data.auscover.org.au/xwiki/bin/view/Product+pages/nsw+5m+woody+extent+and+fpc)  could give a good approximation
     + Tree canopy maps derived more recently from SPOT data would be really nice
     + age from historical maps or assume very old __NeedResearch__

+ Patch size containing site - used in [https://doi.org/10.1016/j.biocon.2009.07.009].  (could require a lot of work processing though) __HeavyProcessing__

+ width of planting has an effect on birds [https://doi.org/10.1111/rec.12676]. But a frog paper found it didn't matter much to frogs [https://doi.org/10.1016/j.agee.2019.05.003].
  + [https://doi.org/10.1007/s10531-016-1140-8] found it only had an effect on plantings, not old growth (or natural regrowth too i guess)
  + __NeedResearch__ __HeavyProcessing__
     + how to teach a computer what the 'width' of a remnant is

+ Configuration indices based on statistical models of landscape generation
  + (spatial) covariance estimated from a --- metre region
  + mean of 'contact distance' estimated from a --- metre region
  + Suggest low priority. Would be interesting to test as they are related to my PhD and I can easily calculate them. But a good chance they won't improve biodiversity predictions much
  + __NeedResearch__ __HeavyProcessing__


### Land Use
+ % Cropland within 100 ha (using NSW's 2011 Land use map) or the CLUM [https://doi.org/10.1007/s10980-015-0193-5] 
+ Catchment scale land Use and Management Classes (CLUM)
  + December 2018 update (updates to Burdekin in QLD, NSW, Vic, WA)
  + Classes given by Australian Land Use and Management (ALUM) Classification version 8
  + data available [here](https://www.agriculture.gov.au/abares/aclump/land-use/catchment-scale-land-use-of-australia-update-december-2018) on 11 December 2019.
  + appears to only have primary and secondary class levels for locations we are training with
+ Commodities map closely related to the CLUM __MaybeLater__
  + details available [here](https://www.agriculture.gov.au/abares/aclump/land-use/catchment-scale-land-use-of-australia-commodities-update-december-2018)
  + a lot of our study region appears to have no classification in the broad commodity type map provided
+ m1b_residual - inhouse-developed proxy for grazing pressure using GPP and rainfall
+ rotational vs continuous grazing __MaybeUnobtainable__
  + matters to frogs [https://doi.org/10.1016/j.agee.2019.05.003]


### Other

+ Fuel Moisture Content (FMC)

+ Rainfall (climatalogical summary, e.g. mean annual rainfall; last 12 months actual rainfall)
  + average rainfall useful for frogs and interacts with continuous vs rotational grazing [https://doi.org/10.1016/j.agee.2019.05.003]
  + wetter years increase amount of smaller birds and decrease amount of larger birds [https://doi.org/10.1111/gcb.14524]. *May be mediated through GPP*
  + [https://doi.org/10.1111/ddi.12874] used the last 12 months of rainfall
  + Found after high rainfall for nomadic species in old growth woodland, that the presence halved in climatically dry sites but more than doubled in climatically wet sites [https://doi.org/10.1111/ddi.12874]. Suggests including interaction with TWI, rainfall, mean annual rainfall, GrowthType and more.

+ Maximum Temperature: mean annual maximum temperature and maximum temperature of last 12 months [https://doi.org/10.1111/ddi.12874] __UnknownLocation__

+ Distance to water. For frogs [https://doi.org/10.1016/j.agee.2019.05.003]
  + maps of dams could be pretty hard to get __MaybeUnobtainable__ __UnknownLocation__

+ Predictors that use spectral heterogeneity in the spirit of the 'Spectral Variation Hypothesis': 
  + __suggest one of these__
      + NVDA say, because 25m resolution would be better for this than the 250m resolution of GPP.
      + Could be highly correlated with GrowthType
  +  [these will make it easier for a few academics to compare the quality of their methods to ours]
  + spatial heterogeneity of NDVI using 25m resolution [Use Rao's Q index?]
  + spatial heterogeneity of GPP. [Use Rao's Q index?]
  + spatial heterogeneity of raw spectral data

+ Multiple time points:
  + rate of cover change (increase, decrease, stable)
+ LIDAR __UnknownLocation__ __HeavyProcessing__ __MaybeLater__
+ RADAR
  + already used in the ALOS woody biomass map
  + __UnknownLocation__ __HeavyProcessing__ __MaybeLater__

+ distance to forest
  + can start with very low resolution calculation of this. If it proves important then increase the resolution (could do it quick by linearly interpolating the distance map will work quite well)

+ Land Capability  __UnknownLocation__

+ last fire (if there was one?) __UnknownLocation__
  + impacts paddock tree populations [https://doi.org/10.1111/aec.12414]  (so may be important if fires have come through since last high resolution map)
  + with a fire:
    + consolidated woody vegetation increased by 2.3% from 2005 to 2011, within observation plots affected by wildfire [https://doi.org/10.1111/aec.12414]
    + scattered tree population  decreased by 20%  (paddock trees?)
  + with no fire:
    + scattered trees increased by 5.3%
    + consolidated woody vegetation increased by 22.5%.


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

+ Windiness (from BOM)? Wind is important in detection.
  +  Average windiness could be correlated to TWI.

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


## Not Include
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
