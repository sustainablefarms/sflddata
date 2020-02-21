# Predictors that We Might Consider Investigating

"Much of the variation (>70%) in bird responses was explained by 3 factors: remnant native‐vegetation attributes (native grassland, scattered paddock trees, patches of remnant native woodland); presence or absence of planted native trees; and the size and shape of tree plantings. " [https://doi.org/10.1111/j.1523-1739.2008.00924.x]

## Planned to Include in First Modelling Investigation
+ Gross Primary Productivity (GPP)
+ Fuel Moisture Content (FMC)
+ Fraction of tree cover within a 500m buffer of each site 
  + using the NCAS forest map (Furby?)
  + centred on the 100m point of a each transect [https://doi.org/10.1098/rspb.2019.0114]
+ Daily precipitation (PG)
+ Average woody cover within a 500m radius
  + Developed by Albert. It uses the NCAS map but excludes less than 20% canopy. There are other differences too.
  + 500m radius is used in other studies [https://doi.org/10.1016/j.biocon.2009.07.009]
 
+ An inhouse-developed proxy for grazing pressure using GPP and rainfall

## Predictor Ideas that May Be Useful in the Future 

### Geology/Elevation
+ Terrain Wetness Index (TWI) (or similar)
  + An index that is time-invariant.
  + Has more importance in drier climates
  + Has an impact on habitat structure [https://doi.org/10.1007/s10980-015-0193-5]
+ Elevation (highly correlated with TWI in [https://doi.org/10.1007/s10980-015-0193-5])
+ Aspect (based on DEM). Useful for habitat structure [https://doi.org/10.1007/s10980-015-0193-5]
+ Geodiversity (not soil maps though - as spatial variation of soil is very high). Mentioned by David L.
+ Lithology fertility [https://doi.org/10.1007/s10980-015-0193-5]
  + Natural soil fertility is the same thing I think - used here [https://doi.org/10.1111/aec.12414]
+ Presence/absence/type/size of granite inselbergs (matters to lizards [https://doi.org/10.1111/j.1442-9993.2009.02092.x]

### Vegetation Type (at point)
+ Type of forest (remnant, regrowth, planting).
  + Initially could use the on-ground data, but later might be able to use landsat to detect anything younger than the 1980s. Papers from SustainableFarms suggest that remnant vs regrowth (vs planting?) has an effect, but that it is mixed depending on species, and on whether site occupancy, site persistence or site colonisation.[https://doi.org/10.1016/j.biocon.2019.05.015]
  + Age of planting/regrowth?
  + plantings are not a refuge from Noisy Miners. Much fewer bird species live in plantings than old growth [https://doi.org/10.1111/ddi.12444]

+ Plant Community Type. 25m GSD. Updated at different dates for different regions.
   + [https://doi.org/10.1016/j.biocon.2012.02.026] found Floodplain Transition Woodland, Inland Floodplain Woodland, Riverine Plain Woodland, Reverine Sandhill Woodland has impacts on bird assemblage.

+ transitional stock route region (TSR) [https://doi.org/10.1016/j.biocon.2012.02.026]


### Vegetation Cover and Configuration
+ NDVI (however highly correlated to with GPP)
+ Percent native vegetation cover within 100 ha [https://doi.org/10.1007/s10980-015-0193-5]
+ Percent cover types within --- metres:
  + Photosynthetic Vegetation (PV)
  + Non-Photosynthetic Vegetation (NPV)
  + Bare Soil (BS) 
  + Availabiity:
     + all of the above at 500m resolution derived from MODIS. See https://www.agriculture.gov.au/abares/aclump/land-cover/ground-cover-monitoring-for-australia]  
     + Also at 25m resolution through Geoscience Australia's Digital Earth Australia: Fractional Cover (percentages of Bare Soil, Photosynthetic Vegetation and Non-Photosynthetic Vegetation) The derivation of PV/NPV/BS is described [here](https://d28rz98at9flks.cloudfront.net/79676/Fractional_Cover_FC25_v1_5.PDF
and [here](http://data.auscover.org.au/xwiki/bin/view/Product+pages/Landsat+Seasonal+Fractional+Cover) 
and the underlying science [here](https://www.sciencedirect.com/science/article/pii/S0034425715000395) 
  + Percent woody cover within --- metres  (derived from NCAS forest map which has a resolution of 25m)

+ Area of woody cover in 1000ha and 10000ha resolution too 
  + suggested by https://doi.org/10.1111/ddi.12145, with some confusion on whether it is native vegetation or not - predominantly it is native
  + [https://doi.org/10.1371/journal.pone.0097029] similar but used the term 'woody vegetation' consistently

+ Landsat derived fractional cover index (Foliage Projection Cover) from the OEH (now in the department of planning, industry and environment).

+ Area of native grass within 500m of a site (see https://doi.org/10.1016/j.biocon.2009.07.009). 
  + This could be possible to determine using summer indications of bare ground, and woody veg. 
  + Will need a way to ignore lucerne though 

+ ALOS Woody Biomass (derived from RADAR). 50m GSD. Static (year of 2009). Related paper suggests it may not include understory.


## Spatial Configuration of Vegetation Cover
+ Number of paddock trees within 500m [https://doi.org/10.1016/j.biocon.2009.07.009]
  + paddock tree populations are declining with time, but mostly due to fire? [https://doi.org/10.1111/aec.12414]
  + mostly canopy is 10m - 25m in diameter
  + these trees generally contain features that take centuries to develop naturally [https://doi.org/10.1111/aec.12414]

+ Generic configuration indices
  + [Suggest low priority. Would be interesting to test as they are related to my PhD and I can easily calculate them. But a good chance they won't improve biodiversity predictions much]
  + (spatial) covariance estimated from a --- metre region
  + 'contact distance' estimated from a --- metre region
  + Patch size - used in [https://doi.org/10.1016/j.biocon.2009.07.009].  (could require a lot of work processing though)
  + Average patch size in a neighbourhood/numbers of patches in a neighbourhood was not important in one study [https://doi.org/10.1111/ddi.12145]

+ width of planting has an effect on birds [https://doi.org/10.1111/rec.12676]. But a frog paper found it didn't matter much to frogs [https://doi.org/10.1016/j.agee.2019.05.003].
  + [https://doi.org/10.1007/s10531-016-1140-8] found it only had an effect on plantings, not old growth (or natural regrowth too i guess)


 
### Land Use
+ % Cropland within 100 ha (using NSW's 2011 Land use map) [https://doi.org/10.1007/s10980-015-0193-5] 
+ Catchment scale land Use and Management Classes (CLUM)
  + December 2018 update (updates to Burdekin in QLD, NSW, Vic, WA)
  + Classes given by Australian Land Use and Management (ALUM) Classification version 8
  + data available [here](https://www.agriculture.gov.au/abares/aclump/land-use/catchment-scale-land-use-of-australia-update-december-2018) on 11 December 2019.
  + appears to only have primary and secondary class levels for locations we are training with
+ Commodities map closely related to the CLUM
  + details available (here)[https://www.agriculture.gov.au/abares/aclump/land-use/catchment-scale-land-use-of-australia-commodities-update-december-2018]
  + a lot of our study region appears to have no classification in the broad commodity type map provided
+ rotational vs continuous grazing
  + matters to frogs [https://doi.org/10.1016/j.agee.2019.05.003]


### Other
+ Rainfall (climatalogical summary, e.g. mean annual rainfall; last 12 months actual rainfall)
  + average rainfall useful for frogs and interacts with continuous vs rotational grazing [https://doi.org/10.1016/j.agee.2019.05.003]
  + wetter years increase amount of smaller birds and decrease amount of larger birds [https://doi.org/10.1111/gcb.14524]. *May be mediated through GPP*
  + [https://doi.org/10.1111/ddi.12874] used the last 12 months of rainfall
  + Found after high rainfall for nomadic species in old growth woodland, that the presence halved in climatically dry sites but more than doubled in climatically wet sites [https://doi.org/10.1111/ddi.12874]. Suggests including interaction with TWI, rainfall, mean annual rainfall, GrowthType and more.
+ Maximum Temperature: mean annual maximum temperature and maximum temperature of last 12 months [https://doi.org/10.1111/ddi.12874]
+ Distance to water. For frogs [https://doi.org/10.1016/j.agee.2019.05.003]
+ Predictors that use spectral heterogeneity in the spirit of the 'Spectral Variation Hypothesis':
  +  [these will make it easier for a few academics to compare the quality of their methods to ours]
  + spatial heterogeneity of NDVI using 25m resolution [Use Rao's Q index?]
  + spatial heterogeneity of GPP. [Use Rao's Q index?]
  + spatial heterogeneity of raw spectral data
+ Multiple time points:
  + trends in cover
  + seasonal variation of GPP (?)
  + summer GPP (or GPP residuals) and winter GPP residuals.
    + Summer will pick up native veg and lucen.
    + Spring will pick up everything else.
+ LIDAR
+ RADAR
+ Consider separating GPP + FMC of cropping from that of forests
+ Summer reflectance by Steve Prado (?) for summer grass information  (only confounding crops would be lucen)
+ Land Capability



### Things that will probably need proxies, extra modelling or ignoring
+ time since underplanting / patch expansion both using tube stock (e.g. positive effects were often 6 - 8 years time lagged in [https://doi.org/10.1111/aec.12622])
  + quadratic interaction effects between survey year and time since enhancement (see appendix of [https://doi.org/10.1111/aec.12622]) for the model used.
  + estimated impact of time since enhancement is substantial (e.g. 0.05 at 0 years to 0.5 after 14 years) [https://doi.org/10.1111/aec.12622]. However 95% credible intervals for these estimates were very large.
  + only three species responded to the increase in vegetation cover within the study period [https://doi.org/10.1111/ddi.12444]

+ Predictions of Noisy Miner (use as a predictor)
  +  the main driver of bird distribution patterns, affecting 65% of the studied species, including 10 species of conservation concern. [https://doi.org/10.1111/ddi.12444]
+ ground layer and understorey attributes
  + Amount of leaf litter, the cover of native grass tussocks, moss and lichen cover, and a scarcity of annual grasses, were important. [https://doi.org/10.1016/j.biocon.2009.07.009]
  + Leaf litter is randomly distributed about a mean given by native vegetation cover  (and an intercept) [https://doi.org/10.1007/s10980-015-0193-5]

+ Mistletoe proxies [https://doi.org/10.1016/j.biocon.2009.07.009, 10.1371/journal.pone.0097029]
  + Many species were more likely to occupy patches with abundant mistletoe
  + Could be well summarised by fenced-off woody veg

+ Eucalypt die-back [https://doi.org/10.1016/j.biocon.2009.07.009]
  + Could use time-series of woody veg and classify locations into increasing/declining/stable 

+ Canopy depth
  + mean predicted by TWI + Lithology Fertility + Aspect + 1 [https://doi.org/10.1007/s10980-015-0193-5]

+ Surrounding landuse (cropping, annual pasture, native annual, exotic, native pasture). Different species respond to these things [https://doi.org/10.1016/j.biocon.2009.07.009]. *What is 'annual pasture', and 'native annual'?*
  + pasture ==> grazed, or land for grazing
  + cropping vs grazing vs mixed?

+ Fencing [https://doi.org/10.1016/j.biocon.2012.02.026]
+ Uncontrolled grazing, controlled grazing or no grazing of the woody patch was found to be very important [https://doi.org/10.1111/rec.12676]
  + could relate to fencing if each site recorded controlled grazing vs no grazing at all


+ Understory
  + Dead trees, logs, mid-sized trees, canopy depth are related to some information that can be remote sensed [https://doi.org/10.1007/s10980-015-0193-5]

+ Mid-storey used in [https://doi.org/10.1111/rec.12676]. Modelled (transformed) using a beta distribution (why?).
+ Native tussocks; exotic tussocks [https://doi.org/10.1111/rec.12676]. Modelled (transformed) using a zero-inflated beta distribution.

+ Windiness (from BOM)? Wind is important in detection.
  +  Average windiness could be correlated to TWI.

+ Hollow-bearing trees. They were significantly important to bird communities in [https://doi.org/10.1371/journal.pone.0097029]. 
  + Can be predicted somewhat by Regrowth + fencing + aspect x regrowth + cropland + aspect x cropland + aspect (quai-poisson distribution about this mean). [https://doi.org/10.1007/s10980-015-0193-5] *is non-linear/gam required for this?*

+ last fire (if there was one?) 
  + impacts paddock tree populations [https://doi.org/10.1111/aec.12414]  (so may be important if fires have come through since last high resolution map)
  + with a fire:
    + consolidated woody vegetation increased by 2.3% from 2005 to 2011, within observation plots affected by wildfire [https://doi.org/10.1111/aec.12414]
    + scattered tree population  decreased by 20%  (paddock trees?)
  + with no fire:
    + scattered trees increased by 5.3%
    + consolidated woody vegetation increased by 22.5%.

## Not Include
+ Broad-scale landscape texture (categorised possibly non-quantitatively)
  + useful if branching into predictions outside agricultural regions [https://doi.org/10.1111/j.1472-4642.2007.00411.x]
  + suspect all the other predictors above will incorporate this idea of landscape texture within farmlands

+ Whether site is at an intersection of planting strips
  + Used by this paper [https://doi.org/10.1007/s10980-007-9156-9]
  + seems very hard to quantify automatically and will be highly correlated with the area of woody veg within 500m


