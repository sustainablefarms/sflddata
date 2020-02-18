# Predictors that We Might Consider Investigating
## Planned to Include in First Modelling Investigation
+ Gross Primary Productivity (GPP)
+ Fuel Moisture Content (FMC)
+ Fraction of tree cover within a 500m buffer of each site using the NCAS forest map
+ Daily precipitation (PG)
+ Average woody cover within a 500m radius
  + Developed by Albert. It uses the NCAS map but excludes less than 20% canopy. There are other differences too.
  + 500m radius is used in other studies [10.1016/j.biocon.2009.07.009]
 
+ An inhouse-developed proxy for grazing pressure using GPP and rainfall

## Predictor Ideas that May Be Useful in the Future 
+ NDVI (however highly correlated to with GPP)
+ Predictors that use spectral heterogeneity in the spirit of the 'Spectral Variation Hypothesis':
  +  [these will make it easier for a few academics to compare the quality of their methods to ours]
  + spatial heterogeneity of NDVI using 25m resolution [Use Rao's Q index?]
  + spatial heterogeneity of GPP. [Use Rao's Q index?]
  + spatial heterogeneity of raw spectral data
+ Type of forest (remnant, regrowth, planting). Different responses [10.1016/j.biocon.2019.05.015]
  + Initially could use the on-ground data, but later might be able to use landsat to detect anything younger than the 1980s. Papers from SustainableFarms suggest that remnant vs regrowth (vs planting?) has an effect, but that it is mixed depending on species, and on whether site occupancy, site persistence or site colonisation.
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
+ spatial configuration of tree canopy (using national carbon accounting scheme's 25m forest maps):
  + [Suggest low priority. Would be interesting to test as they are related to my PhD and I can easily calculate them. But a good chance they won't improve biodiversity predictions much]
  + (spatial) covariance estimated from a --- metre region
  + 'contact distance' estimated from a --- metre region
  + Patch size - used in [10.1016/j.biocon.2009.07.009].  (could require a lot of work processing though)
+ Multiple time points:
  + trends in cover
  + seasonal variation of GPP (?)
  + summer GPP (or GPP residuals) and winter GPP residuals.
    + Summer will pick up native veg and lucen.
    + Spring will pick up everything else.
+ Catchment scale land Use and Management Classes (CLUM)
  + December 2018 update (updates to Burdekin in QLD, NSW, Vic, WA)
  + Classes given by Australian Land Use and Management (ALUM) Classification version 8
  + data available [here](https://www.agriculture.gov.au/abares/aclump/land-use/catchment-scale-land-use-of-australia-update-december-2018) on 11 December 2019.
  + appears to only have primary and secondary class levels for locations we are training with
+ Commodities map closely related to the CLUM
  + details available (here)[https://www.agriculture.gov.au/abares/aclump/land-use/catchment-scale-land-use-of-australia-commodities-update-december-2018]
  + a lot of our study region appears to have no classification in the broad commodity type map provided
+ LIDAR
+ RADAR
+ Consider separating GPP + FMC of cropping from that of forests
+ Terrain Wetness Index (or similar)
  + An index that is time-invariant.
  + Has more importance in drier climates
+ Summer reflectance by Steve Prado (?) for summer grass information  (only confounding crops would be lucen)
+ ALOS Woody Biomass (derived from RADAR). 50m GSD. Static (year of 2009). Related paper suggests it may not include understory.
+ Plant Community Type. 25m GSD. Updated at different dates for different regions.
   + [10.1016/j.biocon.2012.02.026] found Floodplain Transition Woodland, Inland Floodplain Woodland, Riverine Plain Woodland, Reverine Sandhill Woodland has impacts on bird assemblage.
+ Land Capability
+ Geodiversity (not soil maps though - as spatial variation of soil is very high). Mentioned by David L.
+ Elevation
+ Landsat derived fraction cover index (Foliage Projection Cover) from the OEH (now in the department of planning, industry and environment).
+ Area of native grass within 500m of a site (see https://doi.org/10.1016/j.biocon.2009.07.009). 
  + This could be possible to determine using summer indications of bare ground, and woody veg. 
  + Will need a way to ignore lucerne though 
+ transitional stock route region (TSR) [10.1016/j.biocon.2012.02.026]

### Proxies to Compute/Aquire
+ Predictions of Noisy Miner (use as a predictor)
+ ground layer and understorey attributes
  + Amount of leaf litter, the cover of native grass tussocks, moss and lichen cover, and a scarcity of annual grasses, were important. [10.1016/j.biocon.2009.07.009]
+ Mistletoe proxies [10.1016/j.biocon.2009.07.009]
  + Many species were more likely to occupy patches with abundant mistletoe
  + Could be well summarised by fenced-off woody veg
+ Eucalypt die-back [10.1016/j.biocon.2009.07.009]
  + Could use time-series of woody veg and classify locations into increasing/declining/stable 
+ Canopy depth
+ Number of paddock trees within 500m [10.1016/j.biocon.2009.07.009]
+ Surrounding landuse (cropping, annual pasture, native annual, exotic, native pasture). Different species respond to these things [10.1016/j.biocon.2009.07.009]. *What is 'annual pasture', and 'native annual'?*
  + pasture ==> grazed, or land for grazing
  + cropping vs grazing vs mixed?
+ Fencing [10.1016/j.biocon.2012.02.026]
+ Understory
