# Predictors that We Might Consider Investigating
## Planned to Include in First Modelling Investigation
+ Gross Primary Productivity (GPP)
+ Fuel Moisture Content (FMC)
+ Fraction of tree cover within a 500m buffer of each site using the NCAS forest map
+ Daily precipitation (PG)
+ Average woody cover within a 500m radius
  + Developed by Albert. It uses the NCAS map but excludes less than 20% canopy. There are other differences too.
 
+ An inhouse-developed proxy for grazing pressure using GPP and rainfall

## Predictor Ideas that May Be Useful in the Future 
+ NDVI (however highly correlated to with GPP)
+ Predictors that use spectral heterogeneity in the spirit of the 'Spectral Variation Hypothesis':
  +  [these will make it easier for a few academics to compare the quality of their methods to ours]
  + spatial heterogeneity of NDVI using 25m resolution [Use Rao's Q index?]
  + spatial heterogeneity of GPP. [Use Rao's Q index?]
  + spatial heterogeneity of raw spectral data
+ Type of forest (remnant, regrowth, planting). 
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
+ ALOS Woody Biomass (derived from RADAR). 50m GSD. Static (year of 2009)
+ Plant Community Type. 25m GSD. Updated at different dates for different regions.
+ Land Capability
+ Geodiversity (not soil maps though - as spatial variation of soil is very high). Mentioned by David L.
+ Elevation
+ Predictions of Noisy Miner (use as a predictor)

