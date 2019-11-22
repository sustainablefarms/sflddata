# Predictors that We Might Consider Investigating
## Planned to Include in First Modelling Investigation
+ GPP
+ Fraction of tree cover within a 500m buffer of each site
+ An inhouse-developed proxy for grazing pressure using GPP and rainfall
+ We have a few more already - can't remember which ones sorry

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
  + [all of the above at 500m resolution derived from MODIS. See https://www.agriculture.gov.au/abares/aclump/land-cover/ground-cover-monitoring-for-australia]  
  + Percent woody cover within --- metres  (derived from NCAS forest map which has a resolution of 25m)
+ spatial configuration of tree canopy (using national carbon accounting scheme's 25m forest maps):
  + [Suggest low priority. Would be interesting to test as they are related to my PhD and I can easily calculate them. But a good chance they won't improve biodiversity predictions much]
  + (spatial) covariance estimated from a --- metre region
  + 'contact distance' estimated from a --- metre region
+ Multiple time points:
  + trends in cover
  + seasonal variation of GPP (?)

