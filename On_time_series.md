# Model Responses as Time Series?

The occupancy of a species at a location is a time series: current occupancy can be predicted well using past occupancy, except for major disturbances.
Similarly for detection, albeit with more noise.

Technically our project is interested in the multivariate spatiotemporal process of occupancy.

## Good Reasons to use Time Series
* Data generating process is a time series
* Occupancy would be predicted very well by past occupancy
* Can model changes in occupancy
* Doesn't assume occupancy is at equilibrium
* Dependence on environmental changes might take years to be seen (e.g. 6+ years for a planting)

## Good Reasons to not use a Time Series
* Standard time series methods would not predict occupancy at locations that aren't visited (they may be able to predict change in occupancy though)
* Consistent with assuming occupancy is stable

## Conclusions (so far)
Being able to predict occupancy at locations that aren't visited is crucial. However, avoiding the assumption of equilibrium occupancy, would also be fabulous.

*Are there any time-series methods that also predict the initial starting values of a time series?*


