# On Spatial Correlation of Bird Presence
<!-- vim-markdown-toc GFM -->

* [Types of Spatial Dependence](#types-of-spatial-dependence)
* [Hypothetical Scenarios](#hypothetical-scenarios)
* [Conclusions](#conclusions)

<!-- vim-markdown-toc -->

*These musing are on effects of presence of birds on other birds some distance away (not at the same site).*

## Types of Spatial Dependence
__N__ None

__I__ Spatially varying intensity

__C__ Spatial clustering or repulsion


Spatially varying intensity either (a) depends on longitude or latitude explicitly, or (b) depends on environmental factors like rainfall, hours of sunlight, elevation, soil etc (with longitude and latitude associated with these).
My feeling is that option (b) creates a model that is better at generalising to larger regions.


## Hypothetical Scenarios
__C__ There is great habitat for lizard A at location X, but the nearest lizard of species A is 10000km away, so this great habitat is unused.

__C__ Noisy Miners have been removed in a 10000km radius so a new shelterbelt does not host any Noisy Miners for the next few years.

__C__ The cane toad is 100km west of here, so we would expect cane toads to be here soon.

__C__ Bird A likes to be far away from bird B

__I__ Cane toads are unlikely to be here because it is too dry.

__I__ Bird A likes climate X

__C__ Bird A will roost only when there are many other Bird A entities roosting within 10km

__I__ Bird A will only roost where there is lots of good roosting habitat within 10km

__I__ If one finds Bird A at location X, there is more chance of finding other cases of Bird A at locations near X, and it is purely due to environmental factors.
  *very much an equilibrium model of presence/absence*

__C__ If one finds Bird A at location X, there is more chance of finding other cases of Bird A at locations near X, and this is partly due to (spatially broad) communities of birds that die completely randomly, unrelated to environmental factors.

__C__ There are three nearby patches that host birds of species A. All patches are very poor. Each patch is frequently colonised by birds from the other two patches. The chance that all three patches lose all their birds of species A at once is low, but could eventually happen. The chance of a bird of species A existing in the first patch thus depends on the birds existing in either of the other two patches.

__N__ Bird A doesn't want to be at exactly the same location as bird B.

## Conclusions
__Spatially Varying Intensity:__
We are definitely encountering spatially varying intensity. The amount that it depends directly on longitude and latitude could indicate missing environmental predictors.

__Clustering / Repulsion:__
I don't recall any papers by Sustainable Farms that found spatial clustering, nor anything on landscape-scale extinction clustering. 
Invasive species (e.g. cane toad or disease) could absolutely result in spatial clustering. However, these also weren't prominent in the Sustainable Farms papers. 
Noisy Miners are now everywhere that they want to be, so Noisy Miner colonisation effects only applies if Noisy Miners are culled or eradicated from a large region.

Spatial clustering feels like a small effect compared to the spatially varying intensity, and we may also have insufficient data to assess it. 
It is usually not possible to distinguish between unexplained spatially-varying intensity and spatial correlation/clustering. 
When/if there is spatial correlation in residuals from our models, this will be a good time to consider if it is missing/misspecified environmental predictors (i.e. varying intensity) or clustering.

