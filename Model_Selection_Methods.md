# Methods for Model Selection and Model Diagnostics
<!-- vim-markdown-toc GFM -->

* [Model Selection](#model-selection)
	* [Covariate Selection (in preferential order)](#covariate-selection-in-preferential-order)
	* [Numbers of Latent Variables](#numbers-of-latent-variables)
	* [Comparing models (not just nested models) (in preferential order)](#comparing-models-not-just-nested-models-in-preferential-order)
* [Model Diagnostics](#model-diagnostics)
* [References](#references)

<!-- vim-markdown-toc -->


## Model Selection
Kassel is currently feeling like the strategy will be to start with small and simple models, and add complication and covariates. 
This follows the opinion of Gelman, Hwang et al (2014).

### Covariate Selection (in preferential order)

   1. Proposing more covariates: use knowledge from literature and Martin et al
   2. Check proposed covariates (and whether to transform covariate etc): Use Dunn-Smyth detection and occupancy residuals against values of proposed covariate to see if pattern in residuals [Warton et al, 2017]. Useful code could be in Rpresence or Boral
   3. Check covariates are useful: stochastic search variable selection as described in Boral help files sounds excellent (priors combine narrow distribution (for including covariate) or very broad (for not including covariate) ).
   4. Not use: Remove coefficients with wide loading distributions
   5. Not use: Stepwise search powered by an information criteria

### Numbers of Latent Variables
Use the elbow method suggested by Tobler et al (2019)

### Comparing models (not just nested models) (in preferential order)

   1. Cross-validation/out-of-sample: log pointwise predictive density [Gelman, Hwang et al., 2014] . Could do instead of running multiple chains.
   2. Information Criteria: WAIC, possibly for each species. Liked by Gelman, Hwang et al (2014), used by Broms et al (2016). Estimates out-of-sample log pointwise predictive density [Gelman, Hwang et al, 2014]
   3. Won't use AIC, BIC, DIC as Gelman, Hwang et al (2014) preferred WAIC for Bayesian settings. 
   4. Won't use: Pearson's ChiSq discrepancy I.C. for binomial data used by Tobler et al (2015). It seems more appropriate for model checking (hypothesis tests). Tobler et al (2015) developed it into a hypothesis test of sorts. Binomial methods seem hard to use as I have only 2 visits per season. This I.C. was developed by Gelman, Hwang et al in 1996 but Gelman's 2014 paper didn't describe it much.
   5. Won't use: Conditional Predictive Ordinate Criterion (CPO) also demonstrated by Broms et al (2016) as it didn't come up in literature much.
   6. Won't use: area under ROC (AUC). It is not proper and local (Hernandez-Orallo, via Broms 2016). Broms et al (2016) made it sound like AUC was the only one that has seen good use.
   7. Won't use: log scores. Used by Broms et al (2016) in case studies; Gelman, Hwang et al (2014) refers to Bernardo to say that it is both the most common, and it is the unique local and proper rule up to an affine transformation. log pointwise predictive density and WAIC seems to have better properties (noting that I'm not sure that Gelman and Hwang et al don't mean log scores as the log pointwise predictive density).

## Model Diagnostics
1. Dunn-Smyth residuals for drilling into and checking model assumptions [Warton et al, 2017]. Some coding will be required, though can re use Boral or Rpresence code.
   * Residuals per species, against covariates, for detection and occupancy
   1. spatial independence: correlogram of Dunn-Smyth residuals for checking spatial independence
   2. (conditional) independence between years: mean of Dunn-Smyth residuals at a point
   3. If Dunn-Smyth too hard to code then [Gelman, Carlin et al. 2014, Ch 6] suggest binning

2. Average fitted probability of detection vs average detection, per species

3. Hypothesis tests: the "P"-value test mentioned in Warton et al 2017 and also in (Rubin et al 2013, Ch 6)
  * Test model as a whole: like Warton et al?
  * Tests that focus on aspects: Could be conditional to test aspects of the model [Gelman, Carlin et al. 2014, Ch 6]  (e.g. spatial independence, independence between survey years ?)
  * Still don't fully understand __look this up more__


## References

* Broms, Kristin M., Mevin B. Hooten, and Ryan M. Fitzpatrick. “Model Selection and Assessment for Multi-Species Occupancy Models.” Ecology 97, no. 7 (2016): 1759–70.
* Gelman, Andrew, John B. Carlin, Hal S Stern, David B. Dunson, Aki Vehtari, and Donald B. Rubin. Bayesian Data Analysis. Third. Book, Whole. Boca Raton: CRC Press, 2014. https://doi.org/10.1201/b16018.
* Gelman, Andrew, Jessica Hwang, and Aki Vehtari. “Understanding Predictive Information Criteria for Bayesian Models.” Statistics and Computing 24, no. 6 (November 1, 2014): 997–1016. https://doi.org/10.1007/s11222-013-9416-2.
* Tobler, Mathias W., Alfonso Zúñiga Hartley, Samia E. Carrillo‐Percastegui, and George V. N. Powell. “Spatiotemporal Hierarchical Modelling of Species Richness and Occupancy Using Camera Trap Data.” Journal of Applied Ecology 52, no. 2 (2015): 413–21. https://doi.org/10.1111/1365-2664.12399.
Warton, David I., Jakub Stoklosa, Gurutzeta Guillera‐Arroita, Darryl I. MacKenzie, and Alan H. Welsh. “Graphical Diagnostics for Occupancy Models with Imperfect Detection.” Methods in Ecology and Evolution 8, no. 4 (2017): 408–19. https://doi.org/10.1111/2041-210X.12761.

