# Structural Equation Modelling and Its Relevance to Our Project
*By Kassel Hingee, 17 March 2020*

I have had a brief look at Structural Equation Modelling (SEM).
There is a lot that I still don't know.
Much of what I do know (and the following) is from David Kaplan's book [Structural Equation Modeling: Foundations and Extensions](https://dx-doi-org.virtual.anu.edu.au/10.4135/9781452226576).
I do not know how much Martin knows (Martin: please jump in and tell me if I'm wrong!)

## Previous Mistakes of Mine
I was wrong that we would have to fix a model structure.
To some extent model structure can be optimised in the SEM model selection process.
There are limits due to identifiability problems, and due to computational cost.

## What is SEM?
I suspect most non-technical people view SEM as fitting a quasi-causal diagram.
In such a diagram stations in represent random variables (I think), either observable or latent.
The arrows between stations represent dependence, typically a linear relationship.
The fitting process finds the coefficients for these relationships and any other free parameters for the distributions of the random variables (typically mean and variance).

What is actually happening is multiple regression (regression with multivariate response). 
The random variables, coefficients, parameters and structure of the diagram are represented as a matrix equation.
Lack of dependence is probably represented as zeros in these matrices.

According to Kaplan the random variables can be random effects, latent variables, Gaussian noise and more.
The random variables do not have to be Gaussian themselves.


## How could we use SEM?
I think SEM could theoretically do everything we want, except that model diagnostic methods may be fairly poor (because models can be quite complex - I've not looked into this though).

### Relevant Literature
A brief search on scopus revealed very few articles explicitly using SEM for occupancy modelling, at least according to their abstract + title.
[Joseph et al. (2016)](https://www.jstor.org/stable/24703410) did something almost exactly like what we want.
 
 + 2 visits per site per season
 + 170ish sites
 + grazing intensity an unknown latent variable
 + multiple species
 + detection probabilities different for each species and each visit

Their research focused on amphibians. So our detection probabilities, for example, are likely to be more variable.
They also used Bayesian methods (stan), which might be faster that Boral's single MCMC chain.
They used simulations to test their methods, which is better than nothing, but not enough simulations for a new statistical method (if it really can be called 'new').
We're planning out-of-sample test data which will be a bit more rigorous for prediction quality I think.
I have not noticed if their code is available.

Joseph et al.'s work is only cited 19 times. I think that is unusual considering how clean and nice this concept is. Makes me think there is either something to worry about with the work, or it is identical to existing concepts (just using different words).

### Benefits

+ SEM might allow latent variables for habitat properties, occupancy, for population, and for detection rates.
+ Specification of model can be quite precise with SEM.
If the model structure is good then it will reduce the risk of over fitting and perhaps even enable extrapolation. 
+ Model interpretability could also be very good. Estimates of both population and detection rates would be very nice, if possible.

### Difficulties

+ We may not have enough data on enough of the birds to fit an SEM with ecologically-justifiable structure (e.g. population and detection latent variables and interaction between birds).
+ Software may not be flexible enough for the different treatments of all the random variables (some are periodic, some are categorical, others a numeric, some will be on the log scale).

### Computation
__Standard Software:__ As one of SEM's benefits is multivariate responses, I think standard SEM software should be able to cope with multiple bird species.
Although I do not know how one would tell the software which variables to predict, and which ones are covariates (the language of SEM does not seem to have this distinction).

__Boral:__ Boral can do multiple regression. I think it can handle categorical and numerical responses at the same time.
This makes me think that we could obtain the model matrices using SEM software/thinking and then run Boral on them to fit parameters.

__Ask Wade:__ Wade has a recent paper using a form of SEM called 'path analysis'. I think he would be a very good source of knowledge.

## Next Steps
+ Consult Martin
+ Consult Wade
+ Consider whether the SEM we create would just be Boral in disguise


