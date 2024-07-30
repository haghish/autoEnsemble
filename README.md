<a href="https://github.com/haghish/autoEnsemble"><img src='man/figures/logo_noname.png' align="right" height="200" /></a>
  
  `autoEnsemble` : An AutoML Algorithm for Building Homogeneous and Heterogeneous Stacked Ensemble Models by Searching for Diverse Base-Learners
==========================================================================================================================================

[![CRAN version](https://www.r-pkg.org/badges/version/autoEnsemble?color=f29e02)](https://cran.r-project.org/package=autoEnsemble)  [![](https://cranlogs.r-pkg.org/badges/grand-total/autoEnsemble?color=f2c602)](https://cran.r-project.org/package=autoEnsemble) [![](man/figures/manual.svg)](https://CRAN.R-project.org/package=autoEnsemble)

The `autoEnsemble` R package present an AutoML algorithm for building stacked ensemble of classifiers, that are particularly powerful for severely imbalanced outcomes. Building a successfult stacked ensemble models is based on two principles:
  
1. The base-learners are more accurate than a chance prediction. The more accurate the base-learners, the stronger the stacked ensemble
2. The base-learners are diverse, i.e., their error is not correlated. 

While the first principle is easy to assess with straight-forward model evaluation criteria, searching for _diverse excellent models_ is not that easy, particularly under severe class imbalance problem. The **`autoEnsemble`** R package implements an automated machine learning algorithm that identifies excellent - yet destinct - models and stacks them to build an excellent stacked ensemble model. Currently, 2 strategies are programmed, __top__ and __search__:

Strategy    | Description
----------- | -----------
_top_       | combines top-ranked models identified by specific set of model performance metrics. this strategy is a particular case of the `search` strategy and is not expected to perform better than `search`, however, it is less computationally extensive. You only have to specify the top percentage of models to be used for modeling. The drawback is that often, you do not know what is the optimum number of models to be combined and that is why the `search` procedure is applied by default, to search for the optimal number of diverse models that deliver the best results. 
_search_    | combines diverse top-ranked models identified by specific set of model performance metrics, as long as the ensemble model keeps improving. This strategy is expected to outperform `top` strategy, but requires longer computation time to gradually combine diverse top models and examine the improvement of the resulting model. 


What makes these strategies destinct is the procedure of _model evaluation and selection_, not the procedure of building the ensemble. The algorithm will be explained in a journal article (to be expected in 2023). 

### Example

```R
# load the required libraries for building the base-learners and the ensemble models
library(h2o)
library(h2otools)
library(autoEnsemble)

# initiate the h2o server
h2o.init(ignore_config = TRUE, nthreads = 2, bind_to_localhost = FALSE, insecure = TRUE)

# upload data to h2o cloud
prostate_path <- system.file("extdata", "prostate.csv", package = "h2o")
prostate <- h2o.importFile(path = prostate_path, header = TRUE)

### H2O provides 2 types of grid search for tuning the models, which are 
### AutoML and Grid. Below, I tune 2 set of model grids and use them both 
### for building the ensemble, just to set an example ... 

#######################################################
### PREPARE AutoML Grid (takes a couple of minutes)
#######################################################
# run AutoML to tune various models (GLM, GBM, XGBoost, DRF, DeepLearning) for 120 seconds
y <- "CAPSULE"
prostate[,y] <- as.factor(prostate[,y])  #convert to factor for classification
aml <- h2o.automl(y = y, training_frame = prostate, max_runtime_secs = 30, 
                  include_algos=c("DRF","GLM", "XGBoost", "GBM", "DeepLearning"),
                  
                  # this setting ensures the models are comparable for building a meta learner
                  seed = 2023, nfolds = 10, 
                  keep_cross_validation_predictions = TRUE)

#######################################################
### PREPARE H2O Grid 
#######################################################
# make sure equal number of "nfolds" is specified for different grids
grid <- h2o.grid(algorithm = "gbm", y = y, training_frame = prostate,
                 hyper_params = list(ntrees = seq(1,25,1)), grid_id = "ensemble_grid",
                 
                 # this setting ensures the models are comparable for building a meta learner
                 seed = 2023, fold_assignment = "Modulo", nfolds = 10, 
                 keep_cross_validation_predictions = TRUE)

#######################################################
### PREPARE ENSEMBLE MODELS
#######################################################

### get the models' IDs from the AutoML and grid searches.
### this is all that is needed before building the ensemble,
### i.e., to specify the model IDs that should be evaluated.

ids <- c(h2o.get_ids(aml), h2o.get_ids(grid))
top <- ensemble(models = ids, training_frame = prostate, strategy = "top")
search <- ensemble(models = ids, training_frame = prostate, strategy = "search")

#######################################################
### EVALUATE THE MODELS
#######################################################
h2o.auc(aml@leader)                          # best model identified by h2o.automl
h2o.auc(h2o.getModel(grid@model_ids[[1]]))   # best model identified by grid search
h2o.auc(top$model)                           # ensemble model with 'top' strategy
h2o.auc(search$model)                        # ensemble model with 'search' strategy

# > both 'top' and 'search' strategies had identical results, but outperform the grid search and AutoML search. Yet, this was a small dataset, and a quick test... 
```
