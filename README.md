<a href="https://github.com/haghish/ensemble"><img src='man/figures/logo.PNG' align="right" height="200" /></a>

`ensemble` : An AutoML Algorithm for Building Homogeneous and Heterogeneous Stacked Ensemble Models by Searching for Diverse Base-Learners
==========================================================================================================================================

Building a successfult stacked ensemble models is based on two principles:

1. The base-learners are more accurate than a chance prediction. The more accurate the base-learners, the stronger the stacked ensemble
2. The base-learners are diverse, i.e., their error is not correlated. 

While the first principle is easy to assess with straight-forward model evaluation criteria, searching for _diverse excellent models_ is not that easy. The **`ensemble`** R package implements an auomated machine learning algorithm that identifies excellent - yet destinct - models and stacks them to build an excellent stacked ensemble model. Currently, 2 strategies are programmed:

Strategy    | Description
----------- | -----------
_top_       | combines top-ranked models identified by specific set of model performance metrics
_stop_      | combines top-ranked models identified by specific set of model performance metrics, as long as the ensemble model keeps improving


### Example

```R
# load the required libraries for building the base-learners and the ensemble models
library(h2o)
library(h2otools)
library(ensemble)

# initiate the h2o server
h2o.init(ignore_config = TRUE, nthreads = 2, bind_to_localhost = FALSE, insecure = TRUE)

# upload data to h2o cloud
prostate_path <- system.file("extdata", "prostate.csv", package = "h2o")
prostate <- h2o.importFile(path = prostate_path, header = TRUE)

### H2O provides 2 types of grid search for tuning the models, which are 
### AutoML and Grid. Below, I tune 2 set of model grids and use them both 
### for building the ensemble, just to set an example ... 

#######################################################
### PREPARE AutoML Grid
#######################################################
# run AutoML to tune various models (GLM, GBM, XGBoost, DRF, DeepLearning) for 30 seconds
y <- "CAPSULE"
prostate[,y] <- as.factor(prostate[,y])  #convert to factor for classification
aml <- h2o.automl(y = y, training_frame = prostate, max_runtime_secs = 60,
                  include_algos=c("DRF","GLM", "XGBoost", "GBM", "DeepLearning"))

#######################################################
### PREPARE H2O Grid (takes a couple of minutes)
#######################################################
grid <- h2o.grid(algorithm = "gbm", y = y, training_frame = prostate,
                 hyper_params = list(ntrees = seq(1,50,1)))

#######################################################
### PREPARE ENSEMBLE MODEL
#######################################################

# get the models' IDs from the AutoML grid
ids <- c(h2o.ids(aml), h2o.ids(grid))

# RUN THE ENSEMBLE FOR ALL STRATEGIES
ens <- ensemble(models = ids, training_frame = prostate)

# evaluate the model performance of the best model
perf <- h2o.performance(aml@leader, xval = TRUE)
```
