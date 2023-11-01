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

p <- h2o.predict_contributions(object = aml@leader,
                                newdata = prostate,
                                top_n=5)
View(as.data.frame(p))

# Compute SHAP and pick the top two highest regardless of the sign
pabs <- h2o.predict_contributions(aml@leader, prostate, top_n=5, compare_abs=TRUE)
# Compute SHAP and pick the top two lowest regardless of the sign
View(as.data.frame(pabs))

h2o.shap_summary_plot(
  model = aml@leader,
  newdata = prostate,
  columns = NULL,
  top_n_features = 5)

h2o.shap_summary_plot(
  model = h2o.getModel("GBM_grid_1_AutoML_2_20230920_225003_model_7"),
  newdata = prostate,
  columns = NULL,
  top_n_features = 5)

(plt <- h2o.shap_summary_plot(
  model = aml@leader,
  newdata = prostate,
  columns = NULL,
  top_n_features = 20,
  sample_size = 100
))

# > both 'top' and 'search' strategies had identical results, but outperform the grid search and AutoML search. Yet, this was a small dataset, and a quick test...
