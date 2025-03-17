#' @title Automatically Trains H2O Models and Builds a Stacked Ensemble Model
#' @description Automatically trains various algorithms to build base-learners and then
#'              automatically creates a stacked ensemble model
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom h2o h2o.stackedEnsemble h2o.getModel h2o.auc h2o.aucpr h2o.mcc
#'             h2o.F2 h2o.mean_per_class_error h2o.giniCoef h2o.accuracy h2o.automl
#'             h2o.init h2o.saveModel
#' @importFrom curl curl
#' @param include_algos Vector of character strings naming the algorithms to
#'                      restrict to during the model-building phase. this argument
#'                      is passed to autoML.
#' @param x          Vector. Predictor column names or indices.
#' @param y          Character. The response column name or index.
#' @param training_frame An H2OFrame containing the training data.
#'                   Default is \code{h2o.getFrame("hmda.train.hex")}.
#' @param validation_frame An H2OFrame for early stopping. Default is \code{NULL}.
#' @param newdata h2o frame (data.frame). the data.frame must be already uploaded
#'                on h2o server (cloud). when specified, this dataset will be used
#'                for evaluating the models. if not specified, model performance
#'                on the training dataset will be reported.
#' @param family model family. currently only \code{"binary"} classification models
#'               are supported.
#' @param strategy character. the current available strategies are \code{"search"}
#'                 (default) and \code{"top"}. The \code{"search"} strategy searches
#'                 for the best combination of top-performing diverse models
#'                 whereas the \code{"top"} strategy is more simplified and just
#'                 combines the specified of top-performing diverse models without
#'                 examining the possibility of improving the model by searching for
#'                 larger number of models that can further improve the model. generally,
#'                 the \code{"search"} strategy is preferable, unless the computation
#'                 runtime is too large and optimization is not possible.
#' @param max integer. specifies maximum number of models for each criteria to be extracted. the
#'            default value is the \code{"top_rank"} percentage for each model selection
#'            criteria.
#' @param max_models Maximum number of models to build in the AutoML training (passed to autoML)
#' @param model_selection_criteria character, specifying the performance metrics that
#'        should be taken into consideration for model selection. the default are
#'        \code{"c('auc', 'aucpr', 'mcc', 'f2')"}. other possible criteria are
#'        \code{"'f1point5', 'f3', 'f4', 'f5', 'kappa', 'mean_per_class_error', 'gini', 'accuracy'"},
#'        which are also provided by the \code{"evaluate"} function.
#' @param min_improvement numeric. specifies the minimum improvement in model
#'                        evaluation metric to qualify further optimization search.
#' @param max_runtime_secs_per_model Maximum runtime in seconds dedicated to each
#'                                   individual model training process.
#' @param max_runtime_secs Integer. This argument specifies the maximum time that
#'                         the AutoML process will run for in seconds.
#' @param top_rank numeric vector. specifies percentage of the top models taht
#'                 should be selected. if the strategy is \code{"search"}, the
#'                 algorithm searches for the best best combination of the models
#'                 from top ranked models to the bottom. however, if the strategy
#'                 is \code{"top"}, only the first value of the vector is used
#'                 (default value is top 1\%).
#' @param stop_rounds integer. number of stoping rounds, in case the model stops
#'                    improving
#' @param reset_stop_rounds logical. if TRUE, everytime the model improves the
#'                          stopping rounds penalty is resets to 0.
#' @param stop_metric character. model stopping metric. the default is \code{"auc"},
#'                    but \code{"aucpr"} and \code{"mcc"} are also available.
#' @param nfolds     Integer. Number of folds for cross-validation.
#'                   Default is 10.
#' @param balance_classes Logical. Specify whether to oversample the minority
#'                        classes to balance the class distribution; only applicable to classification
#' @param save_models Logical. if TRUE, the models trained will be stored locally
#' @param directory path to a local directory to store the trained models
#' @param sort_metric Metric to sort the leaderboard by (passed to autoML).
#'                    For binomial classification
#'                    choose between "AUC", "AUCPR", "logloss", "mean_per_class_error",
#'                    "RMSE", "MSE". For regression choose between "mean_residual_deviance",
#'                    "RMSE", "MSE", "MAE", and "RMSLE". For multinomial classification choose
#'                    between "mean_per_class_error", "logloss", "RMSE", "MSE". Default is
#'                    "AUTO". If set to "AUTO", then "AUC" will be used for binomial classification,
#'                    "mean_per_class_error" for multinomial classification, and
#'                    "mean_residual_deviance" for regression.
#' @param seed random seed (recommended)
#' @param verbatim logical. if TRUE, it reports additional information about the
#'                 progress of the model training, particularly used for debugging.
#' @param ignore_config arguments to be passed to h2o.init()
#' @param bind_to_localhost arguments to be passed to h2o.init()
#' @param insecure arguments to be passed to h2o.init()
#' @param nthreads arguments to be passed to h2o.init()
#' @param max_mem_size arguments to be passed to h2o.init()
#' @param min_mem_size arguments to be passed to h2o.init()
#' @param startH2O Logical. if TRUE, h2o server will be initiated.
#' @param ... parameters to be passed to autoML algorithm in h2o package
#' @return a list including the ensemble model and the top-rank models that were
#'         used in the model
#' @author E. F. Haghish
#'
#' @examples
#'
#' \dontrun{
#' # load the required libraries for building the base-learners and the ensemble models
#' library(h2o)
# library(h2otools)
#' library(autoEnsemble)
#'
#' # initiate the h2o server
#' h2o.init(ignore_config = TRUE, nthreads = 2, bind_to_localhost = FALSE, insecure = TRUE)
#'
#' # upload data to h2o cloud
#' prostate_path <- system.file("extdata", "prostate.csv", package = "h2o")
#' prostate <- h2o.importFile(path = prostate_path, header = TRUE)
#'
#' ### H2O provides 2 types of grid search for tuning the models, which are
#' ### AutoML and Grid. Below, I tune 2 set of model grids and use them both
#' ### for building the ensemble, just to set an example ...
#'
#' #######################################################
#' ### PREPARE AutoML Grid (takes a couple of minutes)
#' #######################################################
#' # run AutoML to tune various models (GLM, GBM, XGBoost, DRF, DeepLearning) for 120 seconds
#' y <- "CAPSULE"
#' prostate[,y] <- as.factor(prostate[,y])  #convert to factor for classification
#' aml <- h2o.automl(y = y, training_frame = prostate, max_runtime_secs = 120,
#'                  include_algos=c("DRF","GLM", "XGBoost", "GBM", "DeepLearning"),
#'
#'                  # this setting ensures the models are comparable for building a meta learner
#'                  seed = 2023, nfolds = 10,
#'                  keep_cross_validation_predictions = TRUE)
#'
#' #######################################################
#' ### PREPARE H2O Grid (takes a couple of minutes)
#' #######################################################
#' # make sure equal number of "nfolds" is specified for different grids
#' grid <- h2o.grid(algorithm = "gbm", y = y, training_frame = prostate,
#'                  hyper_params = list(ntrees = seq(1,50,1)),
#'                  grid_id = "ensemble_grid",
#'
#'                  # this setting ensures the models are comparable for building a meta learner
#'                  seed = 2023, fold_assignment = "Modulo", nfolds = 10,
#'                  keep_cross_validation_predictions = TRUE)
#'
#' #######################################################
#' ### PREPARE ENSEMBLE MODEL
#' #######################################################
#'
#' ### get the models' IDs from the AutoML and grid searches.
#' ### this is all that is needed before building the ensemble,
#' ### i.e., to specify the model IDs that should be evaluated.
#'
#' ids    <- c(h2o.get_ids(aml), h2o.get_ids(grid))
#' top    <- ensemble(models = ids, training_frame = prostate, strategy = "top")
#' search <- ensemble(models = ids, training_frame = prostate, strategy = "search")
#'
#' #######################################################
#' ### EVALUATE THE MODELS
#' #######################################################
#' h2o.auc(aml@leader)                          # best model identified by h2o.automl
#' h2o.auc(h2o.getModel(grid@model_ids[[1]]))   # best model identified by grid search
#' h2o.auc(top$model).                          # ensemble model with 'top' search strategy
#' h2o.auc(search$model).                       # ensemble model with 'search' search strategy
#'
#' }
#' @export

autoEnsemble <- function(
                     #automl arguments
                     #----------------
                     x, #NEW
                     y, #NEW
                     training_frame,
                     validation_frame = NULL, #NEW
                     nfolds = 10, #NEW
                     balance_classes = TRUE, #NEW
                     max_runtime_secs = NULL,
                     max_runtime_secs_per_model = NULL,
                     max_models = NULL,
                     sort_metric = "AUCPR",
                     include_algos = c("GLM", "DeepLearning", "DRF", "XGBoost", "GBM"),
                     #in future updates, add "StackedEnsemble" to create ensembles with different strategies and
                     #choose the best-performing one. For imbalanced data, however, the autoEnsemble is expected
                     #to outperform other strategies such as best of families and best models.
                     save_models = FALSE,
                     directory = paste("autoEnsemble", format(Sys.time(), "%d-%m-%y-%H:%M")),
                     ...,

                     # ensemble arguments
                     # ------------------
                     newdata = NULL,
                     family = "binary",
                     strategy = c("search"),
                     model_selection_criteria = c("auc","aucpr","mcc","f2"),
                     min_improvement = 0.00001,
                     max = NULL,
                     top_rank = seq(0.01, 0.99, 0.01),
                     stop_rounds = 3,
                     reset_stop_rounds = TRUE,
                     stop_metric = "auc",
                     seed = -1,
                     verbatim = FALSE,
                     #loaded = TRUE,
                     #path = NULL

                     # initiate h2o server
                     # -------------------
                     startH2O = FALSE,
                     nthreads = NULL,
                     max_mem_size = NULL,
                     min_mem_size = NULL,
                     ignore_config = FALSE,
                     bind_to_localhost = FALSE,
                     insecure = TRUE
                     ) {

  # STEP 0: Initiate h2o server
  # ============================================================
  if (startH2O) h2o.init(nthreads = nthreads,
                         ignore_config = ignore_config,
                         max_mem_size = max_mem_size,
                         min_mem_size = min_mem_size,
                         bind_to_localhost = bind_to_localhost,
                         insecure = insecure)

  Sys.sleep(5)

  # STEP 1: train the models
  # ============================================================
  models <- h2o.automl(x=x,
                       y=y,
                       training_frame=training_frame,
                       validation_frame=validation_frame,
                       nfolds=nfolds,
                       balance_classes = balance_classes,
                       max_runtime_secs = max_runtime_secs,
                       max_runtime_secs_per_model = max_runtime_secs_per_model,
                       max_models = max_models,
                       sort_metric=sort_metric,
                       include_algos = include_algos,
                       project_name = "autoEnsemble",
                       seed = seed,
                       verbosity = verbatim,
                       keep_cross_validation_predictions = TRUE,
                       ... )

  # STEP 2: build the autoEnsemble stacked ensemble
  # ============================================================
  ids    <- h2o.get_ids(models)
  ens    <- ensemble(models = ids,
                     training_frame = training_frame,
                     strategy = strategy,
                     newdata = newdata,
                     family = family,
                     model_selection_criteria = model_selection_criteria,
                     min_improvement = min_improvement,
                     max = max,
                     top_rank = top_rank,
                     stop_rounds = stop_rounds,
                     reset_stop_rounds = reset_stop_rounds,
                     stop_metric = stop_metric,
                     verbatim = verbatim,
                     seed = seed)

  class(ens) <- c("autoEnsemble", "list")

  # STEP 3: store the models
  # ============================================================
  if (save_models) {

    dir.create(directory)
    dir.create(paste0(directory,"/baselearners"))
    dir.create(paste0(directory,"/autoEnsemble"))

    for (i in ids) {
      h2o.saveModel(h2o.getModel(i), path = paste0(directory,"/baselearners"), force = T, export_cross_validation_predictions = T)
    }

    h2o.saveModel(ens$model, path = paste0(directory,"/autoEnsemble"), force = T, export_cross_validation_predictions = T)
  }

  # STEP 4: return the stacked ensemble model
  # ============================================================
  return(ens)

}

