#' @title Evaluate H2O Model(s) Performance
#' @description Multiple model performance metrics are computed
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom h2o h2o.stackedEnsemble h2o.getModel h2o.auc h2o.aucpr h2o.mcc
#'             h2o.F2 h2o.mean_per_class_error h2o.giniCoef h2o.accuracy
#' @importFrom h2otools h2o.get_ids
#' @importFrom curl curl
#' @param models H2O search grid or AutoML grid or a character vector of H2O model IDs.
#'               the \code{"h2o.get_ids"} function from \code{"h2otools"} can
#'               retrieve the IDs from grids.
#' @param training_frame h2o training frame (data.frame) for model training
#' @param newdata h2o frame (data.frame). the data.frame must be already uploaded
#'                on h2o server (cloud). when specified, this dataset will be used
#'                for evaluating the models. if not specified, model performance
#'                on the training dataset will be reported.
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
#' @param model_selection_criteria character, specifying the performance metrics that
#'        should be taken into consideration for model selection. the default are
#'        \code{"c('auc', 'aucpr', 'mcc', 'f2')"}. other possible criteria are
#'        \code{"'f1point5', 'f3', 'f4', 'f5', 'kappa', 'mean_per_class_error', 'gini', 'accuracy'"},
#'        which are also provided by the \code{"evaluate"} function.
#' @param top_rank numeric vector. specifies percentage of the top models taht
#'                 should be selected. if the strategy is \code{"search"}, the
#'                 algorithm searches for the best best combination of the models
#'                 from top ranked models to the bottom. however, if the strategy
#'                 is \code{"top"}, only the first value of the vector is used
#'                 (default value is top 1\%).
#' @param stop_rounds integer. number of stoping rounds, in case the model stops
#'                    improving
#' @param stop_metric character. model stopping metric. the default is \code{"auc"},
#'                    but \code{"aucpr"} and \code{"mcc"} are also available.
#' @param seed random seed (recommended)
#' @param verbatim logical. if TRUE, it reports additional information about the
#'                 progress of the model training, particularly used for debugging.
#' @return a matrix of F-Measures for different thresholds or the highest F-Measure value
#' @author E. F. Haghish
#'
#' @examples
#'
#' \dontrun{
#' # initiate h2o server
#' library(h2o)
#' h2o.init(ignore_config = TRUE, nthreads = 2, bind_to_localhost = FALSE, insecure = TRUE)
#'
#' # upload data to h2o cloud
#' prostate_path <- system.file("extdata", "prostate.csv", package = "h2o")
#' prostate <- h2o.importFile(path = prostate_path, header = TRUE)
#'
#' #######################################################
#' ### PREPARE AutoML Grid
#' #######################################################
#'
#' # run AutoML to tune various models (GLM, GBM, XGBoost, DRF, DeepLearning) for 30 seconds
#' y <- "CAPSULE"
#' prostate[,y] <- as.factor(prostate[,y])  #convert to factor for classification
#' aml <- h2o.automl(y = y, training_frame = prostate, max_runtime_secs = 30,
#'                   include_algos=c("DRF","GLM", "XGBoost", "GBM", "DeepLearning"))
#'
#' #######################################################
#' ### PREPARE H2O Grid (takes a couple of minutes)
#' #######################################################
#' grid <- h2o.grid(algorithm = "gbm", y = y, training_frame = prostate,
#'                 hyper_params = list(ntrees = seq(1,500,5)))
#'
#' #######################################################
#' ### PREPARE ENSEMBLE MODEL
#' #######################################################
#'
#' # get the models' IDs from the AutoML grid
#' ids <- c(h2o.ids(aml), h2o.ids(grid))
#'
#' # RUN THE ENSEMBLE FOR ALL STRATEGIES
#' ens <- ensemble(models = ids, training_frame = prostate)
#'
#' # evaluate the model performance of the best model
#'
#' }
#' @export

ensemble <- function(models,
                     training_frame,
                     newdata = NULL,
                     strategy = c("search"),
                     model_selection_criteria = c("auc","aucpr","mcc","f2"),
                     max = NULL,
                     top_rank = seq(0.01, 0.99, 0.01),
                     stop_rounds = 3,
                     reset_stop_rounds = TRUE,
                     stop_metric = "auc",
                     seed = -1,
                     verbatim = FALSE
                     #loaded = TRUE,
                     #path = NULL
                     ) {

  modelTOP  <- NULL
  modelSTOP <- NULL

  # STEP 0: prepare the models
  # ============================================================
  if (inherits(models,"H2OAutoML") | inherits(models,"H2OAutoML")) {
    ids <- h2o.get_ids(models)
  }
  else if (inherits(models,"character")) {
    ids <- models

    # # if the models are not uploaded to H2O cloud, do so
    # if (!loaded) {
    #   for (i in ids) h2o::h2o.loadModel(paste0(path, i))
    # }
  }

  # get the models' parameters from trained models:
  params <- h2o::h2o.getModel(ids[1])
  #nfolds <- params@parameters$nfolds
  x      <- params@parameters$x
  y      <- params@parameters$y

  # STEP 1: Evaluate the models for various criteria
  # ============================================================
  modelEval <- evaluate(id = ids, newdata = newdata)
  if (verbatim) message("\nmodels were successfully evaluated")

  # STEP 2: Apply model selection criteria (TOP)
  # ============================================================
  if ("top" %in% strategy) {
    slctTOP <- modelSelection(eval = modelEval,
                              max = max,
                              top_rank = top_rank[1],
                              model_selection_criteria = model_selection_criteria)

    # train the ensemble
    # ----------------------------------------------------------
    model <- h2o.stackedEnsemble(x = x,
                                 y = y,
                                 training_frame = training_frame,
                                 model_id = "top",
                                 base_models = ids,
                                 seed = seed)
  }
  if (verbatim) message("'top' strategy was successfully evaluated")

  # STEP 3: Apply model selection criteria (SEARCH)
  # ============================================================
  if ("search" %in% strategy) {
    AUC       <- 0
    AUCPR     <- 0
    MCC       <- 0
    TOP       <- NULL
    STOP      <- 0

    if (verbatim) message("'search' strategy tuning:")

    for (i in top_rank) {
      while (STOP <= stop_rounds) {
        slctSTOP <- modelSelection(eval = modelEval,
                                   max = max,
                                   top_rank = i,
                                   model_selection_criteria = model_selection_criteria)

        # train the ensemble and evaluate it
        stopModel <- h2o.stackedEnsemble(x = x,
                                         y = y,
                                         training_frame = training_frame,
                                         model_id = paste0("stop_",i),
                                         base_models = ids,
                                         seed = seed)

        # evaluate the model for AUC
        # ------------------------------------------------------------
        if (stop_metric == "auc") {
          auc <- as.numeric(h2o::h2o.auc(stopModel))
          if (AUC < auc) {
            AUC <- auc
            TOP <- i
            model <- stopModel
            if (reset_stop_rounds) STOP <- 0
          }
          else STOP <- STOP + 1
        }

        # evaluate the model for AUCPR
        # ------------------------------------------------------------
        if (stop_metric == "aucpr") {
          aucpr <- as.numeric(h2o::h2o.aucpr(stopModel))
          if (AUCPR < aucpr) {
            AUCPR <- aucpr
            TOP <- i
            model <- stopModel
            if (reset_stop_rounds) STOP <- 0
          }
          else STOP <- STOP + 1
        }

        # evaluate the model for MCC
        # ------------------------------------------------------------
        if (stop_metric == "mcc") {
          mcc <- max(h2o::h2o.mcc(stopModel)[,2])
          if (MCC < mcc) {
            MCC <- mcc
            TOP <- i
            model <- stopModel
            if (reset_stop_rounds) STOP <- 0
          }
          else STOP <- STOP + 1
        }
      }

      break
    }
  }

  return(model)
}

