#' @title Evaluate H2O Model(s) Performance
#' @description Multiple model performance metrics are computed
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom h2o h2o.stackedEnsemble h2o.getModel h2o.performance h2o.auc h2o.aucpr h2o.mcc
#'             h2o.F2 h2o.mean_per_class_error h2o.giniCoef h2o.accuracy
#' @importFrom h2otools h2o.get_ids
#' @importFrom curl curl
#' @param perf a h2o object of class \code{"H2OBinomialMetrics"} which is provided
#'             by 'h2o.performance' function.
#' @param max logical. default is FALSE. if TRUE, instead of providing the F-Measure
#'            for all the thresholds, the highest F-Measure is reported.
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
#' perf <- h2o.performance(aml@leader, xval = TRUE)
#'
#' }
#' @export


ensemble <- function(models,
                     training_frame,
                     strategy = c("top", "stop"),
                     model_selection_criteria = c("auc","aucpr","mcc","f2"),
                     top_rank = 0.05,
                     stop_rank = seq(0.01, 0.99, 0.01),
                     stop_rounds = 10,
                     stop_metric = "aucpr",
                     loaded = TRUE,
                     path = NULL,
                     seed = -1
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

    # if the models are not uploaded to H2O cloud, do so
    if (!loaded) {
      for (i in ids) h2o::h2o.loadModel(paste0(path, i))
    }
  }

  # get the models' parameters from trained models:
  params <- h2o::h2o.getModel(ids[1])
  #nfolds <- params@parameters$nfolds
  x      <- params@parameters$x
  y      <- params@parameters$y

  # STEP 1: Evaluate the models for various criteria
  # ============================================================
  modelEval <- evaluate(id = ids)

  # STEP 2A: Apply model selection criteria (TOP)
  # ============================================================
  if ("top" %in% strategy) {
    slctTOP <- modelSelection(eval = modelEval, top_rank = top_rank,
                              model_selection_criteria = model_selection_criteria)

    # train the ensemble
    # ----------------------------------------------------------
    modelTOP <- h2o.stackedEnsemble(x = x,
                                    y = y,
                                    training_frame = training_frame,
                                    model_id = "top",
                                    base_models = ids,
                                    seed = seed)
  }

  # STEP 2B: Apply model selection criteria (STOP)
  # ============================================================
  if ("stop" %in% strategy) {
    AUC       <- 0
    AUCPR     <- 0
    MCC       <- 0
    TOP       <- NULL
    STOP      <- 0

    for (i in stop_rank) {
      while (STOP <= stop) {
        slctSTOP <- modelSelection(eval = modelEval, top = i)

        # train the ensemble and evaluate it
        stopModel <- h2o.stackedEnsemble(x = x,
                                         y = y,
                                         training_frame = training_frame,
                                         model_id = paste0("stop_",i),
                                         base_models = ids,
                                         seed = seed)

        auc <- as.numeric(h2o::h2o.auc(stopModel))
        aucpr <- as.numeric(h2o::h2o.aucpr(stopModel))
        mcc <- max(h2o::h2o.mcc(stopModel)[,2])

        # evaluate the model for AUC
        # ------------------------------------------------------------
        if (stop_metric == "auc") {
          if (AUC < auc) {
            AUC <- auc
            TOP <- i
            modelSTOP <- stopModel
          }
          else STOP <- STOP + 1
        }

        # evaluate the model for AUCPR
        # ------------------------------------------------------------
        if (stop_metric == "aucpr") {
          if (AUCPR < aucpr) {
            AUCPR <- aucpr
            TOP <- i
            modelSTOP <- stopModel
          }
          else STOP <- STOP + 1
        }

        # evaluate the model for MCC
        # ------------------------------------------------------------
        if (stop_metric == "mcc") {
          if (MCC < mcc) {
            MCC <- mcc
            TOP <- i
            modelSTOP <- stopModel
          }
          else STOP <- STOP + 1
        }
      }

      break
    }
  }

  return(list(top  = modelTOP,
              stop = modelSTOP))
}

