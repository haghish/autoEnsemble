#' @title Selects Diverse Top-Performing Models for Stacking an Ensemble Model
#' @description Multiple model performance metrics are computed
#' @param eval an object of class \code{"ensemble.eval"} which is provided
#'             by 'evaluate' function. this object is a data.frame, including
#'             several performance metrics for the evaluated models.
#' @param family model family. currently only \code{"binary"} classification models
#'               are supported.
#' @param top_rank numeric. what percentage of the top model should be selected?
#'                 the default value is top 1\% models.
#' @param max integer. specifies maximum number of models for each criteria to be extracted. the
#'            default value is the \code{"top_rank"} percentage for each model selection
#'            criteria.
#' @param model_selection_criteria character, specifying the performance metrics that
#'        should be taken into consideration for model selection. the default are
#'        \code{"c('auc', 'aucpr', 'mcc', 'f2')"}. other possible criteria are
#'        \code{"'f1point5', 'f3', 'f4', 'f5', 'kappa', 'mean_per_class_error', 'gini', 'accuracy'"},
#'        which are also provided by the \code{"evaluate"} function.
#' @return a matrix of F-Measures for different thresholds or the highest F-Measure value
#' @author E. F. Haghish
#'
#' @examples
#'
#' \dontrun{
#' library(h2o)
#' library(h2otools) #for h2o.get_ids() function
#' library(h2oEnsemble)
#'
#' # initiate the H2O server to train a grid of models
#' h2o.init(ignore_config = TRUE, nthreads = 2, bind_to_localhost = FALSE, insecure = TRUE)
#'
#' # Run a grid search or AutoML search
#' prostate_path <- system.file("extdata", "prostate.csv", package = "h2o")
#' prostate <- h2o.importFile(path = prostate_path, header = TRUE)
#' y <- "CAPSULE"
#' prostate[,y] <- as.factor(prostate[,y])  #convert to factor for classification
#' aml <- h2o.automl(y = y, training_frame = prostate, max_runtime_secs = 30,
#'                   seed = 2023, nfolds = 10, keep_cross_validation_predictions = TRUE)
#'
#' # get the model IDs from the H2O Grid search or H2O AutoML Grid
#' ids <- h2otools::h2o.get_ids(aml)
#'
#' # evaluate all the models and return a dataframe
#' evals <- evaluate(id = ids)
#'
#' # perform model selection (up to top 10% of each criteria)
#' select <- modelSelection(eval = evals, top_rank = 0.1))
#' }
#' @export

modelSelection <- function(eval,
                           family = "binary",
                           top_rank = 0.01,
                           max = NULL,
                           model_selection_criteria = c("auc",
                                                        "aucpr",
                                                        "mcc",
                                                        "f2")
                           ) {

  results <- NULL
  if (is.null(max)) max <- round(top_rank*nrow(eval))

  if (family == "binary") {
    for (i in model_selection_criteria) {
      if (i != "mean_per_class_error") {
        eval[,i] <- as.numeric(eval[,i])
        sub <- eval[order(eval[,i], decreasing = T), ]
        maxim <- sub[1, i]
        minim <- maxim - (maxim*top_rank)
        lnth <- length(sub[sub[,i] >= minim,"id"])
        if (lnth > max) results <- c(results, sub[1:max,"id"])
        else results <- c(results, sub[sub[,i] >= minim,"id"])
      }
      else if (i == "mean_per_class_error") {
        eval[,i] <- as.numeric(eval[,i])
        sub <- eval[order(eval[,i], decreasing = F), ]
        minim <- sub[1, i]
        maxim <- minim + (minim*top_rank)
        lnth <- length(sub[sub[,i] <= maxim,"id"])
        if (lnth > max) results <- c(results, sub[1:max,"id"])
        else results <- c(results, sub[sub[,i] <= max,"id"])
      }
    }
  }
  else {
    stop("currently only 'binary' models are supported")
  }

  return(unique(results))
}


