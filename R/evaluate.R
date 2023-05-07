#' @title Evaluate H2O Model(s) Performance
#' @description Multiple model performance metrics are computed for each model
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom h2o h2o.getModel h2o.performance h2o.auc h2o.aucpr h2o.mcc
#'             h2o.mean_per_class_error h2o.giniCoef h2o.accuracy
#' @importFrom h2otools Fmeasure kappa
#' @importFrom curl curl
#' @param id a character vector of H2O model IDs retrieved from H2O Grid search
#'           or AutoML random search. the \code{"h2o.get_ids"} function from
#'           \code{"h2otools"} can retrieve the IDs from grids.
#' @param newdata h2o frame (data.frame). the data.frame must be already uploaded
#'                on h2o server (cloud). when specified, this dataset will be used
#'                for evaluating the models. if not specified, model performance
#'                on the training dataset will be reported.
#' @param ... arguments to be passed to \code{"h2o.performance"} from H2O package
#' @return a data.frame of various model performance metrics for each model
#' @author E. F. Haghish
#'
#' @examples
#'
#' \dontrun{
#' library(h2o)
#' library(h2otools) #for h2o.get_ids() function
#' library(autoEnsemble)
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
#' }
#' @export

evaluate <- function(id, newdata = NULL, ...) {

  # collect the models' measures
  results <- NULL
  z <- 0
  pb <- txtProgressBar(z, length(id), style = 3)

  for (i in id) {
    perf <- h2o::h2o.performance(model = h2o::h2o.getModel(i), newdata = newdata, ...)

    if (is.null(perf)) {
      #if (xval) stop("cross-validation metrics failed. did you specify 'nfolds' correctly?")
      stop("cross-validation metrics failed. did you specify 'nfolds' correctly?")
    }

    # calculate the model metrics
    auc <- as.numeric(h2o::h2o.auc(perf))
    aucpr <- as.numeric(h2o::h2o.aucpr(perf))
    mcc <- max(h2o::h2o.mcc(perf)[,2], na.rm = TRUE)
    f1point5 <- h2otools::Fmeasure(perf, beta = 1.5, max=TRUE)
    f2 <- h2otools::Fmeasure(perf, beta = 2, max=TRUE)
    f3 <- h2otools::Fmeasure(perf, beta = 3, max=TRUE)
    f4 <- h2otools::Fmeasure(perf, beta = 4, max=TRUE)
    f5 <- h2otools::Fmeasure(perf, beta = 5, max=TRUE)
    kp <- h2otools::kappa(perf, max=TRUE)
    mpce <- h2o::h2o.mean_per_class_error(perf)
    gini <- h2o::h2o.giniCoef(perf)
    accuracy <- max(h2o::h2o.accuracy(perf)[,2])

    res <- as.data.frame(matrix(c(i, auc, aucpr, mcc, f2,
             f1point5, f3, f4, f5, kp,
             mpce, gini, accuracy), nrow = 1))

    colnames(res) <- c("id","auc","aucpr","mcc","f2",
                    "f1point5","f3","f4","f5","kappa",
                    "mean_per_class_error","gini","accuracy")

    results <- rbind(results, as.data.frame(res))

    z <- z + 1
    setTxtProgressBar(pb, z)
  }

  # sort the data
  results <- results[order(results$auc, decreasing = TRUE),]

  class(results) <- c("ensemble.eval", "data.frame")
  return(results)
}


