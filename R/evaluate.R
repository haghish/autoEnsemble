#' @title Evaluate H2O Model(s) Performance
#' @description Multiple model performance metrics are computed for each model
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom h2o h2o.getModel h2o.performance h2o.auc h2o.aucpr h2o.mcc
#'             h2o.mean_per_class_error h2o.giniCoef h2o.accuracy
#' @importFrom h2otools Fmeasure kappa
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
#' library(h2o)
#' library(h2otools) #for h2o.get_ids() function
#' library(ensemble)
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

evaluate <- function(id, newdata = NULL,
                     train = FALSE, valid = FALSE, xval = FALSE) {

  # # if no data is provided, and train and valid are FALSE,
  # # then report cross validation (or return an error)
  # if (is.null(newdata) & !train & !valid) xval <- TRUE

  # collect the models' measures
  results <- NULL
  z <- 0
  pb <- txtProgressBar(z, length(id), style = 3)

  for (i in id) {
    model <- h2o::h2o.getModel(i)
    perf <- h2o::h2o.performance(model = model,
                                 newdata = newdata,
                                 train = train,
                                 valid = valid,
                                 xval = xval)

    if (is.null(perf)) {
      if (xval) stop("cross-validation metrics failed. did you specify 'nfolds' correctly?")
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


