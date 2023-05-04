#' @title Evaluate H2O Model(s) Performance
#' @description Multiple model performance metrics are computed
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom h2o h2o.getModel h2o.performance h2o.auc h2o.aucpr h2o.mcc
#'             h2o.F2 h2o.mean_per_class_error h2o.giniCoef h2o.accuracy
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
#' h2o.init(ignore_config = TRUE, nthreads = 2, bind_to_localhost = FALSE, insecure = TRUE)
#' prostate_path <- system.file("extdata", "prostate.csv", package = "h2o")
#' prostate <- h2o.importFile(path = prostate_path, header = TRUE)
#' y <- "CAPSULE"
#' prostate[,y] <- as.factor(prostate[,y])  #convert to factor for classification
#' aml <- h2o.automl(y = y, training_frame = prostate, max_runtime_secs = 30)
#'
#' # evaluate the model performance
#' perf <- h2o.performance(aml@leader, xval = TRUE)
#'
#' # evaluate F-Measure for a Beta = 3
#' kappa(perf, max = TRUE)
#' }
#' @export

modelSelection <- function(eval,
                           family = "binary",
                           top_rank = 0.01,
                           max = round(0.5*top_rank*nrow(eval)),
                           #min = round(max/2),
                           model_selection_criteria = c("auc",
                                                        "aucpr",
                                                        "mcc",
                                                        "f2"
                                                        #"f4",
                                                        #"mean_per_class_error",
                                                        #"kappa"
                                                        )
                           ) {

  results <- NULL
  #print(paste("min:",min, "  max:",max))

  if (family == "binary") {
    for (i in model_selection_criteria) {
      if (i != "mean_per_class_error") {
        print(i)
        eval[,i] <- as.numeric(eval[,i])
        sub <- eval[order(eval[,i], decreasing = T), ]
        maxim <- sub[1, i]
        minim <- maxim - (maxim*top_rank)
        #print(length(sub[sub[,i] >= minim,"id"]))
        lnth <- length(sub[sub[,i] >= minim,"id"])
        #if (lnth < min) results <- c(results, sub[1:min,"id"])
        if (lnth > max) results <- c(results, sub[1:max,"id"])
        else results <- c(results, sub[sub[,i] >= minim,"id"])
        #print(paste("length of results:", length(results)))
      }
      else if (i == "mean_per_class_error") {
        #print(i)
        eval[,i] <- as.numeric(eval[,i])
        sub <- eval[order(eval[,i], decreasing = F), ]
        minim <- sub[1, i]
        maxim <- minim + (minim*top_rank)
        #print(length(sub[sub[,i] <= maxim,"id"]))
        lnth <- length(sub[sub[,i] <= maxim,"id"])
        #if (lnth < min) results <- c(results, sub[1:min,"id"])
        if (lnth > max) results <- c(results, sub[1:max,"id"])
        else results <- c(results, sub[sub[,i] <= max,"id"])
        #print(paste("length of results:", length(results)))
      }
    }
  }
  else {
    stop("currently only 'binary' models are supported")
  }

  return(unique(results))
}


