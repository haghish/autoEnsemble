


#' @importFrom h2o h2o.getModel h2o.performance h2o.auc h2o.aucpr h2o.mcc
#'             h2o.F2 h2o.mean_per_class_error h2o.giniCoef h2o.accuracy
#' @importFrom h2otools Fmeasure



#' @importFrom utils setTxtProgressBar txtProgressBar


evaluate <- function(id, newdata = NULL,
                     train = FALSE, valid = FALSE,
                     xval = FALSE) {

  # if no data is provided, an train and valid are FALSE,
  # then report cross validation (or return an error)
  if (is.null(newdata) & !train & !valid) xval <- TRUE

  # collect the models' measures
  results <- NULL
  z <- 0
  pb <- txtProgressBar(z, length(id), style = 3)

  for (i in id) {
    perf <- h2o::h2o.performance(h2o::h2o.getModel(i),
                                 newdata = newdata,
                                 train = train,
                                 valid = valid,
                                 xval = xval)

    # calculate the model metrics
    auc <- h2o::h2o.auc(perf)
    aucpr <- h2o::h2o.aucpr(perf)
    mcc <- max(h2o::h2o.mcc(perf)[,2])
    f1point5 <- h2otools::Fmeasure(perf, beta = 1.5, max=TRUE)
    f2 <- max(h2o::h2o.F2(perf)[,2])
    f3 <- h2otools::Fmeasure(perf, beta = 3, max=TRUE)
    f4 <- h2otools::Fmeasure(perf, beta = 4, max=TRUE)
    f5 <- h2otools::Fmeasure(perf, beta = 5, max=TRUE)
    mpce <- h2o::h2o.mean_per_class_error(perf)
    gini <- h2o::h2o.giniCoef(perf)
    accuracy <- max(h2o::h2o.accuracy(perf)[,2])

    res <- c(i, auc, aucpr, mcc, f1point5, f2, f3, f4, f5,
             mpce, gini, accuracy)

    names(res) <- c("id","auc","aucpr","mcc","f1point5",
                    "f2","f3","f4","f5","mpce","gini","accuracy")

    results <- rbind(results, res)

    z <- z + 1
    setTxtProgressBar(pb, z)
  }

  return(as.data.frame(results))
}


# a <- automl.evaluate(aml, xval = TRUE)
# aml@leaderboard
# View(a)
# automl.names(aml)
