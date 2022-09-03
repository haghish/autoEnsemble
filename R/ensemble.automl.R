

automl.evaluate <- function(automl, validation_frame = NULL) {

  # get the models' names
  modelName <- automl.names(automl)

  print(modelName)

  # collect the models' measures
  results <- NULL
  for (i in modelName) {
    perf <- h2o::h2o.performance(h2o::h2o.getModel(i),
                                 newdata = validation_frame)

    # calculate auc, aucpr, f2, f3, f4, f5, f6, mcc, mpce
    auc <- h2o::h2o.auc(perf)
    aucpr <- h2o::h2o.aucpr(perf)
    mcc <- max(h2o::h2o.mcc(perf)[,2])
    f1point5 <- h2otools::Fmeasure(perf, beta = 1.5, max=TRUE)
    f2 <- max(h2o::h2o.f2(perf)[,2])
    f3 <- h2otools::Fmeasure(perf, beta = 3, max=TRUE)
    f4 <- h2otools::Fmeasure(perf, beta = 4, max=TRUE)
    f5 <- h2otools::Fmeasure(perf, beta = 5, max=TRUE)

    res <- c(i, auc, aucpr, mcc, f1point5, f2, f3, f4, f5)
    results <- rbind(results, res)
  }

  return(results)
}


#ensemble.automl(auto)
