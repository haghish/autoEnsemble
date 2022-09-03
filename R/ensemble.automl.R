
#' @importFrom utils setTxtProgressBar txtProgressBar


automl.evaluate <- function(automl, newdata = NULL,
                            train = FALSE, valid = FALSE,
                            xval = FALSE) {

  # if no data is provided, an train and valid are FALSE,
  # then report cross validation (or return an error)
  if (is.null(newdata) & !train & !valid) xval <- TRUE

  # get the models' names
  modelName <- automl.names(automl)

  return(evaluate(id=modelName,
                  newdata = newdata,
                  train = train,
                  valid = valid,
                  xval = xval))
}


# a <- automl.evaluate(aml, xval = TRUE)
# aml@leaderboard
# View(a)
# automl.names(aml)
