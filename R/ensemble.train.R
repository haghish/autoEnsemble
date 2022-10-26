

ensemble.train <- function(ids, training_frame,
                           loaded = TRUE, path = NULL) {

  if (!loaded) {
    for (i in ids) h2o::h2o.loadModel(paste0(path, i))
  }

  # get the models' parameters from trained models:
  params <- h2o::h2o.getModel(ids[1])
  nfolds <- params@parameters$nfolds
  x      <- params@parameters$x
  y      <- params@parameters$y


  model <- h2o.stackedEnsemble(x = x,
                               y = y,
                               training_frame = training_frame,
                               base_models = ids)

  return(model)

}

