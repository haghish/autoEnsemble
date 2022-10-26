

elBoost <- function(df, x, y, training_frame, max_models = 20, seed = 54321) {

  for (i in 1:max_models) {
    boot <- sample.int(nrow(df), replace = TRUE)
    hex <- h2o::as.h2o(df[boot, ])
    seed <- seed * i
    ml <- h2o.automl(x = x, y = y,
                     project_name = "elboost",
                     training_frame = hex,
                     balance_classes = TRUE,
                     #max_after_balance_size = max_after_balance_size,
                     nfolds = 10,
                     #max_runtime_secs = 10000,
                     #max_models = NULL, # run all that you can
                     keep_cross_validation_predictions = TRUE,
                     seed = seed,

                     # tuning parameters for automl
                     # ----------------------------
                     include_algos = c("GLM"),

                     # Stopping parameters
                     # ----------------------------
                     stopping_metric = "aucpr",
                     sort_metric = "AUCPR",
                     #stopping_tolerance = 0.0015, #0.00268 recommended
                     stopping_rounds = 3
    )

    # save the components

  }

  ids <- h2o::h2o.getAutoML(project_name = "elboost")



  model <- h2o.stackedEnsemble(x = x,
                               y = y,
                               training_frame = hex,
                               base_models = ids)

  return(model)

}
