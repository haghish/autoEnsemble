

library(h2o)
h2o.init()
prostate_path <- system.file("extdata", "prostate.csv", package = "h2o")
prostate <- h2o.importFile(path = prostate_path, header = TRUE)
y <- "CAPSULE"
prostate[,y] <- as.factor(prostate[,y])  #convert to factor for classification
aml <- h2o.automl(y = y,
                  training_frame = prostate,
                  max_runtime_secs = 20)
lb <- h2o.get_leaderboard(aml)
head(lb)
