

automl.names <- function(automl) {
  return(as.data.frame(automl@leaderboard$model_id)[,1])
}
