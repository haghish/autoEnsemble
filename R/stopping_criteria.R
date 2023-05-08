#' @title Stopping Criteria for Ending the Search
#' @description Defines criteria for ending the optimization search
#' @param df data.frame. includes the metrics of ensemblem model performance
#' @param round integer. the current round of optimization
#' @param stop integer. current round of stopping penalty
#' @param min_improvement numeric. specifies the minimum improvement in model
#'                        evaluation metric to qualify further optimization search.
#' @param stop_rounds integer. number of stoping rounds, in case the model stops
#'                    improving
#' @param reset_stop_rounds logical. if TRUE, everytime the model improves the
#'                          stopping rounds penalty is resets to 0.
#' @param stop_metric character. model stopping metric. the default is \code{"auc"},
#'                    but \code{"aucpr"} and \code{"mcc"} are also available.

#' @return a matrix of F-Measures for different thresholds or the highest F-Measure value
#' @author E. F. Haghish

stopping_criteria <- function(df,
                              round,
                              stop,
                              min_improvement,
                              stop_rounds = 3,
                              reset_stop_rounds = TRUE,
                              stop_metric = "auc") {

  IMPROVED  <- FALSE

  # if there is only one stopping criterion, stop the search
  #    when the 'stop_rounds' are reached without any improvement
  #    if several stopping criteria, stop the search
  #    when the 'stop_rounds' are reached without any improvement
  #    for any of the criterion
  # ----------------------------------------------------------
  for (i in stop_metric) {
    if (df[round, i] > (df[round, i-1]+ min_improvement)) IMPROVED <- TRUE
  }

  # evaluate stopping criteria
  # ----------------------------------------------------------
  if (IMPROVED & reset_stop_rounds) STOP <- 0
  else if (!IMPROVED) STOP <- STOP + 1

  return(list(stop = STOP,
              improved = IMPROVED))
}


