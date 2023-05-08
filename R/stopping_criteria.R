#' @title Stopping Criteria for Ending the Search
#' @description Defines criteria for ending the optimization search
#' @param stop_rounds integer. number of stoping rounds, in case the model stops
#'                    improving
#' @param reset_stop_rounds logical. if TRUE, everytime the model improves the
#'                          stopping rounds penalty is resets to 0.
#' @param stop_metric character. model stopping metric. the default is \code{"auc"},
#'                    but \code{"aucpr"} and \code{"mcc"} are also available.
#' @param stop integer. current round of stopping penalty
#' @return a matrix of F-Measures for different thresholds or the highest F-Measure value
#' @author E. F. Haghish



stopping_criteria <- function(stop_rounds = 3,
                              reset_stop_rounds = TRUE,
                              stop_metric = "auc",
                              stop = STOP) {

  AUC       <- 0
  AUCPR     <- 0
  MCC       <- 0
  STOP      <- 0
  STOPauc   <- 0
  STOPaucpr <- 0
  STOPmcc   <- 0

  # evaluate the model for AUC
  # ----------------------------------------------------------
  if ("auc" %in% stop_metric) {
    auc <- as.numeric(h2o::h2o.auc(stopModel))
    if (AUC < auc) {
      AUC <- auc
      TOP <- i
      model <- stopModel
      if (reset_stop_rounds) STOP <- 0
    }
    else STOPauc <- STOPauc + 1
  }

  # evaluate the model for AUCPR
  # ----------------------------------------------------------
  if ("aucpr" %in% stop_metric) {
    aucpr <- as.numeric(h2o::h2o.aucpr(stopModel))
    if (AUCPR < aucpr) {
      AUCPR <- aucpr
      TOP <- i
      model <- stopModel
      if (reset_stop_rounds) STOP <- 0
    }
    else STOPaucpr <- STOPaucpr + 1
  }

  # evaluate the model for MCC
  # ----------------------------------------------------------
  if ("mcc" %in% stop_metric) {
    mcc <- max(h2o::h2o.mcc(stopModel)[,2])
    if (MCC < mcc) {
      MCC <- mcc
      TOP <- i
      model <- stopModel
      if (reset_stop_rounds) STOP <- 0
    }
    else STOPmcc <- STOPmcc + 1
  }

  # Create a data.frame of all available criteria
  # ----------------------------------------------------------

  return(STOP)
}


