#' @title Evaluate SHAP Values of Selected Models
#' @description Weighted average of SHAP values of selected models
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom h2o h2o.stackedEnsemble h2o.getModel h2o.auc h2o.aucpr h2o.mcc
#'             h2o.F2 h2o.mean_per_class_error h2o.giniCoef h2o.accuracy
# @importFrom h2otools h2o.get_ids
#' @importFrom curl curl
#' @author E. F. Haghish
#'
#' @export

shapleySummary <- function(models,
                           newdata = NULL,
                           sample_size = 100,
                           plot = TRUE,
                           legendstyle = "continuous",
                           scale_colour_gradient = NULL,
                           family = "binary",
                           model_selection_criteria = c("aucpr")
                           # seed = -1,
                           # verbatim = FALSE
) {

  library(ggplot2)
  library(grid)

  # Syntax check
  # ============================================================
  if (length(scale_colour_gradient) != 3 & !is.null(scale_colour_gradient)) {
    stop("'scale colour_gradient' must be a vector of length 3 or NULL")
  }

  # STEP 0: prepare the models
  # ============================================================
  if (inherits(models,"H2OAutoML") | inherits(models,"H2OAutoML")) {
    ids <- h2o.get_ids(models)
  }
  else if (inherits(models,"character")) {
    ids <- models
  }

  w <- NULL
  results <- NULL
  z <- 0
  pb <- txtProgressBar(z, length(ids), style = 3)

  # STEP 1: Evaluate the models for various criteria
  # ============================================================
  # This is already done before the autoEnsemble model is built and
  # should not be repeated. The model evaluation should already exist somewhere

  # STEP 2: Draw a sample
  # ============================================================

  # STEP 3: Extract the SHAP values
  # ============================================================
  for (i in ids) {
    z <- z + 1
    model <- h2o.getModel(i)
    m <- h2o.shap_summary_plot(
      model = model,
      newdata = newdata,
      columns = NULL #get SHAP for all columns
      #top_n_features = 5
      #sample_size = 100
    )

    # Extract the performance metrics
    # ----------------------------------------------------------
    w <- c(w, h2o.aucpr(model))
    W <<- w

    # create the summary dataset
    # ----------------------------------------------------------
    if (z == 1) {
      data <- m$data #reserve the first model's data
      results <- data[, c("Row.names", "feature", "contribution")]
      results <- results[order(results$Row.names), ]
      #results$w1 <- w #this will be the performance metric of the model
      MODEL <<- m
    }
    else {
      holder <- m$data[, c("Row.names", "contribution")]
      #holder$w <- w #this will be the performance metric of the model
      colnames(holder) <- c("Row.names", paste0("contribution", z))
      holder <- holder[order(holder$Row.names), ]
      results <- cbind(results, holder[, 2, drop = FALSE])
    }

    setTxtProgressBar(pb, z)
  }

  datasource    <<- data
  results <<- results

  # Calculate the summary shap values
  # ============================================================
  summaryShaps <- data.frame(
    feature = unique(results$feature),
    mean = NA,
    sd = NA,
    ci = NA
  )
  for (j in unique(results$feature)) {
    tmp <- results[results$feature == j, grep("^contribution", names(results))]
    tmp2 <<- tmp
    tmp <- colSums(abs(tmp))
    tmp3 <<- tmp

    weighted_mean <- weighted.mean(tmp, w)
    weighted_var  <- sum(w * (tmp - weighted_mean)^2) / (sum(w) - 1)
    weighted_sd   <- sqrt(weighted_var)


    summaryShaps[summaryShaps$feature == j, "mean"] <- weighted_mean #mean(tmp)
    summaryShaps[summaryShaps$feature == j, "sd"] <- weighted_sd
    summaryShaps[summaryShaps$feature == j, "ci"] <- 1.96 * weighted_sd / sqrt(length(tmp))
  }

  # NORMALIZE the SHAP contributions and their CI
  # ============================================================
  # the minimum contribution should not be normalized as zero. instead,
  # it should be the ratio of minimum value to the maximum value.
  # The maximum would be the highest mean + the highest CI
  max  <- max(summaryShaps$mean + summaryShaps$ci)
  min  <- min(summaryShaps$mean)/max
  summaryShaps$normalized_mean <- (summaryShaps$mean - min) / (max - min)
  summaryShaps$normalized_ci <- (summaryShaps$ci - min) / (max - min)

  SS <<- summaryShaps

  # TEST DRIVE
  # ============================================================
  # calculate the mean contribution
  mydata <- MODEL$data
  mydata <- mydata[order(mydata$Row.names), ]
  mydata <- results[, grep("^contribution", names(results))]


  meancontribution <- rowMeans(mydata)

  mydata <<- mydata
  meancontribution <<- meancontribution

  MODEL2 <- MODEL
  MODEL2$data <- MODEL2$data[order(MODEL2$data$Row.names), ]
  MODEL2$data$contribution <- meancontribution



  # STEP 3: PLOT
  # ============================================================
  SS$feature <- factor(SS$feature, levels = SS$feature[order(SS$normalized_mean)])
  MODEL <- ggplot(SS, aes(x = feature, y = normalized_mean)) +
    # Plotting the mean values as bars
    geom_col(fill = "#07B86B", alpha = 0.8) +  # Set color to blue and opacity to 0.5
    geom_errorbar(aes(ymin = normalized_mean - normalized_ci,
                      ymax = normalized_mean + normalized_ci),
                  width = 0.2, color = "#7A004BF0",
                  alpha = 0.75, size = 0.7) +
    coord_flip() +  # Rotating the graph to have mean values on X-axis

    ggtitle("") +
    xlab("Features\n") +
    ylab("\nMean absolute SHAP contributions with 95% CI") +
    theme_classic() +
    theme(

      #legend.margin=margin(grid::unit(0,"cm")),

      plot.margin = margin(t = -0.5, r = .25, b = .25, l = .25, unit = "cm")  # Reduce top plot margin
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)))  # Set lower limit of expansion to 0






  # To plot or not to plot! That is the question...
  # ============================================================
  if (plot) print(MODEL)

  return(list(model = MODEL,
              ids = ids,
              shapley_contributions_by_ids = results)
         )
}



#shap(aml, newdata = prostate)
ids <- h2o.get_ids(aml)
a <- shapleySummary(models = ids,
          newdata = prostate
          )
a



