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

shap <- function(models,
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

    m <- h2o.shap_summary_plot(
      model = h2o.getModel(i),
      newdata = newdata,
      columns = NULL #get SHAP for all columns
      #top_n_features = 5
      #sample_size = 100
    )

    # Extract the performance metrics
    # ----------------------------------------------------------
    w <- NA

    if (z == 1) {
      data <- m$data #reserve the first model's data
      results <- data[, c("Row.names", "contribution")]
      results <- results[order(results$Row.names), ]
      results$w1 <- w #this will be the performance metric of the model
      MODEL <<- m
    }
    else {
      holder <- m$data[, c("Row.names", "contribution")]
      holder$w <- w #this will be the performance metric of the model
      colnames(holder) <- c("Row.names", paste0("contribution", z), paste0("w", z))
      holder <- holder[order(holder$Row.names), ]
      results <- cbind(results, holder[, 2:3])
    }

    setTxtProgressBar(pb, z)
  }

  datasource    <<- data
  results <<- results

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
  MODEL2 <- MODEL2 +
    ggtitle("") +
    xlab("Features\n") +
    ylab("\nSHAP contribution") +
    theme_classic() +
    labs(colour = "Normalized values") +
    theme(
      legend.position="top",
      legend.justification = "right",
      legend.title.align = 0.5,
      legend.direction = "horizontal",
      legend.text=element_text(colour="black", size=6, face="bold"),
      legend.key.height = grid::unit(0.3, "cm"),
      legend.key.width = grid::unit(1, "cm"),
      #legend.margin=margin(grid::unit(0,"cm")),
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
      plot.margin = margin(t = -0.5, r = .25, b = .25, l = .25, unit = "cm")  # Reduce top plot margin
    ) +
    guides(colour = guide_colourbar(title.position = "top", title.hjust = 0.5))

  if (legendstyle == "continuous") {
    # Set color range
  }

  else if (legendstyle == "categorical") {
    MODEL2 <- MODEL2 +
      guides(colour = guide_legend(title.position = "top",
                                   title.hjust = 0.5,
                                   legend.margin = margin(t = -1, unit = "cm"),
                                   override.aes = list(size = 3)
      )) +
      theme(legend.key.height = grid::unit(0.4, "cm"),
            legend.key.width = grid::unit(0.4, "cm"))
  }

  # Fix the color scale of the model
  # ============================================================
  if (length(scale_colour_gradient) == 3) {
    MODEL2 <- MODEL2 +
      scale_colour_gradient2(low=scale_colour_gradient[1],
                             mid=scale_colour_gradient[2],
                             high=scale_colour_gradient[3],
                             midpoint = 0.5)
  }

  # To plot or not to plot! That is the question...
  # ============================================================
  if (plot) print(MODEL2)

  return(list(model = MODEL2,
              ids = ids))
}


#
# #shap(aml, newdata = prostate)
# a <- shap(as.vector(aml@leaderboard[,1])[1:2],
#           legendstyle = "categorical",
#           newdata = prostate,
#           scale_colour_gradient = c("#07B86B", "#1564AD", "#CC1A44")
#           )
# a



