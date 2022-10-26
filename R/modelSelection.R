modelSelection <- function(eval,
                           family = "binary",
                           top = 0.01,
                           max = round(0.5*top*nrow(eval)),
                           #min = round(max/2),
                           criteria = c("auc",
                                        "aucpr",
                                        "mcc",
                                        "f2"
                                        #"f4",
                                        #"mean_per_class_error",
                                        #"kappa"
                           )) {

  results <- NULL
  #print(paste("min:",min, "  max:",max))

  if (family == "binary") {
    for (i in criteria) {

      if (i != "mean_per_class_error") {
        #print(i)
        eval[,i] <- as.numeric(eval[,i])
        sub <- eval[order(eval[,i], decreasing = T), ]
        maxim <- sub[1, i]
        minim <- maxim - (maxim*top)
        #print(length(sub[sub[,i] >= minim,"id"]))
        lnth <- length(sub[sub[,i] >= minim,"id"])
        #if (lnth < min) results <- c(results, sub[1:min,"id"])
        if (lnth > max) results <- c(results, sub[1:max,"id"])
        else results <- c(results, sub[sub[,i] >= minim,"id"])
        #print(paste("length of results:", length(results)))
      }
      else if (i == "mean_per_class_error") {
        #print(i)
        eval[,i] <- as.numeric(eval[,i])
        sub <- eval[order(eval[,i], decreasing = F), ]
        minim <- sub[1, i]
        maxim <- minim + (minim*top)
        #print(length(sub[sub[,i] <= maxim,"id"]))
        lnth <- length(sub[sub[,i] <= maxim,"id"])
        #if (lnth < min) results <- c(results, sub[1:min,"id"])
        if (lnth > max) results <- c(results, sub[1:max,"id"])
        else results <- c(results, sub[sub[,i] <= max,"id"])
        #print(paste("length of results:", length(results)))
      }
    }
  }

  return(unique(results))
}
