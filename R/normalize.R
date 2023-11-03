#' @author E. F. Haghish
#' @export
normalize <- function(x, min=NULL, max=NULL) {                              # Create user-defined function
  if (is.null(min)) min <- min(x)
  if (is.null(max)) max <- max(x)
  return((x - min) / (max - min))                                           # Return normalized data
}
