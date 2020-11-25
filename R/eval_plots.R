#' Generate Receiver Operating Characteristic Object
#' @details object is plottable
#' @param preds vector of predicted probabilities for response (positive class)
#' @param actual binary vector of actual response
#' @param plot whether to return ROC curve plot; defaults to `TRUE``
#' @import ggplot2
#' @return
#'
#' when plot is set to `TRUE`, a ggplot object that visualizes ROC curve
#' \itemize{
#'  \item \code{cutoff} will be displayed in color as gradient on the curb
#'  \item \code{tpr} will be placed on the y-axis representing true positive rate
#'  \item \code{fpr} will be placed on the x-axis representing false positive rate
#' }
#'
#' when plot is set to `FALSE`, a data.frame object
#' \itemize{
#'  \item \code{cutoff} different cutoffs utilized to evaluate performance
#'     (determined by)
#'  \item \code{tpr} true positive rate \eqn{TP/(TP + FN)}
#'  \item \code{fpr} false positive rate \eqn{FP/(TN + FP)}
#' }
#' @export
eval.roc <- function(preds, actual, plot = TRUE) {
  assertthat::assert_that(is.prob(preds), # helper function in utils.R
                          msg = "preds must be numeric vector with values between 0 and 1!")
  assertthat::assert_that(is.binary(actual), # helper function in utils.R
                          msg = "actual must be binary vector!")
  assertthat::assert_that(length(preds) == length(actual),
                          msg = "preds and actual not of the same length!")

  size <- length(actual)
  n_pos <- sum(actual == 1)
  n_neg <- length(actual) - n_pos
  roc <- lapply(c(size:0) / size, FUN = function(x) {
    pred.class <- as.integer(preds >= x)
    data.frame(cutoff = x,
               tpr = sum(pred.class == actual & actual == 1) / n_pos,
               fpr = sum(pred.class != actual & pred.class == 1) / n_neg)
  })
  roc <- do.call("rbind", roc)

  if (plot) {
    roc_ggplot <- ggplot(roc, aes(x = fpr, y = tpr, color = cutoff)) +
      geom_step(size = 2)
    return(roc_ggplot)
  } else {
    return(roc)
  }
}
utils::globalVariables(c("cutoff", "fpr", "tpr"))
