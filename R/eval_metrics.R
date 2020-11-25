#' Evaluate Area Under ROC Curve
#' @details algorithm comes from \url{https://blog.revolutionanalytics.com/2017/03/auc-meets-u-stat.html},
#' also from \url{https://github.com/mlr-org/mlr/blob/e710dd0ac84b5a6c8197f8902b330d5c5c92b26a/R/measures.R#L778}
#' @param preds vector of predicted probabilities for response (positive class)
#' @param actual binary vector of actual response
#' @importFrom data.table frankv
#' @return numeric calculation of AUC value
#' @export
eval.auc <- function(preds, actual) {
  assertthat::assert_that(is.prob(preds), # helper function in utils.R
                          msg = "preds must be numeric vector with values between 0 and 1!")
  assertthat::assert_that(is.binary(actual), # helper function in utils.R
                          msg = "actual must be binary vector!")
  assertthat::assert_that(length(preds) == length(actual),
                          msg = "preds and actual not of the same length!")

  i <- actual == 1
  if (length(unique(i)) < 2L) {
    warning("actual only contains one level; unable to calculate AUC")
  }
  if (length(i) > 5000L) {
    r <- data.table::frankv(preds)
  }
  else {
    r <- rank(preds)
  }
  n_pos <- sum(i)
  n_neg <- length(i) - n_pos

  (sum(r[i]) - n_pos * (n_pos + 1)/2)/(n_pos * n_neg)
}
