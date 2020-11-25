#' @title Binary Classification Model Evaluation Metrics
#' @description access to commonly-used metric calculations
#' \itemize{
#'  \item \code{eval.auc} calculates area under ROC curve using algorithm from \url{https://blog.revolutionanalytics.com/2017/03/auc-meets-u-stat.html}
#'  \item \code{eval.sens} calculates true positive rate \eqn{TPR = TP / (TP + FN)}
#'  \item \code{eval.spec} calculates true negative rate \eqn{TNR = TN / (TN + FP)}
#' }
#' @name eval_metrics
#' @param preds vector of predicted probabilities for response (positive class)
#' @param preds.class binary vector of predicted response values
#' @param actual binary vector of actual response
#' @importFrom data.table frankv
#' @return numeric calculation of specified metric
NULL

#' @rdname eval_metrics
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

#' @rdname eval_metrics
#' @export
eval.sens <- function(preds.class, actual) {
  assertthat::assert_that(is.binary(actual) & is.binary(preds.class), # helper function in utils.R
                          msg = "both preds.class and actual must be binary vector!")
  assertthat::assert_that(length(preds.class) == length(actual),
                          msg = "preds and actual not of the same length!")

  with(data.frame(preds = preds.class,
                  actual = actual),
       sum(preds == actual & actual == 1)/sum(actual == 1))
}

#' @rdname eval_metrics
#' @export
eval.spec <- function(preds.class, actual) {
  assertthat::assert_that(is.binary(actual) & is.binary(preds.class), # helper function in utils.R
                          msg = "both preds.class and actual must be binary vector!")
  assertthat::assert_that(length(preds.class) == length(actual),
                          msg = "preds and actual not of the same length!")

  with(data.frame(preds = preds.class,
                  actual = actual),
       sum(preds == actual & actual == 0)/sum(actual == 0))
}

