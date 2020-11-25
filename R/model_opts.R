#' Find Optimal Cutoff to Utilize in Classification Model
#' @param preds
#' @param preds vector of predicted probabilities for response (positive class)
#' @param actual binary vector of actual response
#' @param metric metric to be optimized for. Defaults to accuracy
#' @param custom_func if specified, will use a custom metric to optimize for
#' @return scalar of optimal threshold
#' @export
get.opt_cutoff <- function(preds, actual, metric = "roc", custom_func = NULL) {
  assertthat::assert_that(is.prob(preds), # helper function in utils.R
                          msg = "preds must be numeric vector with values between 0 and 1!")
  assertthat::assert_that(is.binary(actual), # helper function in utils.R
                          msg = "actual must be binary vector!")
  assertthat::assert_that(length(preds) == length(actual),
                          msg = "preds and actual not of the same length!")
  assertthat::assert_that(metric %in% c("acc", "roc", "custom"),
                          msg = "metric must be of c('acc', 'roc', 'custom')!")

  if (!missing(custom_func) | metric == "custom") {
    assertthat::assert_that(
      class(custom_func) == "function" & assertthat::has_args(custom_func, c("preds.class", "actual"), exact = TRUE),
      msg = "custom_func must be of class function(preds.class, actual)!")
    message("utilizing custom function...")
    metric <- "custom"
  }

  cutoff <- c(length(actual):0)/length(actual)
  metric_values <- sapply(cutoff, function(x){
    preds.class <- ifelse(preds >= x, 1, 0)
    switch(metric,
           acc = eval.acc(preds.class, actual),
           roc = 1/abs(eval.sens(preds.class, actual) - eval.spec(preds.class, actual)),
           custom = custom_func(preds.class, actual)
           )
  })
  opt_cutoff <- cutoff[metric_values == max(metric_values)]
  if (length(opt_cutoff) > 1) warning("more than one optimal cutoffs returned!")
  return(opt_cutoff)
}
