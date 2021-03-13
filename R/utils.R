#' Check if an object contains probabilities
#' @param pred vector of values to be tested
#' @return boolean of whether input contains valid probability values
is.prob <- function(pred) {
  is.numeric(pred) & max(pred) <= 1 & min(pred) >= 0
}

#' Check if object contains valid response values
#' @param truth object to be tested
#' @return boolean of whether input contains valid response values
is.binary <- function(truth) {
  is.vector(truth) & is.numeric(truth) & all(truth %in% c(0, 1))
}
