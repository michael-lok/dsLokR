#' @title Sampling Methods
#' @description different ways to generate partitions of dataset
#' \itemize{
#'  \item \code{sample.strat} ensures sample has adequate representation of response variable
#' }
#' @name model_sampling
#' @param response vector of response classifications
#' @param pct proportion of response records to include in sample; defaults to 0.7
#' @return vector of indices to pull from response vector
NULL

#' @rdname model_sampling
#' @export
sample.strat <- function(response, pct = 0.7) {
  assertthat::assert_that(is.vector(response),
                          msg = "response must be a vector of response records!")
  assertthat::assert_that(is.numeric(pct) & pct >= 0 & pct <= 1,
                          msg = "pct must be numeric value between 0 and 1!")
  assertthat::assert_that(length(response) > length(unique(response)),
                          msg = "all values in response are unique; unable to stratify")

  unlist(lapply(unique(response), function(x) {
    assertthat::assert_that(sum(response == x) > 1,
                            msg = sprintf("class '%s' only has one instance; unable to stratify!"))
    sample(which(response == x), sum(response == x) * pct)
  }))
}
