context("eval_plots")

preds <- runif(10)
actual <- sample(c(0, 1), size = 10, replace = TRUE)

test_that("eval.roc default output is gg/ggplot class",
          expect_s3_class(eval.roc(preds, actual),
                          c("gg", "ggplot"),
                          exact = TRUE))

test_that("eval.roc optional output is data.frame class",
          expect_s3_class(eval.roc(preds, actual, plot = FALSE),
                          c("data.frame"),
                          exact = TRUE))
