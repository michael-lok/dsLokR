context("eval_metrics")

preds <- runif(100)
actual <- sample(c(0, 1), size = 100, replace = TRUE)

test_that("eval.auc works correctly",
          expect(
            ok = eval.auc(preds, actual) <= 1 & eval.auc(preds, actual) >= 0,
            failure = "AUC does not fall between 0 and 1!"))

preds <- runif(10)
actual <- sample(c(0, 1), size = 9, replace = TRUE)

test_that("eval.auc checks the length of arguments",
          expect_error(object = eval.auc(preds, actual)))

preds <- c(1:10)
actual <- sample(c(0, 1), size = 10, replace = TRUE)

test_that("eval.auc ensures preds arg contains probabilities",
          expect_error(eval.auc(preds, actual)))

preds <- runif(10)
actual <- sample(c(3:5), size = 10, replace = TRUE)

test_that("eval.auc ensures actual is binary vector",
          expect_error(eval.auc(preds, actual)))

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
