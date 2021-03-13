context("eval_metrics")

preds <- runif(100)
preds.class <- sample(c(0, 1), size = 100, replace = TRUE)
actual <- sample(c(0, 1), size = 100, replace = TRUE)

test_that("eval.auc works correctly",
          expect(
            ok = eval.auc(preds, actual) <= 1 & eval.auc(preds, actual) >= 0,
            failure = "AUC does not fall between 0 and 1!"))

test_that("eval.sens and eval.spec work correctly",
          expect(
            ok = eval.sens(preds.class, actual) <= 1 &
              eval.sens(preds.class, actual) >= 0 &
              eval.spec(preds.class, actual) <= 1 &
              eval.spec(preds.class, actual) >= 0,
            failure = "eval.sens and/or eval.spec does not fall between 0 and 1!"))

preds <- runif(10)
actual <- sample(c(0, 1), size = 9, replace = TRUE)

test_that("eval.auc checks the length of arguments",
          expect_error(object = eval.auc(preds, actual)))

test_that("eval.sens checks the length of arguments",
          expect_error(object = eval.sens(preds.class, actual)))

test_that("eval.spec checks the length of arguments",
          expect_error(object = eval.auc(preds.class, actual)))

preds <- c(1:10)
preds.class <- runif(10)
actual <- sample(c(0, 1), size = 10, replace = TRUE)

test_that("eval.auc ensures preds arg contains probabilities",
          expect_error(eval.auc(preds, actual)))

test_that("eval.sens ensures preds.class arg contains binary vector",
          expect_error(eval.sens(preds.class, actual)))

test_that("eval.spec ensures preds.class arg contains binary vector",
          expect_error(eval.spec(preds.class, actual)))

preds <- runif(10)
actual <- sample(c(3:5), size = 10, replace = TRUE)

test_that("eval.auc ensures actual is binary vector",
          expect_error(eval.auc(preds, actual)))
