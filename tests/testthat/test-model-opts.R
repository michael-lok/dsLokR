context("model_opts")

preds <- runif(100)
actual <- sample(c(0:1), 100, replace = T)

test_that("get.opt_cutoff returns numeric vector",
          expect(ok = is.numeric(get.opt_cutoff(preds, actual)),
                 failure = "get.opt_cutoff does not return numeric vector!"))
