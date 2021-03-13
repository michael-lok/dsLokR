context("model_sampling")

response <- sample(c(0:1), size = 1000, replace = T)
strat.good <- sample.strat(response)

test_that("sample.strat includes approximately 70% of response records",
          expect(ok = all.equal(target = 700,
                                current = length(strat.good),
                                tolerance = 5),
                 failure = "strat.good does not contain approximately 70%% of response records!"))

test_that("sample.strat will include approximately 70% of each class",
          expect(ok = all.equal(target = c(0.7, 0.7),
                                current = as.numeric(table(response[strat.good])/table(response)),
                                tolerance = 0.1),
                 failure = "strat.good does not contain approximately 70% of each class!"))

test_that("sample.strat will error if all response values are unique",
          expect_error(sample.strat(c(0, 1))))
