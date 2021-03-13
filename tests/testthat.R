library(testthat)
library(dsLokR)

test_check("dsLokR", reporter = JunitReporter$new(file = "results.xml"))
