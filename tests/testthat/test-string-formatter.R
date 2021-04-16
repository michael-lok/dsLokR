context("string_formatter")

txt.test = "str_form is a {{adj}} function."
test_that("str_form errors on missing optional arguments",
          expect_error(
            str_form(txt.test, "failing"),
            "missing keys from optional args!",
            ignore.case = TRUE
          ))

test_that("str_form errors with unnamed optional arguments",
          expect_error(
            str_form(txt.test, adj = "partially operational", "failed"),
            "optional arguments must be key/value pairs!",
            ignore.case = TRUE
          ))

test_that("str_form errors with extra optional arguments",
          expect_error(
            str_form(txt.test, adj = "correct", problem = "excessive"),
            "extra keys in optional args!",
            ignore.case = TRUE))

test_that("str_form errors when values contain NA values",
          expect_error(
            str_form(txt.test, adj = NA),
            "NA values found!"))

test_that("str_form errors when values contain NULL values",
          expect_error(
            str_form(txt.test, adj = NULL),
            "NULL values found!"))

test_that("str_form errors when there are duplicated keys",
          expect_error(
            str_form(txt.test, adj = "okay", adj = "duplicated"),
            "duplicated keys!",
            ignore.case = TRUE))

test_that("str_form correctly substitutes string",
          expect(
            ok = str_form(txt.test, adj = "working") == "str_form is a working function.",
            failure = "value not correctly substituted!"))


txt.test <- "A {{creature}} has {{number}} legs and {{number}} eyes"
test_that("str_form substitutes each key found in text string",
          expect(
            ok = str_form(txt.test, creature = "spider", number = 8) == "A spider has 8 legs and 8 eyes",
            failure = "unable to substitute all values in text string!"))
