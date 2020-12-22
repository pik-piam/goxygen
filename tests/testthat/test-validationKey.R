library(testthat)
context("validation key")

test_that("buildLibrary validation key is valid", {
  folder <- ifelse(file.exists("DESCRIPTION"),".","../../")
  expect_true(lucode2:::validkey(folder)$valid)
})