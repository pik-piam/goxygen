library(testthat)

context("goxygen")

test_that("goxygen extracts dummy model documentation without errors", {
  docfolder <- tempdir()
  out <- try(goxygen(path = system.file("dummymodel",package="goxygen"),
                     docfolder = docfolder))
  expect_null(out)
  expect_true(file.exists(paste0(docfolder,"/html/index.htm")))
  expect_true(file.exists(paste0(docfolder,"/html/01_fancymodule.htm")))
  expect_true(file.exists(paste0(docfolder,"/html/02_crazymodule.htm")))
  expect_true(file.exists(paste0(docfolder,"/documentation.tex")))
})
