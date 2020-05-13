library(testthat)
library(goxygen)

context("goxygen")

test_that("extract documentation from modular dummy model", {
  docfolder <- paste(tempdir(),"/doc_modular")
  out <- try(goxygen(path = system.file("dummymodel",package="goxygen"),
                     docfolder = docfolder, cff = "HOWTOCITE.cff"))
  expect_null(out)
  expect_true(file.exists(paste0(docfolder,"/html/index.htm")))
  expect_true(file.exists(paste0(docfolder,"/html/01_fancymodule.htm")))
  expect_true(file.exists(paste0(docfolder,"/html/02_crazymodule.htm")))
  expect_true(file.exists(paste0(docfolder,"/documentation.tex")))
})

test_that("cache and unknown output", {
  docfolder <- paste(tempdir(),"/doc_modular")
  expect_warning(out <- try(goxygen(path = system.file("dummymodel",package="goxygen"),
                     docfolder = docfolder, cache = TRUE, output="bla")))
  expect_null(out)
})

test_that("extract documentation from simple dummy model", {
  docfolder <- paste(tempdir(),"/doc_simple")
  out <- try(goxygen(path = system.file("dummymodel",package="goxygen"),
                     docfolder = docfolder, modularCode = FALSE,
                     cff = "HOWTOCITE.cff"))
  expect_null(out)
  expect_true(file.exists(paste0(docfolder,"/html/index.htm")))
  expect_true(file.exists(paste0(docfolder,"/html/modules_01_fancymodule_default_calculations.htm")))
  expect_true(file.exists(paste0(docfolder,"/html/modules_02_crazymodule_module.htm")))
  expect_true(file.exists(paste0(docfolder,"/documentation.tex")))
})