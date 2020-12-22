library(testthat)
library(goxygen)

context("goxygen")

skip_if_not(check_pandoc(error=FALSE))

test_that("extract documentation from modular dummy model", {
  docfolder <- paste0(tempdir(),"/doc_modular")
  expect_null(goxygen(path = system.file("dummymodel",package="gms"),
                      docfolder = docfolder, includeCore = TRUE, cff = "HOWTOCITE.cff"))
  expect_true(file.exists(paste0(docfolder,"/html/index.htm")))
  expect_true(file.exists(paste0(docfolder,"/html/core.htm")))
  expect_true(file.exists(paste0(docfolder,"/html/01_fancymodule.htm")))
  expect_true(file.exists(paste0(docfolder,"/html/02_crazymodule.htm")))
  expect_true(file.exists(paste0(docfolder,"/documentation.tex")))
})

test_that("extract HTML documentation from modular dummy model with classic style", {
  docfolder <- paste0(tempdir(),"/doc_modular_classic")
  out <- try(goxygen(path = system.file("dummymodel",package="gms"),
                     htmlStyle = "classic", output="html",
                     docfolder = docfolder, includeCore = TRUE, cff = "HOWTOCITE.cff"))
  expect_null(out)
  expect_true(file.exists(paste0(docfolder,"/html/index.htm")))
  expect_true(file.exists(paste0(docfolder,"/html/core.htm")))
  expect_true(file.exists(paste0(docfolder,"/html/01_fancymodule.htm")))
  expect_true(file.exists(paste0(docfolder,"/html/02_crazymodule.htm")))
})



test_that("cache and unknown output", {
  docfolder <- paste0(tempdir(),"/doc_modular")
  expect_warning(out <- try(goxygen(path = system.file("dummymodel",package="gms"),
                     docfolder = docfolder, includeCore = TRUE, cache = TRUE, output="bla")))
  expect_null(out)
})

test_that("extract documentation from simple dummy model", {
  docfolder <- paste0(tempdir(),"/doc_simple")
  out <- try(goxygen(path = system.file("dummymodel",package="gms"),
                     docfolder = docfolder, modularCode = FALSE,
                     cff = "HOWTOCITE.cff"))
  expect_null(out)
  expect_true(file.exists(paste0(docfolder,"/html/index.htm")))
  expect_true(file.exists(paste0(docfolder,"/html/modules_01_fancymodule_default_calculations.htm")))
  expect_true(file.exists(paste0(docfolder,"/html/modules_02_crazymodule_module.htm")))
  expect_true(file.exists(paste0(docfolder,"/documentation.tex")))
})