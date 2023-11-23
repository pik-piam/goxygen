skip_if_not(check_pandoc())

test_that("extract documentation from modular dummy model", {
  docfolder <- file.path(withr::local_tempdir(), "doc_modular")

  out <- suppressWarnings(try(goxygen(path = system.file("dummymodel", package = "gms"), output = c("html", "tex"),
                                      docfolder = docfolder, includeCore = TRUE,
                                      cff = "HOWTOCITE.cff", DoNotPlot = TRUE)))
  expect_null(out)
  expect_true(file.exists(file.path(docfolder, "html", "index.htm")))
  expect_true(file.exists(file.path(docfolder, "html", "core.htm")))
  expect_true(file.exists(file.path(docfolder, "html", "01_fancymodule.htm")))
  expect_true(file.exists(file.path(docfolder, "html", "02_crazymodule.htm")))
  expect_true(file.exists(file.path(docfolder, "html", "crazy.htm")))
  expect_true(file.exists(file.path(docfolder, "html", "settings.htm")))
  expect_true(file.exists(file.path(docfolder, "documentation.tex")))
})

test_that("extract HTML documentation from modular dummy model with classic style", {
  docfolder <- file.path(withr::local_tempdir(), "doc_modular_classic")

  out <- suppressWarnings(try(goxygen(path = system.file("dummymodel", package = "gms"),
                                      htmlStyle = "classic", output = "html",
                                      docfolder = docfolder, includeCore = TRUE,
                                      cff = "HOWTOCITE.cff", DoNotPlot = TRUE)))
  expect_null(out)
  expect_true(file.exists(file.path(docfolder, "html", "index.htm")))
  expect_true(file.exists(file.path(docfolder, "html", "core.htm")))
  expect_true(file.exists(file.path(docfolder, "html", "01_fancymodule.htm")))
  expect_true(file.exists(file.path(docfolder, "html", "02_crazymodule.htm")))
  expect_true(file.exists(file.path(docfolder, "html", "crazy.htm")))
  expect_true(file.exists(file.path(docfolder, "html", "settings.htm")))
})

test_that("cache and unknown output", {
  docfolder <- file.path(withr::local_tempdir(), "doc_modular")
  expect_warning({
    out <- try(goxygen(path = system.file("dummymodel", package = "gms"), docfolder = docfolder,
                       includeCore = TRUE, cache = TRUE, output = "bla", DoNotPlot = TRUE))
  })
  expect_null(out)
})

test_that("extract documentation from simple dummy model", {
  docfolder <- file.path(withr::local_tempdir(), "doc_simple")
  out <- try(goxygen(path = system.file("dummymodel", package = "gms"),
                     docfolder = docfolder, modularCode = FALSE,
                     output = c("html", "tex"), cff = "HOWTOCITE.cff", DoNotPlot = TRUE))
  expect_null(out)
  expect_true(file.exists(file.path(docfolder, "html", "index.htm")))
  expect_true(file.exists(file.path(docfolder, "html", "modules_01_fancymodule_default_calculations.htm")))
  expect_true(file.exists(file.path(docfolder, "html", "modules_02_crazymodule_module.htm")))
  expect_true(file.exists(file.path(docfolder, "html", "crazy.htm")))
  expect_true(file.exists(file.path(docfolder, "html", "settings.htm")))
  expect_true(file.exists(file.path(docfolder, "documentation.tex")))
})
