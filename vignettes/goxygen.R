## ----setup, include = FALSE-----------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE-----------------------------------------------------------------------------------------------
#  setwd(tempdir())

## ----eval=FALSE-----------------------------------------------------------------------------------------------
#  # copy the folder containing a simple dummy model with goxygen comments
#  file.copy(from = system.file("dummymodel-plain",package="goxygen"), to = ".", recursive = TRUE)

## ----eval=FALSE-----------------------------------------------------------------------------------------------
#  goxygen::goxygen(path = "dummymodel-plain/", cff = "HOWTOCITE.cff")

## ----eval=FALSE-----------------------------------------------------------------------------------------------
#  # copy all files and folders containing the modular dummy model
#  file.copy(from = system.file("dummymodel",package="gms"), to = ".", recursive = TRUE)

## ----eval=FALSE-----------------------------------------------------------------------------------------------
#  goxygen::goxygen(path = "dummymodel/", cff = "HOWTOCITE.cff")
