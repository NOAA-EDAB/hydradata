## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(collapse = TRUE,comment = "#>")
library(mshydradata)

## ----echo = TRUE,eval=FALSE----------------------------------------------
#  package?mshydradata

## ----echo = TRUE---------------------------------------------------------
inputs <- setup_default_inputs()
str(inputs)

## ----echo = TRUE,eval = FALSE--------------------------------------------
#  hydraData <- create_datpin_files(inputs,hydraData)

## ----echo = TRUE,eval = FALSE--------------------------------------------
#  create_documentation(outFile="documentation.html",outDir=getwd(),data = hydraData)

## ----data, echo = FALSE--------------------------------------------------
str(hydraData)

