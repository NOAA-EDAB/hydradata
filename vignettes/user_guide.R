## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(collapse = TRUE,comment = "#>")
library(hydradata)

## ----echo = TRUE,eval=FALSE----------------------------------------------
#  package?hydradata

## ----echo = TRUE---------------------------------------------------------
hydraData <- set_hcr(currentData=hydraData, Nthresholds=5, minMaxThresholds=c(0.1,0.4), minMaxExploitations=c(0.04,0.4), increment=.02)
print(hydraData$exploitationOptions)

## ----echo = TRUE---------------------------------------------------------
print(hydraData$thresholds)

## ----echo = TRUE---------------------------------------------------------
inputs <- setup_default_inputs()
inputs$outDir <- "path/to/your/output/directory"
str(inputs)

## ----echo = TRUE,eval = FALSE--------------------------------------------
#  hydraData <- create_datpin_files(inputs,hydraData)

## ----echo = TRUE,eval = FALSE--------------------------------------------
#  create_documentation(outFile="documentation.html",outDir=getwd(),data = hydraData)

## ----data, echo = FALSE--------------------------------------------------
str(hydraData)

