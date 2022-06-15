##################################################
## Project: qHTSWaterfall
## Script purpose: Server launch support qHTSWaterfall deployment
## Date: 5/15/2022
## Authors: Bryan Queme, John Braisted
## Institute: National Center for Advancing Translational Sciences, NCATS
## National Institutes of Health
##################################################

options(shiny.autoload.r = FALSE)
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
print("loaded package")
runWaterfall()
print("running app")

