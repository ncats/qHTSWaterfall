options(shiny.autoload.r = FALSE)
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
print("loaded package")
runWaterfall()
print("running app")

