##################################################
## Project: qHTSWaterfall
## Script purpose: Server file for qHTSWaterfall
## Date: 5/15/2022
## Authors: Bryan Queme, John Braisted
## Institute: National Center for Advancing Translational Sciences, NCATS
## National Institutes of Health
##################################################


#' Runs the qHTSWaterfall Shiny Application User Interface locally
#' @examples
#' \dontrun{
#' library(qHTSWaterfall)
#' runQHTSWaterfallApp()
#'
#' # use 'Esc' button to exit application.
#'
#' }
#' @export
runQHTSWaterfallApp <- function() {
  appDir <- system.file("shinyApp",package = "qHTSWaterfall")
  shiny::runApp(appDir, display.mode = "normal", test.mode = F, quiet = T, host="127.0.0.1")
}


#' Kicks off the shiny app on a server
#'
#' @export
runWaterfall <- function() {
  appDir <- system.file("shinyApp", package = 'qHTSWaterfall')
  shiny::runApp(appDir, display.mode = "normal")
}



