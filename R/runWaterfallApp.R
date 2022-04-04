#' Runs the qHTSWaterfall Shiny Application User Interface
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

