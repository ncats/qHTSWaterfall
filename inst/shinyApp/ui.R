library(shiny)
library(shinyjs)
# addReadoutSelector <- function(readouts) {
#   print("HEY I'm adding a checkbox group here, where???")
#
#   defaultColors <- c('darkgreen', 'blue2', 'darkorange', 'deeppink3', 'deepskyblue3', 'darkorchid3',
#                      'brown3', 'aquamarine3')
#
#   colorChoosers <- list()
#
#   readoutBoxIds <- c()
#   for(readout in readouts) {
#     readoutBoxIds <- c(readoutBoxIds, paste0(readout,'-readout-checkbox'))
#   }
#
#   colorNum = 1
#   colorVal = 'white'
#   for(readout in readouts) {
#     if(colorNum <= length(defaultColors)) {
#       colorVal <- defaultColors[colorNum]
#     }
#
#     colorNum = colorNum + 1
#
#   #    colourInput(inputreadout, "Choose color:", "cornflowerblue")
#   # choices = c("steelblue", "cornflowerblue",
#   #             "firebrick", "palegoldenrod",
#   #             "forestgreen")
#
#   readoutColor <- colourpicker::colourInput(
#     inputId = paste0(readout,'-line-color'), label = paste0(readout," line color:"),
#     value = colorVal
#   )
#   colorChoosers[[paste0(readout,'-line-color')]] <- readoutColor
#
#   readoutColor <- colourpicker::colourInput(
#     inputId = paste0(readout,'-point-color'), label = paste0(readout," point color:"),
#     value = colorVal
#   )
#   colorChoosers[[paste0(readout,'-point-color')]] <- readoutColor
#
#   }
#
#   readoutDiv <- div(
#
#     readoutBoxes <- checkboxGroupInput(
#       inputId = 'readoutCollection',
#       label = 'Select Readouts to Plot',
#       #choices = readouts,
#       selected = readoutBoxIds,
#       width = '400px',
#       choiceNames = readouts,
#       choiceValues = readoutBoxIds,
#       inline = T
#   ),
#   colorChoosers
#   #for(readout in readouts) {
#
#
# #    colourInput(inputreadout, "Choose color:", "cornflowerblue")
#     # choices = c("steelblue", "cornflowerblue",
#     #             "firebrick", "palegoldenrod",
#     #             "forestgreen")
#
#     # readoutColor <- colourpicker::colourInput(
#     #   inputId = 'fluc-color', label = paste0("Pick a color for ","fluc",":"),
#     #   value = 'cornflowerblue'
#     # )
#   #}
# )


#
#
#   return(readoutDiv)
# }



shinyUI(fluidPage(
  useShinyjs(),

  titlePanel("Waterfall Plot"),

  sidebarLayout(
    sidebarPanel(id="file_input",
                 fileInput(
                   inputId = 'inputFile',
                   label = 'Select Input File',
                   multiple = FALSE,
                   accept = "*.csv",
                   width = NULL,
                   buttonLabel = "Browse...",
                   placeholder = "No file selected"
                 )

                 ),
    mainPanel("Plot")
  )



  # checkboxGroupInput(
  #   inputId = 'readoutCollection',
  #   label = 'Select Readouts to Plot',
  #   choices = c('fluc', 'nluc'),
  #   selected = c('fluc', 'nluc'),
  #   choiceName = c('readout1','readout2'),
  #   choiceValues = c('fluc','nluc'),
  #   inline = F
  # )
))


