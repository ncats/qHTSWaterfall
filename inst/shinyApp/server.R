library(shiny)


addReadoutSelector <- function(readouts) {
  print("HEY I'm adding a checkbox group here, where???")

  defaultColors <- c('darkgreen', 'blue2', 'darkorange', 'deeppink3', 'deepskyblue3', 'darkorchid3',
                     'brown3', 'aquamarine3')

  colorChoosers <- list()

  readoutBoxIds <- c()
  for(readout in readouts) {
    readoutBoxIds <- c(readoutBoxIds, paste0(readout,'-readout-checkbox'))
  }

  colorNum = 1
  colorVal = 'white'
  for(readout in readouts) {
    if(colorNum <= length(defaultColors)) {
      colorVal <- defaultColors[colorNum]
    }

    colorNum = colorNum + 1

    #    colourInput(inputreadout, "Choose color:", "cornflowerblue")
    # choices = c("steelblue", "cornflowerblue",
    #             "firebrick", "palegoldenrod",
    #             "forestgreen")

    readoutColor <- colourpicker::colourInput(
      inputId = paste0(readout,'-line-color'), label = paste0(readout," line color:"),
      value = colorVal
    )
    colorChoosers[[paste0(readout,'-line-color')]] <- readoutColor

    readoutColor <- colourpicker::colourInput(
      inputId = paste0(readout,'-point-color'), label = paste0(readout," point color:"),
      value = colorVal
    )
    colorChoosers[[paste0(readout,'-point-color')]] <- readoutColor

  }

  readoutDiv <- div(

    readoutBoxes <- checkboxGroupInput(
      inputId = 'readoutCollection',
      label = 'Select Readouts to Plot',
      #choices = readouts,
      selected = readoutBoxIds,
      width = '400px',
      choiceNames = readouts,
      choiceValues = readoutBoxIds,
      inline = T
    ),
    colorChoosers
    #for(readout in readouts) {


    #    colourInput(inputreadout, "Choose color:", "cornflowerblue")
    # choices = c("steelblue", "cornflowerblue",
    #             "firebrick", "palegoldenrod",
    #             "forestgreen")

    # readoutColor <- colourpicker::colourInput(
    #   inputId = 'fluc-color', label = paste0("Pick a color for ","fluc",":"),
    #   value = 'cornflowerblue'
    # )
    #}
  )
}



server <- function(input, output, session) {


  status <- ""

  #ui <- source(file.path(".","ui.R"), local=T)$value

  observeEvent(input$inputFile, {

    print("event!!!")

    # mytable <- read.csv(input$inputFile$datapath)
    #
    # print(dim(mytable))
    #
    status <<- qHTSWaterfall:::evaluateInputFile(input$inputFile$datapath)

    if(length(status$readouts) > 0) {
      print("back in server... adding ui")

      insertUI(ui=addReadoutSelector(status$readouts), selector='#inputFile_progress', where='afterEnd')
    }

   }, ignoreNULL = TRUE)


  observeEvent(input$readoutCollection, {



      print("Hey checkbox click, can we print str????")

      if(is.null(input$readoutCollection)) {
        return
      }
      vals <- input$readoutCollection

      print("base readouts")
      print(status$readouts)
      # fluc nluc

      print("checkbox values")
      print(vals)
      # fluc-readout-checkbox nluc-readout-checkbox

      toDisable <- c()
      if(is.null(vals)) {
        for(readout in status$readouts) {
          toDisable <- c(toDisable, paste0(readout,'-line-color'))
          toDisable <- c(toDisable, paste0(readout,'-point-color'))
        }
      } else {

        vals <- gsub('-readout-checkbox', '', vals)

        #allReadouts <- paste0(status$readouts, collapse='-line-color')
        #allReadouts <- c(allReadouts, paste0(status$readouts, collapse='-point-color'))
        #print("allReadouts")
        #print(allReadouts)
        offReadouts <- setdiff(status$readouts, vals)
        print("offreadouts")
        print(offReadouts)

        for(readout in offReadouts) {
          toDisable <- c(toDisable, paste0(readout,'-line-color'))
          toDisable <- c(toDisable, paste0(readout,'-point-color'))
        }
      }

      for(control in toDisable) {

        #print(str(session[[control]]))
        #print(str(input[[control]]))

        shinyjs::disable(id=control)
        #removeUI(input[[control]])
        print('disabling ...')
        print(control)
      }

      #v2 <- isolate(input$readoutCollection)
      #print(v2)
      # print(input$`fluc-readout-checkbox`)

      # print(input$readoutCollection[['fluc-readout-checkbox']])


  }, ignoreNULL = FALSE, ignoreInit = TRUE)

}
#shinyApp(ui, server)




