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
    cDiv <- div(id=paste0(readout,"-readout-color-div"),
    readoutColor <- colourpicker::colourInput(
      inputId = paste0(readout,'-line-color'), label = paste0(readout," line color:"),
      value = colorVal
    ),
    #colorChoosers[[paste0(readout,'-line-color')]] <- readoutColor

    readoutColor <- colourpicker::colourInput(
      inputId = paste0(readout,'-point-color'), label = paste0(readout," point color:"),
      value = colorVal
    )
    #colorChoosers[[paste0(readout,'-point-color')]] <- readoutColor
    )
    colorChoosers[[paste0(readout,"-readout-color-div")]] <- cDiv

  }

  plotInactivePoints <- div(id="plot-inactives-div",
                            checkboxInput(inputId = "plot-inactives-checkbox", label="Include inactive results:", value=T))

  colorChoosers[['plot-inactives-div']] <- plotInactivePoints

  # add inactive color
  inactiveColor <- div(id="inactive-color-div",colourpicker::colourInput(
    inputId = "inactive-point-color", label =  "Inactive point color:",
    value = 'gray'
  ))

  colorChoosers[['inactive-color-div']] <- inactiveColor

  colorDiv <- div(id="color-controls-div", colorChoosers)

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
    colorDiv
  )

  # now the extra controls

  # point size
  # line weight
  # plot aspect ratio (x (conc), y(response), z(curve position))
  # curve resolution 25, slider
  # lineWeight 1.0
  # antialising, smoothing curves F

  # consideration of managing process

  # phase 1 is to read the input to be able to set initial UI values
  # Can we capture and keep the generated data in the core class as a data object?
  # So process / update with a button
  # if core data is null, then update will create data, then plot
  # if data exists, update will just re-plot with desired params.

  # need a method to collect parameters and replot
  # need to be able to generate a plot in the main panel.
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
      insertUI(ui=addReadoutSelector(status$readouts), selector='#inputFile_progress', where='afterEnd')
    }



   }, ignoreNULL = TRUE)

  observeEvent(input[["plot-inactives-checkbox"]],
                {
                  print("in inactives listener...")
                    if((input[["plot-inactives-checkbox"]])) {
                      shinyjs::show('inactive-color-div')
                    } else {
                      shinyjs::hide('inactive-color-div',anim=T, time=0.25)
                    }
                }
                ,ignoreInit=TRUE)

  observeEvent(input$readoutCollection, {

      if(is.null(input$readoutCollection)) {
        return
      }

      vals <- input$readoutCollection

      print("base readouts")
      print(status$readouts)
      # fluc nluc

      print("checkbox values")
      print(vals)

      # need to assess current selections, and add back as needed
      for(readout in vals) {
        readout <- gsub("-readout-checkbox", "", readout)
        print("In insertUI testing... readout=")
        #print(readout)
        currId <- paste0(readout, "-readout-color-div")
        shinyjs::show(currId)
      }



      toDisable <- c()
      if(is.null(vals)) {
        for(readout in status$readouts) {
          toDisable <- c(toDisable, paste0(readout,'-readout-color-div'))

        }
      } else {

        vals <- gsub('-readout-checkbox', '', vals)

        offReadouts <- setdiff(status$readouts, vals)

        for(readout in offReadouts) {
          toDisable <- c(toDisable, paste0(readout,'-readout-color-div'))
        }
      }

      for(control in toDisable) {
        shinyjs::hide(id=control, anim=T, time=0.5)
      }



  }, ignoreNULL = FALSE, ignoreInit = TRUE)

}
#shinyApp(ui, server)




