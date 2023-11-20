##################################################
## Project: qHTSWaterfall
## Script purpose: Server file for qHTSWaterfall
## Date: 5/15/2022
## Authors: John Braisted
## Institute: National Center for Advancing Translational Sciences, NCATS
## National Institutes of Health
##################################################

# builds ui elements based on input file
addReadoutSelector <- function(status) {

  readouts <- status$readouts

  defaultColors <<- c('darkgreen', 'blue4', 'red4', 'gold3', 'darkseagreen4', 'lightsalmon4', 'darkorchid3',
                      'aquamarine4', 'darkorange3', 'sienna4', 'seagreen3', 'lemonchiffon4', 'lightskyblue2',
                      'palegreen3', 'palevioletred3', 'peru', 'deeppink3', 'orangered3', 'purple4', 'gray20',
                      'plum3', 'wheat4', 'steelblue4', 'lightgoldenrod')

  colorChoosers <- list()

  readoutBoxIds <- c()
  for(readout in readouts) {
    idReadout <- gsub(" ", "-", readout)
    readoutBoxIds <- c(readoutBoxIds, paste0(idReadout,'-readout-checkbox'))
  }

  colorNum = 1
  colorVal = 'darkgreen'
  for(readout in readouts) {
    if(colorNum <= length(defaultColors)) {
      colorVal <- defaultColors[colorNum]
    }

    print("Setting up readout ui")
    print(readout)
    print(colorVal)

    colorNum = colorNum + 1


    idReadout <- gsub(" ", "-", readout)

    cDiv <- div(id=paste0(idReadout,"-readout-color-div"),
                readoutColor <- colourpicker::colourInput(
                  inputId = paste0(idReadout,'-line-color'), label = paste0(readout," line color:"),
                  value = colorVal
                ),
                readoutColor <- colourpicker::colourInput(
                  inputId = paste0(idReadout,'-point-color'), label = paste0(readout," point color:"),
                  value = colorVal
                )
    )
    colorChoosers[[paste0(idReadout,"-readout-color-div")]] <- cDiv
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

  rangeDiv <- div(id = 'range_div',
                  h4("Axis Range Parameters"),
                  div(id = 'response_custom',
                      checkboxInput(inputId = 'customize_response_range', label="Customize Response Axis"),
                      shinyjs::hidden(rangeConfig <- div(id = 'response_range_config',
                                                h3('Response Range Customization'),
                                                textInput(inputId = 'response_axis_title', label = "Response Axis Title", value="Response"),
                                                splitLayout(cellWidths = c("50%", "50%"),
                                                  textInput(inputId = "resp_min", label = "Range Min", value = status$minResp),
                                                  textInput(inputId = "resp_max", label = "Range Max", value = status$maxResp)
                                                ),
                                                splitLayout(cellWidths = c("50%", "50%"),
                                                  textInput(inputId = "resp_tick_width", label = "Tick Width", value = 50),
                                                  textInput(inputId = "resp_first_tick", label = "First Tick Value", value = status$minResp)
                                                )
                      ))
                  ),
                  div(id = 'conc_custom',
                      checkboxInput(inputId = 'customize_conc_range', label="Customize Concentration Axis"),
                      shinyjs::hidden(rangeConfig <- div(id = 'conc_range_config',
                                                h3('Concentration Range Customization'),
                                                textInput(inputId = 'conc_axis_title', label = "Concentration Axis Title", value="log10[conc], M"),
                                                splitLayout(cellWidths = c("50%", "50%"),
                                                  textInput(inputId = "conc_min", label = "Range Min", value = status$minConc),
                                                  textInput(inputId = "conc_max", label = "Range Max", value = status$maxConc)
                                                ),
                                                splitLayout(cellWidths = c("50%", "50%"),
                                                  textInput(inputId = "conc_tick_width", label = "Tick Width", value = 1.0),
                                                  textInput(inputId = "conc_first_tick", label = "First Tick Value", value = round(status$minConc))
                                                )
                      ))
                  ),
                  div(id = 'axis-font-div',
                    selectInput(
                    inputId = 'axis_label_font_size',
                    label = 'Axis Label Font Size',
                    choices = c(11, 12, 13, 14, 15),
                    selected = 13,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = NULL,
                    size = NULL
                  ))
                  ,
                  checkboxInput(inputId = "show_curve_index_checkbox", label="Show Curve Number Axis Labels", value=T),
                  div(id = 'export_image_div',
                      selectInput(inputId = 'export_image_format',
                                  label = 'Image Export File Format',
                                  choices = c("png", "jpeg", "svg", "webp"),
                                  selected = "png",
                                  multiple = FALSE,
                                  selectize = TRUE,
                                  width = NULL,
                                  size = NULL
                      ),
                      selectInput(inputId = 'export_image_scale',
                              label = 'Image Export Scale',
                              choices = c(1:8),
                              selected = 4,
                              multiple = FALSE,
                              selectize = TRUE,
                              width = NULL,
                              size = NULL
                  )
                  )

  )

  readoutDiv <- div(
    id = 'readout_params',
    h4("Readout Parameters"),
    readoutBoxes <- checkboxGroupInput(
      inputId = 'readoutCollection',
      label = 'Select Readouts to Plot',
      #choices = readouts,
      selected = readouts,
      width = '400px',
      choiceNames = readouts,
      choiceValues = readouts,
      inline = F
    ),
    colorDiv,
    rangeDiv
  )

  return(readoutDiv)
}


# Collect parameters from UI elements prior to a plot refresh
collectParameters <- function(input, output, status) {

  if(usingSampleData) {
    inputFile <- sampleData
  } else {
    inputFile <- input$inputFile$datapath
  }

  # try to get the input file from the current status
  # if it's not set in the ui
  # if(is.null(inputFile) && !is.null(status) && !is.null(status$inputFile)) {
  #   inputFile <- status$inputFile
  #   input$inputFile$datapath <- inputFile
  # }

  readouts <- input$readoutCollection
  readabouts <- input$readoutCollection
  readouts <- gsub('-readout-checkbox', "", readouts)

  idReadouts <- gsub(" ", "-", readouts)

  if(newData == TRUE && is.null(input$readoutCollection)) {
    return(NULL)
  } else if(length(readouts)==0 || identical(readouts, character(0))) {
    if(!newData) {
      showModal(modalDialog(h4('You need to select to plot at least one readout.'),
                            tags$h4("The ", tags$i(tags$strong("Select Readouts to Plot")), " checkboxes are to the left."),
                            h4(paste0("Please Select one or more of your readouts: ", paste0(status$readouts,collapse = ', '))),
                            title="No Readouts Selected Warning"))
    }
    return(NULL)
  } else {
    pointColors <- c()
    lineColors <- c()
    for(readout in idReadouts) {
      lineColorTag = paste0(readout, "-line-color")
      pointColorTag = paste0(readout,"-point-color")
      lineColors <- c(lineColors, input[[lineColorTag]])
      pointColors <- c(pointColors, input[[pointColorTag]])
    }

    plotInactives <- input[['plot-inactives-checkbox']]
    if(plotInactives) {
      inactiveColor = input[['inactive-point-color']]
    } else {
      inactiveColor = 'gray'
    }

    pointSize <- input$pointSize
    lineWeight <- input$lineWeight

    aspectRatio <-c()
    aspectRatio <- c(aspectRatio, input$aspectX)
    aspectRatio <- c(aspectRatio, input$aspectY)
    aspectRatio <- c(aspectRatio, input$aspectZ)
    aspectRatio <- as.numeric(unlist(aspectRatio))

    antialias <- input$antialias
    lineRes <- as.numeric(input$curvePoints)


    # plane colors and grid color
    planeColors <- list()
    planeColors[['basePlaneColor']] <- input$base_plane_color
    planeColors[['rightPlaneColor']] <- input$right_plane_color
    planeColors[['leftPlaneColor']] <- input$left_plane_color

    gridColor = input$gridColor

    # response scale config
    responseAxisConfig <- list(min=input$resp_min, max=input$resp_max,
                               tickSizeVal=input$resp_tick_width, firstTick=input$resp_first_tick)

    # conc scale config
    concAxisConfig <- list(min=input$conc_min, max=input$conc_max,
                           tickSizeVal=input$conc_tick_width, firstTick=input$conc_first_tick)

    # axis font
    axisFontSize <- input$axis_label_font_size

    # show curve number scale?
    showCurveNumberLabels <- input$show_curve_index_checkbox

    # axisTitles
    axisTitles <- list()
    axisTitles[['concTitle']] <- input$conc_axis_title
    axisTitles[['respTitle']] <- input$response_axis_title
    axisTitles[['curveTitle']] <- "Compound"

    exportFileScale = input$export_image_scale
    exportFileFormat = input$export_image_format

    props <- list(inputFile = inputFile,
                  fileFormat = status$fileFormat,
                  activityReadouts = readouts,
                  logMolarConcVector = status$logConc,
                  pointColors = pointColors,
                  curveColors = lineColors,
                  inactiveColor = inactiveColor,
                  alpha=1,
                  pointSize = pointSize,
                  lineWeight = lineWeight,
                  plotInactivePoints = plotInactives,
                  curveResolution = lineRes,
                  plotAspectRatio = aspectRatio,
                  axisTitles = axisTitles,
                  planeColors = planeColors,
                  gridColor = gridColor,
                  showCurveNumberLabels = showCurveNumberLabels,
                  concAxisConfig = concAxisConfig,
                  responseAxisConfig = responseAxisConfig,
                  axisFontSize = axisFontSize,
                  exportFileFormat = exportFileFormat,
                  exportFileScale = exportFileScale)
  }
  return(props)
}


# refesh plot, either initial plot or user triggered refresh
plotRefresh <- function(input, output, status, newData){

  doRefresh = TRUE
  props = NULL
  p = NULL

  if(!newData) {
    currProps <- collectParameters(input=input, output=output, status)
    if(is.null(currProps)) {
      doRefresh = FALSE
      props <- NULL
    } else {
      props <- currProps
      newData <<- FALSE
    }
  } else {
    props <- NULL
  }

  if(!is.null(props)) {

    if(!doRefresh)
      return(NULL)

    # This block runs if we have props, indicating user selected update/refresh.
    p <- qHTSWaterfall::plotWaterfall(inputFile = props$inputFile,
                                      fileFormat = props$fileFormat,
                                      activityReadouts = props$activityReadouts,
                                      logMolarConcVector = props$logMolarConcVector,
                                      pointColors = props$pointColors,
                                      curveColors = props$curveColors,
                                      inactiveColor = props$inactiveColor,
                                      alpha=1,
                                      pointSize = props$pointSize,
                                      lineWeight = props$lineWeight,
                                      plotInactivePoints = props$plotInactivePoints,
                                      curveResolution = props$curveResolution,
                                      plotAspectRatio = props$plotAspectRatio,
                                      axisTitles = props$axisTitles,
                                      planeColors = props$planeColors,
                                      gridColor = props$gridColor,
                                      showCurveNumberLabels = props$showCurveNumberLabels,
                                      concAxisConfig = props$concAxisConfig,
                                      responseAxisConfig = props$responseAxisConfig,
                                      axisFontSize = props$axisFontSize,
                                      plotExportScale = as.integer(props$exportFileScale),
                                      plotExportFileType = props$exportFileFormat
                                      )


  }  else if(newData) {

    if(!doRefresh) {
      return(NULL)
    }

    # Build a default starting plot
    # We hit this option if a file has been selected, but UI controls are still being constructed.
    p <- qHTSWaterfall::plotWaterfall(inputFile = status$inputFile,
                                      fileFormat = status$fileFormat,
                                      activityReadouts = status$readouts,
                                      logMolarConcVector = status$logConc,
                                      pointColors = status$defaultColors,
                                      curveColors = status$defaultColors
    )
  }

  if(!is.null(p)) {
    output$mainPlot <- plotly::renderPlotly(p)
  }
  newData <<- FALSE
}

sampleDataDialog <- function() {

  showModal(
    modalDialog(label="Use Sample Data",
                h4("Select to either plot or download an example data file."),
                radioButtons(inputId = "sample_data_radio_btns",
                             label = "",
                             choiceNames=c("Plot Sample Data",
                                           "Download Sample Data File"),
                             choiceValues=c("plot_data", "download_data"),
                             selected = "plot_data"),
                size='m',
                footer = tagList(
                  modalButton("Cancel"),
                  actionButton("ok_sample_data", "OK")
                )
    )
  )
}


plotSampleData <- function(input, output, sampleData) {

  if(!is.null(status)) {
    # if the input file represents a file change, remove the UI for readouts, then rebuild
    # removing ui
    removeUI(selector='#readout_params', immediate=T)
    status <<- NULL
    newData <<- TRUE
  } else {

  }

  status <<- qHTSWaterfall:::evaluateInputFile(sampleData)
  status$inputFile <- sampleData



  if(!status$valid) {
    showModal(modalDialog(h4(status$problem),title="File Format Problem"))
    return(NULL)
  }

  if(length(status$readouts) > 0) {
    shinyjs::enable(id='plotRefreshBtn')

    # coming soon...
    #enable(id='plotExportBtn')

    insertUI(ui=tags$div(addReadoutSelector(status)), selector='#inputFile_progress', where='afterEnd', immediate=F)
    plotRefresh(input, output, status, newData)

    newData <<- FALSE
  }
}

downloadSampleData <- function(output, sampleDataFile) {

  downloadHandler(
    filename = "generic_qhts_data.xlsx",
    content = function(file) {
      #file.copy(sampleData, "generic_qhts_data.xlsx")
      write.xlsx(sampleData, file, row.names = FALSE)
    }
    ,
    contentType = "text/csv"
  )

}

# exports plot... working on solution....
savePlot <- function(p) {

  # interactive plots in plotly have no direct R methods to
  # export in formats other than low res png
  # other systems have supported this in the past, mostly on the python side of plotly
  # first, kaleido, the orca command line tool.
  # certain utilities may exist for paid subscriptions to plotly
  #

  # pdf(file = "/Users/braistedjc/Desktop/My_Plot.pdf",   # The directory you want to save the file in
  #     width = 4, # The width of the plot in inches
  #     height = 4) # The height of the plot in inches
  #
  # p
  #
  # dev.off()

}



####################################
#
#
# Main server method, includes various 'observers' to react to input.
#
#
####################################
server <- function(input, output, session) {

  options(shiny.maxRequestSize=100*1024^2)

  shinyjs::disable(id='plotRefreshBtn')
  shinyjs::disable(id='plotExportBtn')

  status <<- NULL
  usingSampleData <<- FALSE
  newData <<- TRUE
  exportScale <<- 4
  exportType <<- 'png'

  sampleData <<- system.file("extdata", "Generic_qHTS_Format_Example.csv", package="qHTSWaterfall")
  downloadSampleData <<- system.file("extdata", "Generic_qHTS_Format_Example.xlsx", package="qHTSWaterfall")

  wfPoints <- reactiveVal(0)
  wfLines <- reactiveVal(0)

  # Input file selected, initialize status and build initial plot.................
  observeEvent(input$inputFile, {

    usingSampleData <<- FALSE

    if(!is.null(status)) {
      # if the input file represents a file change, remove the UI for readouts, then rebuild
      # removing ui
      removeUI(selector='#readout_params', immediate=T)
      status <<- NULL
      newData <<- TRUE
    }

    status <<- qHTSWaterfall:::evaluateInputFile(input$inputFile$datapath)

    if(!status$valid) {
      showModal(modalDialog(h4(status$problem),title="File Format Problem"))
      return(NULL)
    }

    if(length(status$readouts) > 0) {
      shinyjs::enable(id='plotRefreshBtn')

      #enable(id='plotExportBtn')

      insertUI(ui=tags$div(addReadoutSelector(status)), selector='#inputFile_progress', where='afterEnd', immediate=F)
      plotRefresh(input, output, status, newData)
    }

  }, ignoreNULL = TRUE)


  # selection made to the plotInactives checkbox................
  observeEvent(input[["plot-inactives-checkbox"]],
               {
                 if((input[["plot-inactives-checkbox"]])) {
                   shinyjs::show('inactive-color-div')
                 } else {
                   shinyjs::hide('inactive-color-div',anim=T, time=0.25)
                 }
               }
               ,ignoreInit=TRUE)



  observeEvent(input[["sampleDataBtn"]],
               {
                 usingSampleData <<- TRUE
                 newData <<- TRUE

                 plotSampleData(input, output, sampleData)
               }
               ,ignoreNULL = TRUE
               )

  output$sampleDataDownload <- downloadHandler(
     filename = function() {
       "qHTS_Generic_Data_Sample.xlsx"
     },
     content = function(con) {
       file.copy(downloadSampleData, con)
     }
  )


  # selection made to input checkboxes..................
  observeEvent(input$readoutCollection, {

    if(is.null(input$readoutCollection)) {
      return
    }

    vals <- input$readoutCollection

    # need to assess current selections, and add back as needed
    for(readout in vals) {
      readout <- gsub("-readout-checkbox", "", readout)
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
  }, ignoreNULL = FALSE, ignoreInit = TRUE
  )


  # checkbox selected to update the concentration range parameters.........
  observeEvent(input$customize_conc_range, {
    if(input$customize_conc_range) {
      shinyjs::show("conc_range_config")
    } else {
      shinyjs::hide("conc_range_config")
    }
  })

  # checkbox selected to update the response range parameters.........
  observeEvent(input$customize_response_range, {
    if(input$customize_response_range) {
      shinyjs::show("response_range_config")
    } else {
      shinyjs::hide("response_range_config")
    }
  })

  # button hit to refresh plot................
  observeEvent(input$plotRefreshBtn, {
    newData <<- FALSE
    plotRefresh(input, output, status, FALSE)
  }
  )


  observeEvent(input[['export_image_format']], {

    # if dynamic addition... check for null input
    if(is.null(input[['export_image_format']])) {
      return()
    }

    val <- input[['export_image_format']]

    # only update on changed values...
    if(val != exportType) {
      exportType <<- val
      newData <<- FALSE
      plotRefresh(input, output, status, FALSE)
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE
  )


  observeEvent(input[['export_image_scale']], {

    # if dynamic addition... check for null input
    if(is.null(input[['export_image_scale']])) {
      return()
    }

    val <- input[['export_image_scale']]

    # only update on changed values...
    if(val != exportScale) {
      exportScale <<- val
      newData <<- FALSE
      plotRefresh(input, output, status, FALSE)
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE
  )

  # button hit to export plot..................
  # observeEvent(input$plotExportBtn, {
  #   props <- collectParameters(input=input, output=output, status)
  #
  #   if(is.null(props)) {
  #     return()
  #   } else {
  #
  #     p <- qHTSWaterfall::plotWaterfall(inputFile = props$inputFile,
  #                                       fileFormat = props$fileFormat,
  #                                       activityReadouts = props$activityReadouts,
  #                                       logMolarConcVector = props$logMolarConcVector,
  #                                       pointColors = props$pointColors,
  #                                       curveColors = props$curveColors,
  #                                       inactiveColor = props$inactiveColor,
  #                                       alpha=1,
  #                                       pointSize = props$pointSize,
  #                                       lineWeight = props$lineWeight,
  #                                       plotInactivePoints = props$plotInactivePoints,
  #                                       curveResolution = props$curveResolution,
  #                                       plotAspectRatio = props$plotAspectRatio,
  #                                       axisTitles = props$axisTitles,
  #                                       planeColors = props$planeColors,
  #                                       gridColor = props$gridColor,
  #                                       showCurveNumberLabels = props$showCurveNumberLabels,
  #                                       concAxisConfig = props$concAxisConfig,
  #                                       responseAxisConfig = props$responseAxisConfig
  #     )
  #
  #     savePlot(p)
  #   }
  # }
  # )

  # Application closed, stut down gracefully.............
  session$onSessionEnded(function() {
    stopApp()
  })

}

