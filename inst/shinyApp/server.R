library(shiny)
library(shinyjs)

# builds ui elements based on input file
addReadoutSelector <- function(status) {

  readouts <- status$readouts

  defaultColors <<- c('darkgreen', 'blue4', 'red4', 'gold3', 'darkseagreen4', 'deeppink3', 'deepskyblue3', 'darkorchid3',
                      'aquamarine4', 'darkorange3', 'sienna4', 'seagreen3', 'lemonchiffon4', 'lightskblue2',
                      'palegreen3', 'palevioletred3', 'peru', 'orangered3', 'purple4', 'gray20',
                      'plum3', 'wheat4', 'steelblue4', 'lightgoldenrod')

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

    cDiv <- div(id=paste0(readout,"-readout-color-div"),
                readoutColor <- colourpicker::colourInput(
                  inputId = paste0(readout,'-line-color'), label = paste0(readout," line color:"),
                  value = colorVal
                ),
                readoutColor <- colourpicker::colourInput(
                  inputId = paste0(readout,'-point-color'), label = paste0(readout," point color:"),
                  value = colorVal
                )
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

  rangeDiv <- div(id = 'range_div',
                  h4("Axis Range Parameters"),
                  div(id = 'response_custom',
                      checkboxInput(inputId = 'customize_response_range', label="Customize Response Axis"),
                      hidden(rangeConfig <- div(id = 'response_range_config',
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
                      hidden(rangeConfig <- div(id = 'conc_range_config',
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
                  checkboxInput(inputId = "show_curve_index_checkbox", label="Show Curve Number Axis Labels", value=T)
  )

  readoutDiv <- div(
    id = 'readout_params',
    h4("Readout Parameters"),
    readoutBoxes <- checkboxGroupInput(
      inputId = 'readoutCollection',
      label = 'Select Readouts to Plot',
      #choices = readouts,
      selected = readoutBoxIds,
      width = '400px',
      choiceNames = readouts,
      choiceValues = readoutBoxIds,
      inline = F
    ),
    colorDiv,
    rangeDiv
  )

  return(readoutDiv)
}


# Collect parameters from UI elements prior to a plot refresh
collectParameters <- function(input, output, status) {

  inputFile <- input$inputFile$datapath

  readouts <- input$readoutCollection
  readouts <- gsub('-readout-checkbox', "", readouts)

  if(is.null(readouts) || identical(readouts, character(0))) {
    return(NULL)
  } else if(length(readouts)==0) {
    showModal(modalDialog(h4("Need to select to plot at least one readout."),title="Missing Selected Readouts"))
    return(NULL)
  } else {
    pointColors <- c()
    lineColors <- c()
    for(readout in readouts) {
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


    # show curve number scale?
    showCurveNumberLabels <- input$show_curve_index_checkbox

    # axisTitles
    axisTitles <- list()
    axisTitles[['concTitle']] <- input$conc_axis_title
    axisTitles[['respTitle']] <- input$response_axis_title
    axisTitles[['curveTitle']] <- ""

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
                  returnPlotObject = T,
                  axisTitles = axisTitles,
                  planeColors = planeColors,
                  gridColor = gridColor,
                  showCurveNumberLabels = showCurveNumberLabels,
                  concAxisConfig = concAxisConfig,
                  responseAxisConfig = responseAxisConfig)
  }
  return(props)
}


# refesh plot, either initial plot or user triggered refresh
plotRefresh <- function(input, output, status){

  props <- collectParameters(input=input, output=output, status)

  if(!is.null(props)) {

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
                                      returnPlotObject = T,
                                      axisTitles = props$axisTitles,
                                      planeColors = props$planeColors,
                                      gridColor = props$gridColor,
                                      showCurveNumberLabels = props$showCurveNumberLabels,
                                      concAxisConfig = props$concAxisConfig,
                                      responseAxisConfig = props$responseAxisConfig
    )


  }  else {

    # Build a default starting plot
    # We hit this option if a file has been selected, but UI controls are still being constructed.
    p <- qHTSWaterfall::plotWaterfall(inputFile = status$inputFile,
                                      fileFormat = status$fileFormat,
                                      activityReadouts = status$readouts,
                                      logMolarConcVector = status$logConc,
                                      pointColors = status$defaultColors,
                                      curveColors = status$defaultColors,
                                      returnPlotObject = T
    )
  }

  output$mainPlot <- plotly::renderPlotly(p)

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

  options(shiny.maxRequestSize=30*1024^2)

  disable(id='plotRefreshBtn')
  disable(id='plotExportBtn')

  status <- ""

  wfPoints <- reactiveVal(0)
  wfLines <- reactiveVal(0)

  # Input file selected, initialize status and build initial plot.................
  observeEvent(input$inputFile, {

    status <<- qHTSWaterfall:::evaluateInputFile(input$inputFile$datapath)

    if(!status$valid) {
      showModal(modalDialog(h4(status$problem),title="File Format Problem"))
      return(NULL)
    }

    if(length(status$readouts) > 0) {
      enable(id='plotRefreshBtn')

      # coming soon...
      #enable(id='plotExportBtn')

      insertUI(ui=tags$div(addReadoutSelector(status)), selector='#inputFile_progress', where='afterEnd', immediate=F)
      plotRefresh(input, output, status)
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
  observeEvent(input$plotRefreshBtn,
               plotRefresh(input, output, status)
  )

  # button hit to export plot..................
  observeEvent(input$plotExportBtn, {
    props <- collectParameters(input=input, output=output, status)

    if(is.null(props)) {
      return()
    } else {

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
                                        returnPlotObject = T,
                                        axisTitles = props$axisTitles,
                                        planeColors = props$planeColors,
                                        gridColor = props$gridColor,
                                        showCurveNumberLabels = props$showCurveNumberLabels,
                                        concAxisConfig = props$concAxisConfig,
                                        responseAxisConfig = props$responseAxisConfig
      )

      savePlot(p)
    }
  }
  )

  # Application closed, stut down gracefully.............
  session$onSessionEnded(function() {
    stopApp()
  })

}

