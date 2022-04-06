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

  #concTextBox <- textAreaInput(inputId="concTextArea",label="Log Molar Conc. Values(lowest to highest, one per row or comma separated)",cols=8, rows=15)
  sizeParamDiv <- div(
  h3("Point Size and Line Weight"),
  pointSize <- sliderInput(inputId = "pointSize", label="Point Size [0 to 5.0]", min=0.0, max=5.0, value=1.0, step=0.1, round=T),
  lineWeight <- sliderInput(inputId = "lineWeight", label="Line Weight [0 to 5.0]", min=0.0, max=5.0, value=1.0, step=0.1, round=T),
  )
  aspectRatioDiv <- div(
  h3("Plot Aspect Ratio"),
  aspectX <- selectInput(inputId="aspectX", label="X (conc.) size ratio", choices=c(1:10), selected=1),
  aspectY <- selectInput(inputId="aspectY", label="Y (response) size ratio", choices=c(1:10), selected=1),
  aspectZ <- selectInput(inputId="aspectZ", label="Z (plot width) size ratio", choices=c(1:10), selected=3),
  )
  antiAliasing <- checkboxInput(inputId="antialias", label="Antialias/Smooth Lines",value=T)
  curvePointCount <- selectInput(inputId="curvePoints", label="Number of points to define curve fits.", choices=c(seq(25,250,25)), selected=100)

  extraParamsDiv <- div( class='param_div',
                         #concTextBox,
                         sizeParamDiv,
                        aspectRatioDiv,
                         antiAliasing,
                         curvePointCount
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
      inline = T
    ),
    colorDiv,
    extraParamsDiv
  )

  return(readoutDiv)
}

addPlotCustomization <- function(status) {

  plotParamDiv <- div( id="detail_params",
                       div(id='plane_colors',
                           h3("Plot Plane Colors"),
                           basePlaneColor <- colourpicker::colourInput(
                             inputId = 'base_plane_color', label = 'base/bottom plane color',
                             value = "#b8b6b6"
                           ),

                           leftPlaneColor <- colourpicker::colourInput(
                             inputId = 'left_plane_color', label = 'left verical plane color',
                             value = "#6e6868"
                           ),

                           rigthPlaneColor <- colourpicker::colourInput(
                             inputId = 'right_plane_color', label = 'right verical plane color',
                             value = "#999494"
                           ),
                           gridColor <- colourpicker::colourInput(
                             inputId = 'grid_color', label = 'grid color',
                             value = "#ffffff"
                           )
                       ),
                       div(id = 'range_div',
                           h3("Axis Range Parameters"),
                       div(id = 'response_custom',
                           checkboxInput(inputId = 'customize_response_range', label="Customize Response Axis"),
                           hidden(rangeConfig <- div(id = 'response_range_config',
                                              h3('Response Range Customization'),
                                              textInput(inputId = 'response_axis_title', label = "Response Axis Title", value="Response"),
                                              splitLayout(
                                                textInput(inputId = "resp_min", label = "Range Min", value = status$minResp),
                                                textInput(inputId = "resp_max", label = "Range Max", value = status$maxResp),
                                              ),
                                              splitLayout(
                                                textInput(inputId = "resp_tick_width", label = "Tick Width", value = 50),
                                                textInput(inputId = "resp_first_tick", label = "First Tick Value", value = status$minResp),
                                              )
                           ))
                       ),
                       div(id = 'conc_custom',
                           checkboxInput(inputId = 'customize_conc_range', label="Customize Concentration Axis"),
                           hidden(rangeConfig <- div(id = 'conc_range_config',
                                              h3('Concentration Range Customization'),
                                              textInput(inputId = 'conc_axis_title', label = "Concentration Axis Title", value="log10[conc], M"),
                                              splitLayout(
                                                textInput(inputId = "conc_min", label = "Range Min", value = status$minConc),
                                                textInput(inputId = "conc_max", label = "Range Max", value = status$maxConc),
                                              ),
                                              splitLayout(
                                                textInput(inputId = "conc_tick_width", label = "Tick Width", value = 1.0),
                                                textInput(inputId = "conc_first_tick", label = "First Tick Value", value = round(status$minConc)),
                                              )
                           ))
                       ),
                       checkboxInput(inputId = "show_curve_index_checkbox", label="Show Curve Number Axis Labels", value=T)
                       )
  )
  return(plotParamDiv)
}


collectParameters <- function(input, output, status) {

  inputFile <- input$inputFile$datapath

  readouts <- input$readoutCollection
  readouts <- gsub('-readout-checkbox', "", readouts)

  if(length(readouts)==0) {
    print("Message, Need to select at least one readout?")
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
    print(plotInactives)
    if(plotInactives) {
      inactiveColor = input[['inactive-point-color']]
    } else {
      inactiveColor = 'gray'
    }

    #concentrations
    # concText <- input$concTextArea
    #
    # if(length(grep(',',concText)) > 0) {
    #   concVals <- strsplit(concText, ",")
    # } else {
    #   concVals <- strsplit(concText, "\n")
    # }
    #
    # if(!is.null(concVals)) {
    #   concVals <- unlist(concVals)
    # }
    #
    # if(!is.null(concVals) && length(concVals) > 1) {
    #   concValues <- trimws(concVals)
    #   concVals <- as.numeric(concVals)
    #   naCount <- sum(is.na(concVals))
    #   if(naCount > 0) {
    #     vOrVs <- ' value'
    #     if(naCount > 1) {
    #       vOrVs <- ' values'
    #     }
    #     print("Message, some concentrations are not numbers.")
    #     showModal(modalDialog(h4(paste0("Some concentrations (",naCount,vOrVs,") are not numeric. Please remove/edit incorrect values.")), title="Invalid Concentration Values"))
    #     return(NULL)
    #   }
    # } else {
    #   print("Message, hey we need concentration values?")
    #   showModal(modalDialog(h4("Please provide log-molar concentrations for the data set."),title="Missing Concentration Values"))
    #   return(NULL)
    # }

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

  print("Props...")
  print(props)


  return(props)
}

server <- function(input, output, session) {


  disable(id='plotRefreshBtn')

  status <- ""

  wfPoints <- reactiveVal(0)
  wfLines <- reactiveVal(0)

  observeEvent(input$inputFile, {


    print(input$inputFile)
    print(input$datapath)
    print(input$name)

    status <<- qHTSWaterfall:::evaluateInputFile(input$inputFile$datapath)
    print(status$readouts)
    print(status$valid)
    print(status$problem)

    if(!status$valid) {
      showModal(modalDialog(h4(status$problem),title="File Format Problem"))
      return(NULL)
    }


    if(length(status$readouts) > 0) {
      insertUI(ui=addReadoutSelector(status$readouts), selector='#inputFile_progress', where='afterEnd')
      insertUI(ui = addPlotCustomization(status), selector="#readout_params", where='afterEnd')
    }

    enable(id='plotRefreshBtn')

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


  observeEvent(input$customize_conc_range, {
    if(input$customize_conc_range) {
      shinyjs::show("conc_range_config")
    } else {
      shinyjs::hide("conc_range_config")
    }
  })

  observeEvent(input$customize_response_range, {
    if(input$customize_response_range) {
      shinyjs::show("response_range_config")
    } else {
      shinyjs::hide("response_range_config")
    }
  })


  observeEvent(input$plotRefreshBtn, {
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
      output$mainPlot <- plotly::renderPlotly(p)

      # output$mainPlot <- rgl::renderRglwidget(
      #   expr ={
      #     scene
      #     rgl::rglwidget()
      #   }
      # )
    }
  }
  )

  session$onSessionEnded(function() {
    stopApp()
  })

}

