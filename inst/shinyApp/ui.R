library(shiny)
library(shinyjs)


addPlotCustomizationUI <- function() {

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
                         sizeParamDiv,
                         aspectRatioDiv,
                         antiAliasing,
                         curvePointCount
  )

  plotParamDiv <- div( id="detail_params",
                       extraParamsDiv,
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
                       )
  )
  return(plotParamDiv)
}

sampleData <<- system.file("extdata", "Generic_qHTS_Format_Example.csv", package="qHTSWaterfall")

shinyUI(
  fluidPage(
  useShinyjs(),
  inlineCSS("
      #sidebarPanel {
      }
      #mainPanel {
        padding-top: 85px;
        margin-top: 5px;
        padding: 2px 5px 10px 10px;
        margin-left: 0px;
        margin-right: 0px;
        background-color: #e6e6e6;
        border: 1.5px solid #b1b3b5;
        border-radius: 6px;
        min-height=800px;
        height: 100%;
      }
      #mainPlotDiv, #mainPlot {
          border: 1.5px solid #b1b3b5;
          border-radius: 6px;
          background-color: #ffffff;
          width: 100%;
          height: 100%;
          min-height=800px;
      }

      #plotRefreshBtn, #plotExportBtn {
          background-color: #0f7ac7;
          color: #ffffff;
          margin-bottom: 5px;
          margin-right: 10px;
          width: 300px;
          text-align: center;
          font-weight: bold;
      }
      #plotRefreshBtn:disabled {
          opacity: 0.5;
      }
      #plotRefreshBtn:hover {
          background-color: #0967aa;
      }
      #titlePanel {
           background-image: linear-gradient(to right, #0f7ac7, white);
           padding: 5px 25px 5px 25px;
           color: #FFFFFF;
           margin-bottom: 0px;
      }
      #param-panel {
          border: 1.5px solid #b1b3b5;
          border-radius: 6px;
          background-color: #e6e6e6;
          padding: 0px 5px 5px 10px;
          margin-top: 5px;
          float: right;
      }
      #secondary-param-panel {
          border: 1.5px solid #b1b3b5;
          border-radius: 6px;
          background-color: #e6e6e6;
          padding: 0px 5px 5px 10px;
          margin-top: 5px;
          float: left;
      }
      #readoutCollection {
          margin-left: 0px;
          margin-right: 10px;
      }
      #sampleDataBtn {
          background-color: #8FBC8F;
          margin-left: 0px;
          margin-right: 10px;
          margin-top: 5px;
          margin-botom: 5px;
          width: 100%;

          display: inline-block;
          vertical-align: top;
      }
      #or {
          text-align: center;
          font-weight: bold;
      }
    "),
  fluidRow(
    column(12,
           id = 'titlePanel',
           titlePanel(title = "qHTS Waterfall Plot", windowTitle = "qHTS Waterfall Plot")
    )
  ),
  fluidRow(
    column(3,
           div(
             id = "param-panel",
             h3("Data Parameters"),
             actionButton(inputId = 'sampleDataBtn', label = 'Plot Our Sample Data'),
             downloadLink(outputId='sampleDataDownload', label="Download Sample Input Data"),
             h4(id="or","or"),
             fileInput(
               inputId = 'inputFile',
               label = 'Select Your Input File',
               multiple = FALSE,
               accept = c("*.csv","*.xlsx"),
               width = NULL,
               buttonLabel = "Browse...",
               placeholder = "No file selected"
             )
           )
    ),
    column(6,
           id = "mainPanel",
           h3("Plot Preview"),
           actionButton(inputId = 'plotRefreshBtn', label = 'Plot / Refresh'),
           actionButton(inputId = 'plotExportBtn', label = 'Export Plot'),
           div(
             id = "mainPlotDiv",
             shinycssloaders::withSpinner(plotly::plotlyOutput(outputId = "mainPlot",
                                                               width = '100%', height = '800px', inline=F
             ))
           )
    ),
    column(3,
           div(
             id = "secondary-param-panel",
             h3("Plot Parameters", id="plot_params_panel"),
             addPlotCustomizationUI()
           ),
    )
  )

))



