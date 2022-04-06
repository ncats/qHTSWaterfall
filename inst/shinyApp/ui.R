# library(shiny)
# library(shinyjs)
#

shinyUI(fluidPage(
  useShinyjs(),
  # tags$head(
  # tags$style(
  #   HTML(
     inlineCSS("
      #sidebarPanel {
      }
      #mainLayout {
        padding-top: 85px;
        margin-top: 10px;
      }
      #mainPlotDiv {
          border: 1.5px solid #b1b3b5;
          border-radius: 4px;
      }
      #mainPlot, #mainPanel {
      }
      #plotRefreshBtn {
          background-color: #0f7ac7;
          color: #ffffff;
          margin-bottom: 5px;
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
           margin-bottom: 10px;
      }
    "),
  #   )
  # )),

  div(
    id = 'titlePanel',
    titlePanel(title = "qHTS Waterfall Plot", windowTitle = "qHTS Waterfall Plot")
  ),


  sidebarLayout(
    sidebarPanel = sidebarPanel(
      width = 4,
      id = "sidebarPanel",

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
    mainPanel = mainPanel(
      width = 8,
      id = "mainPanel",
      div(
        id = "plotDiv",
        h4("Plot Preview"),
        actionButton(inputId = 'plotRefreshBtn', label =
                       'Plot / Refresh'),
        div(
          id = "mainPlotDiv",
          shinycssloaders::withSpinner(plotly::plotlyOutput(outputId = "mainPlot",
                               width = '100%',
                               height = '600px'))
          # rgl::rglwidgetOutput(
          #   outputId = "mainPlot"
          #   #width = '100%',
          #   #height = '600px'
          # )
        )
      )
    ),
    position = 'left',
    fluid = T

  )
))
