# THE PURPOSE OF THIS PROGRAM IS TO TAKE A .CSV FILE AND CREATE A 3D WATERFALL PLOT


extractReadoutColumns <- function(colList, readout, colKey) {
  cols <- colList[grep(colKey, colList)]
  col <- cols[grep(readout, cols)]
  return(col)
}

# utility functions to recreate titration curves ----------------------------------------
interleave <- function(x) {
  unlist(lapply(1:(length(x)-1), function(i) c(x[i], x[i+1])))
}

f <- function(params, concs, interleave=TRUE, curveRes) {
  # print("starting f")
  xx <- seq(min(concs), max(concs), length=curveRes)
  yy <- with(params, ZERO + (INF-ZERO)/(1 + 10^((LAC50-xx)*HILL)))
  # print("ending f")
  return(data.frame(x=xx, y=yy))
}

#' Plots 3D qHTS waterfall plot, given Pubchem activity file or NCATS qHTS format file.
#' @param inputFile The input file path.
#' @param fileFormat a required value indicdating the file format. Valid values are 'Generic_qhts' or 'NCATS_qhts'. The value is case-insensitive.
#' @param activityReadouts Activity data readouts to include in plot
#' @param logMolarConcVector An optional input list of log molar concentrations, if input is not from PubChem.
#' @param pointColors color list for activity readout \emph{\bold{data points}}.
#' The order should match the activityReadoutsList order.
#' @param curveColors color list for activity readout \emph{\bold{fitted dose-response curves}}.
#' The order should match the activityReadoutsList order.
#' @param inactiveColor color to display inactive data, default is 'gray'.
#' @param alpha alpha transparency of the the plot lines, default is 1.0
#' @param pointSize relative size of plotted points, default = 2
#' @param lineWeight thickness of fitted curves. Default thickness is 1.0. Decimal numbers are permitted.
#' @param plotInactivePoints TRUE will plot inactive data as datapoints, FALSE Will hide inactive data.
#' @param curveResolution value between 25 and 250, number of points to define dose-response curves.
#' Fewer points renders as connected straight lines.
#' @param plotAspectRatio relative sizes of concentration axis (x), response axis (y), and waterfall width (z).
#' Input as list, derault: c(1, 1, 3)
#' @param antialiasSmoothing smooths plot line rendering. Default is FALSE. Setting as TRUE will smooth lines, but may slow response during re-drawing plot.
#' @param returnPlotObject if True, it returns an rgl 'scene' object that can be plotted using 'rgl::plot3d()'.
#' If FALSE, the default value, this function opens and rgl plot window for plotting.
#' @param axisTitles a list holding response, concentration and curve number axis titles (default, list(concTitle="log2Conc, M", respTitle="Response", curveTitle=""))
#' @param planeColors a list holding base, left and right plane colors, value names/keys are 'basePlaneColor, rightPlaneColor, leftPlaneColor.
#' @param gridColor a specified color for grids, color name or hex color value (default #ffffff)
#' @param showCurveNumberLabels boolean, indicates if the curve index numbers should be shown on the axis, or hidden.
#' @param concAxisConfig a list with 4 named values: min, max, tickWidth, firstTick.
#' @param responseAxisConfig a list with 4 named values: min, max, tickWidth, firstTick.
#' @importFrom utils read.csv
#' @import plotly
#' @import stringr
#' @import dplyr
#' @import tidyr
#' @examples
#' \dontrun{
#'
#' # specify sample data file
#' filePath <- system.file("extdata", "NCATS_CMT1A_PMP22_Follow_Up.csv", package="qHTSWaterfall")
#'
#' # supply log molar concentrations (for non-PubChem data only)
#' logConc <- c(
#'  -9.011761791,
#'  -8.534640544,
#'  -8.057519287,
#'  -7.580398031,
#'  -7.103276777,
#'  -6.626155522,
#'  -6.149034267,
#'  -5.671913012,
#'  -5.194791758,
#'  -4.717670503,
#'  -4.240549248
#')
#'
#' # make plotting call with desired parameter settings
#' # 3D Plot will be presented in a separate window.
#' plotWaterfall(
#'   inputFile=filePath,
#'   fileFormat = 'ncats_qhts',
#'   activityReadouts = c("fluc", "nluc"),
#'   logMolarConcVector = logConc,
#'   pointColors = c("darkgreen", "royalblue3"),
#'   curveColors = c("darkgreen", "royalblue3"),
#'   inactiveColor = "gray",
#'   pointSize = 3,
#'   alpha = 1,
#'   plotInactivePoints = F,
#'   curveResolution = 100,
#'   plotAspectRatio = c(2,2,5),
#'   returnPlotObject = F
#'   )
#' }
#' @export
plotWaterfall <- function(inputFile, fileFormat='generic_qhts', activityReadouts = c('Activity'), logMolarConcVector = NULL,
                          pointColors=c('darkgreen','blue4'), curveColors=c('darkgreen', 'blue4'),
                          inactiveColor='gray', alpha=1, pointSize=1.0, lineWeight=1.5, plotInactivePoints=T, curveResolution=25,
                          plotAspectRatio=c(1,1,3), antialiasSmoothing = F, returnPlotObject = F,
                          axisTitles = list(concTitle="log2[Conc], M", respTitle="Response", curveTitle=""),
                          planeColors = list(basePlaneColor="#b8b6b6", rightPlaneColor="#999494", leftPlaneColor="#6e6868"),
                          gridColor = "#ffffff", showCurveNumberLabels = TRUE, concAxisConfig = NULL, responseAxisConfig = NULL) {

  if(fileFormat == 'ncats_qhts') {
    generic_data <- 0
  } else {
    generic_data <- 1
  }

  # # print(inputFile)
  ## write("in plot function, just starting", stderr())


  fileExt <- tools::file_ext(inputFile)

  print("starting plotWaterfall method..")
  # print("file ext =")
  # print(fileExt)

  # # print(activityReadouts)
  # # print(curveColors)
  # # print(logMolarConcVector)
  # # print(pointColors)
  # # print(plotAspectRatio)

  # aspect ratio... note plotly switches y and z relative to our notion, hence the indices swap on assignment
  aspect <- list()
  aspect[['x']] <- plotAspectRatio[1]
  aspect[['y']] <- plotAspectRatio[3]
  aspect[['z']] <- plotAspectRatio[2]

  plotPoints = T
  if(pointSize == 0) {
    plotPoints = F
  }

  #activity readouts, should have an equal or greater numbrer of point and curve colors

  if(curveResolution < 25) {
    curveResolution = 25
  } else if(curveResolution > 250) {
    curveResolution = 250
  }

  #Important settings
  keyword_1 <- "Active"
  keyword_2 <- "Inactive"
  keyReadouts <- activityReadouts
  alpha_1 <- alpha

  if(!is.null(logMolarConcVector)) {
    conc <- logMolarConcVector
  } else {

    fileDetails <- extractConcFromFile(inputFile)

    if(is.null(fileDetails) || is.null(fileDetails$logConc)) {
      return(NULL)
    }

    conc <- fileDetails$logConc
    logMolarConcVector <- conc
  }
  #Color of points in graph ------------------------------------------------------
  pointColors <- c(pointColors, inactiveColor)
  lineColors <- c(curveColors, inactiveColor)

  # print("in plot method, about to read data")
  # read data file (header line 2 and body) ----------------------------
  if(tolower(fileExt)  == 'csv') {
    cdata <- read.csv(inputFile, header=TRUE, skip=1, na.string="null")
  } else {
    cdata <- openxlsx::read.xlsx(inputFile, sheet=1, startRow=2)
  }
  heads <- colnames(cdata)

  # print("in plot method, finished read data, headers")
  # print(heads)

  # print("cdata dim")
  # print(dim(cdata))

  #Identifying data headers to load ----------------------------------------------
  if(generic_data == 1) {

    #matching variable names
    compId <- "Comp_ID"
    readout <- "Readout"
    fit <- "Fit_Output"
    lac50 <- "Log_AC50_M"
    hill <- "Hill_Slope"
    inf <- "S_Inf"
    zero <- "S_0"

  } else {

    #matching variable names
    compId <- "Sample.ID"
    fit <- "Fit_Output"
    readout <- "Sample.Data.Type"
    lac50 <- "Log.AC50..M."
    hill <- "Hill.Coef"
    inf <- "Inf.Activity"
    zero <- "Zero.Activity"

  }

  titrationLengths <- list()
  minConc <- list()
  maxConc <- list()



  # for now, apply the single logMolarConcVector to all readouts
  concValues <- list()
  for(currReadout in activityReadouts) {
    logMolarConcVector <- format(round(logMolarConcVector, 2), nsmall = 2)
    logMolarConcVector <- as.numeric(logMolarConcVector)
    concValues[[currReadout]] <- logMolarConcVector
    minConc[[currReadout]] <- min(logMolarConcVector)
    maxConc[[currReadout]] <- max(logMolarConcVector)
  }


  # Now we have the following concentration related lists, per readout
  # concValues, minConc, maxConc, values or vectors, named by readout


  #creating a Fit_Output if not given one ----------------------------------------
  if(fit %in% heads == FALSE){
    if(generic_data == 1) {
      for (i in 1:nrow(cdata)){
        if(cdata[i,readout] == 'Active') {
          #if(cdata[i,readout]==keyword_1 || cdata[i,readout]==keyword_2){
          cdata[i,fit] = 1
        } else {
          cdata[i,fit] = 0
        }
      }
    } else {
      for (i in 1:nrow(cdata)){
        if(cdata[i,readout] %in% keyReadouts) {
          #if(cdata[i,readout]==keyword_1 || cdata[i,readout]==keyword_2){
          cdata[i,fit] = 1
        } else {
          cdata[i,fit] = 0
        }
      }
    }
  }


  # need to capture the list of data for each readout

  #creating smaller data sets with only needed data-------------------------------
  cdata_points <- list()
  cdata_curves <- list()
 # if(F){
    # for(currReadout in names(concMapping)) {
    #   concCols <- concMapping[[currReadout]]
    #   cdata_points[[currReadout]] <- cdata %>% select(fit, compId, readout, concCols)
    #   cdata_curves[[currReadout]] <- cdata %>% select(compId, fit, readout, lac50[[currReadout]], hill[[currReadout]], inf[[currReadout]], zero[[currReadout]])
    #   colnames(cdata_curves[[currReadout]]) <- c("COMP_ID", "Fit_Output", "readout", "LAC50", "HILL", "INF", "ZERO")
    #   # need to alter readout to reflect the readout
    #   cdata_curves[[currReadout]]$readout <- currReadout
    # }


  dataCols <- NULL
  for(i in 1:length(conc)){
    dataCols[i] <- c(paste("Data",(i-1), sep=""))
  }
  all_cdata_points <- cdata[,c(fit, compId, readout, dataCols)]
  all_cdata_points$compIndex <- 1:nrow(all_cdata_points)

  all_cdata_curves <- cdata[,c(fit, compId, readout, lac50, hill, inf, zero)]
  all_cdata_curves$compIndex <- 1:nrow(all_cdata_curves)

  # partition the data by readout
  for(currReadout in activityReadouts) {
    cdata_points[[currReadout]] <- subset(all_cdata_points, all_cdata_points[[readout]] == currReadout)

    cdata_curves[[currReadout]] <- subset(all_cdata_curves, all_cdata_curves[[readout]] == currReadout)
    colnames(cdata_curves[[currReadout]]) <- c("Fit_Output", "COMP_ID", "readout", "LAC50", "HILL", "INF", "ZERO", "compIndex")

    # set numeric columns
    cdata_curves[[currReadout]]$LAC50 <- as.numeric(as.character(cdata_curves[[currReadout]]$LAC50))
    cdata_curves[[currReadout]]$HILL <- as.numeric(as.character(cdata_curves[[currReadout]]$HILL))
    cdata_curves[[currReadout]]$INF <- as.numeric(as.character(cdata_curves[[currReadout]]$INF))
    cdata_curves[[currReadout]]$ZERO <- as.numeric(as.character(cdata_curves[[currReadout]]$ZERO))
  }



  #titration points to be put on the graph ---------------------------------------

  numReadouts <- length(activityReadouts)
  readoutCount <- 1
  curveCount <- 1
  for(currReadout in activityReadouts) {
    curveCount <- readoutCount
    dataPoints <- cdata_points[[currReadout]]
    #dataPoints$z <- dataPoints$compIndex
#    for(i in 1:nrow(dataPoints)) {
#      dataPoints$z[i] <- curveCount
#      curveCount <- curveCount + numReadouts
#    }
    colnames(dataPoints) <- c("Fit_Output","COMP_ID", "readout", concValues[[currReadout]], "z")
    # set the readout columnn
    dataPoints$readout <- currReadout
    cdata_points[[currReadout]] <- dataPoints
    readoutCount <- readoutCount + 1
  }

  # pivot main matrices
  fullMatriix <- NULL
  readoutCount <- 1
  # first stack the matrices
  for(currReadout in activityReadouts) {
    if(readoutCount == 1) {
      fullMatrix <- cdata_points[[currReadout]]
    } else {
      fullMatrix <- rbind(fullMatrix, cdata_points[[currReadout]])
    }
    readoutCount <- readoutCount + 1
  }

  # write("about to pivot matrix... usin tidyr pivot_longer", stderr())

  mainMatrix <- tidyr::pivot_longer(fullMatrix, cols = 4:(ncol(fullMatrix)-1), names_to = "x", values_to = "y")

  #mainMatrix <- tidyr::pivot_longer(cdata_points, cols = 2:(l-1), names_to = c("x","z"), names_pattern = "(.)(.)", values_to = "y")

  myMain <- mainMatrix
  #correcting data type-----------------------------------------------------------

  mainMatrix$x <- as.double(mainMatrix$x)
  mainMatrix$y <- as.double(mainMatrix$y)
  mainMatrix$z <- as.double(mainMatrix$z)

  waterfall_POINTS_data <- mainMatrix

  # set numeric columns in curves data




  #separating the data by sampel data type (readout)------------------------------------------------------------

  #hold the ploted points in here
  waterfallPoints = list()
  for(currReadout in keyReadouts) {
    waterfallPoints[[currReadout]] = data.frame(x=double(), y=double(), z=double())
  }
  if(plotInactivePoints) {
    waterfallPoints[['inactive']] = data.frame(x=double(), y=double(), z=double())
  }
  # hold teh plotted lines, curve fits in here, only need to handle actives, with plot = 0
  waterfallLines = list()
  for(currReadout in keyReadouts) {
    waterfallLines[[currReadout]] = data.frame(x=double(), y=double(), z=double())
  }

  # write("getting/building point data now, starting...", stderr())

  ##########
  ##
  ## Old points method, inefficient
  ##
  ##########

  # notes during rework...
  # iterates over all waterfall points
  # for each row get the readout on that row
  # capture show fit

  # if the readout is a key readout and we show the fit
  # subset the waterfall points df by readout (list dereference)
  # then bind that row to that data subset
  # then put that readout subset back into the list, putting away the dataframe.

  # why bind rows individually, will a subsetting keep the order?

  # l <- nrow(waterfall_POINTS_data)
  # for(i in 1:l){
  #
  #   readout <- waterfall_POINTS_data$readout[i]
  #   showFit <- waterfall_POINTS_data$Fit_Output[i]
  #
  #   if(readout %in% keyReadouts && showFit == 1) {
  #     #if(waterfall_POINTS_data$readout[i] == keyword_1 || waterfall_POINTS_data$readout[i] == keyword_2) {
  #     wfData <- waterfallPoints[[readout]]
  #
  #     wfData <- dplyr::bind_rows(wfData, data.frame(x=waterfall_POINTS_data$x[i],
  #                                                   y=waterfall_POINTS_data$y[i],
  #                                                   z=waterfall_POINTS_data$z[i]))
  #     waterfallPoints[[readout]] <- wfData
  #   } else if(plotInactivePoints) {
  #
  #     wfDataInactive = waterfallPoints[['inactive']]
  #     wfDataInactive <- dplyr::bind_rows(wfDataInactive, data.frame(x=waterfall_POINTS_data$x[i],
  #                                                                   y=waterfall_POINTS_data$y[i],
  #                                                                   z=waterfall_POINTS_data$z[i]))
  #     waterfallPoints[['inactive']] <- wfDataInactive
  #   }
  # }

  # Reworked point methods, only iterates over readouts and checks inactives, about 6x faster for a 15K point set
  # The relitive improvement will increase for very large data sets, because we no longer iterate over all points, but rather subset.
  for(readout in keyReadouts) {
    readoutData <- waterfall_POINTS_data[(waterfall_POINTS_data$readout == readout & waterfall_POINTS_data$Fit_Output == 1), c('x','y','z')]
    waterfallPoints[[readout]] <- readoutData
  }

  if(plotInactivePoints) {
    readoutData <- waterfall_POINTS_data[waterfall_POINTS_data$Fit_Output == 0, c('x','y','z')]
    waterfallPoints[['inactive']] <- readoutData
  }

  #taking care of missing data ---------------------------------------------------
  cdata_curves$LAC50[is.na(cdata_curves$LAC50)] <- log10(10)
  cdata_curves$HILL[ is.na(cdata_curves$HILL)] <- 1

  #



  #recreating titration curves keyword_1------------------------------------------
#  mainMatrix <- data.frame(x=double(),y=double(),z=double())
#  rowIndex = 0;

  # write("have points, creating curves", stderr())
  #for(r in keyReadouts) {
  #  # write(paste0("key readout = ",r), stderr())
  #}

  #for(r in activityReadouts) {
  #  # write(paste0("activity readout = ",r), stderr())
  #}


  # Need to use the same strategy used for setting z on points, interleaving response curves for different readouts
  numReadouts <- length(activityReadouts)
  readoutCount <- 1
  curveCount <- 1

  for(currReadout in activityReadouts) {
    currCurveSet <- cdata_curves[[currReadout]]

    # print("In readout loop for curves, current curvset dim:")
    # print(currReadout)
    # print(keyReadouts)
    # print(currReadout %in% keyReadouts)
    # print(dim(currCurveSet))
    # print("curve set col names")
    # print(colnames(currCurveSet))



    curveCount = readoutCount
    l <- nrow(currCurveSet)

    for (i in 1:l) {

      #currReadout = currCurveSet[i,"readout"]

      # write(paste0("processing for curves, index= ", i, " currReadout = ", currReadout, " readout = ", readout), stderr())

      if((currCurveSet[i,"Fit_Output"]==1) && (currReadout %in% keyReadouts)) {

        # print("inside the if statement...")
        # # write("checkpoint 1", stderr())
        # for(n in names(waterfallLines)) {
        #   # # write(paste0("name in waterfall lines = **",n,"**"),stderr())
        #   # # write(paste0("currReadout = **",currReadout, "**"), stderr())
        #   # # write(paste0("n == currReadout: ",(n == currReadout)), stderr())
        #   # # write(paste0("identical(n,currReadout): ",(identical(n, currReadout))), stderr())
        #   # # write(paste0("identical(n, currReadout, num.eq=F): ",(identical(n, currReadout, num.eq=F))), stderr())
        #   # # write(paste0("identical(n, currReadout, num.eq=F, ignore.bytecode = F): ",(identical(n, currReadout, num.eq=F, ignore.bytecode = F))), stderr())
        #   # # write(paste0("identical(n, currReadout, num.eq=F, ignore.bytecode = F, ignore.environment = F): ",(identical(n, currReadout, num.eq=F, ignore.bytecode = F, ignore.environment = F))), stderr())
        #   # ## write(paste0("Encoding(n), Encoding(currReadout)", Encoding(n), ", ", Encoding(currReadout)), stderr())
        #
        #   junk <- waterfallLines[[n]]
        #   # write("checkpoint 1a", stderr())
        #   #currReadout <- n
        #   junk2 <- waterfallLines[[currReadout]]
        #   # write("checkpoint 1b", stderr())
        # }

        wfLines <- waterfallLines[[currReadout]]
        # write("checkpoint 2", stderr())
        # print(typeof(currCurveSet))
        # print("vals")
        # print(currCurveSet[i,])

        d1 <- data.frame(f(currCurveSet[i,], c(minConc[[currReadout]], maxConc[[currReadout]]), interleave = T, curveRes = curveResolution), z=currCurveSet[i,"compIndex"])
        # write("checkpoint 3", stderr())

        # d1 <- data.frame(f(currCurveSet[i,], c(minConc[[currReadout]], maxConc[[currReadout]])), z=i)
        if(nrow(currCurveSet) > 1) {
          # print("In block currCurveSet > 1")
          # # write("checkpoint 4", stderr())
          wfLines <- dplyr::bind_rows(wfLines, data.frame(x=d1[,1], z=d1[,'z'], y=d1[,2]))
          # # write("checkpoint 5", stderr())
          wfLines <- dplyr::bind_rows(wfLines, data.frame(x=NA, z=NA, y=1))
          # # write("checkpoint 6", stderr())
          # print("Exiting block currCurveSet > 1")

        }
        # write("checkpoint 7", stderr())
        waterfallLines[[currReadout]] <- wfLines
        # write("checkpoint 8", stderr())
        curveCount <- curveCount + numReadouts
      }
      # write("checkpoint 9", stderr())
    }
    # write("checkpoint finished df for currReadout", stderr())

    readoutCount <- readoutCount + 1
  }

  # write("in plot function, we've created the lines and points data", stderr())
  # write("about to reformat for plotly...", stderr())

  # verify numeric data
  for(i in 1:length(waterfallPoints)) {
    m <- waterfallPoints[[i]]
    m$x <- as.double(m$x)
    m$y <- as.double(m$y)
    m$z <- as.double(m$z)
    waterfallPoints[[i]] <- m
  }

  for(i in 1:length(waterfallLines)) {
    m <- waterfallLines[[i]]
    m$x <- as.double(m$x)
    m$y <- as.double(m$y)
    m$z <- as.double(m$z)
    waterfallLines[[i]] <- m
  }

  TimeAfterCurveBuild <- Sys.time()

  #correcting data type if needed
  # mainMatrix$x <- as.double(mainMatrix$x)
  # mainMatrix$y <- as.double(mainMatrix$y)
  # mainMatrix$z <- as.double(mainMatrix$z)
  #
  # waterfall_LINES_data_1 <- mainMatrix

  #recreating titration curves keyword_2------------------------------------------
  # mainMatrix <- data.frame(x=double(),y=double(),z=double())
  # rowIndex = 0;
  # l <- nrow(cdata_curves)
  # for (i in 1:l) {
  #   if(cdata_curves[i,"Fit_Output"]==1 && cdata_curves[i,"readout"]==keyword_2) {
  #     rowIndex = rowIndex+1
  #     d1 <- data.frame(f(cdata_curves[i,], c(lowerBound, upperBound)), z=i)
  #
  #     #add multiple rows
  #     mainMatrix <- dplyr::bind_rows(mainMatrix, data.frame(x=d1[,1], z=i, y=d1[,2]))
  #     #needed for break mechanic when graphing
  #     mainMatrix <- dplyr::bind_rows(mainMatrix, data.frame(x=NA, z=NA, y=1))
  #
  #   }
  # }

  #correcting data type if needed
  # mainMatrix$x <- as.double(mainMatrix$x)
  # mainMatrix$y <- as.double(mainMatrix$y)
  # mainMatrix$z <- as.double(mainMatrix$z)
  #
  # waterfall_LINES_data_2 <- mainMatrix

  #3D Graphing -------------------------------------------------------------------

  #CHANGING POP-UP WINDOW PARAMETERS
  # Changing parameters defaults from open3d() using par3d() for what I consider
  # an initial good view of graph and window size
  # The open3() window can be manually enlarged and the graph can be rotated
  # using user's mouse

  # graph view coordinates

  # Plotly...
  resIndex = 1


  # prepare lines data
  for(n in names(waterfallLines)) {

    if(nrow(waterfallLines[[n]]) < 1) {
      next
    }

    d <- waterfallLines[[n]]
    d$readout <- n
    d2 <- waterfallPoints[[n]]
    d2$readout <- n

    if(resIndex == 1) {
      wfl <- d
      wfp <- d2
    } else {
      wfl <- rbind(wfl, d)
      wfp <- rbind(wfp, d2)
    }
    resIndex = resIndex + 1
  }

  resIndex = 1
  if(plotInactivePoints) {
    d <- waterfallPoints[['inactive']]
    d$readout <- 'inactive'
    wfp <- rbind(wfp, d)
  }

  #get the color palettes set...
  linePal <- lineColors
  linePal <- setNames(linePal, activityReadouts)

  pointPal <- pointColors
  if(plotInactivePoints) {
    pointPal <- setNames(pointPal, c(activityReadouts, 'inactive'))
  } else {
    pointPal <- setNames(pointPal, activityReadouts)
  }

  # right verical plane
  # default #999494
  axx <- list(
    title = axisTitles[['concTitle']],
    backgroundcolor=planeColors[['rightPlaneColor']],
    gridcolor=gridColor,
    zerolinecolor=gridColor,
    showbackground = T,
    showspikes=F,
    hovertext='skip',
    ticks='outside'
  )

  # left vertical plane
  # #6e6868
  axy <- list(
    title = axisTitles[['curveTitle']],
    autorange='reversed',
    backgroundcolor=planeColors[['leftPlaneColor']],
    gridcolor=gridColor,
    zerolinecolor=gridColor,
    showbackground = T,
    showspikes=F,
    showticklabels = showCurveNumberLabels
  )

  # base plane
  # default #b8b6b6
  axz <- list(
    title = axisTitles[['respTitle']],
    backgroundcolor=planeColors[['basePlaneColor']],
    gridcolor=gridColor,
    zerolinecolor=gridColor,
    showbackground = T,
    showspikes=F,
    ticks='outside'
  )

  if(!is.null(responseAxisConfig)) {
    axz[['autotick']] <- F
    axz[['tickmode']] <- 'linear'
    axz[['range']] <- list(responseAxisConfig$min, responseAxisConfig$max)
    #axz[['nticks']] <- responseAxisConfig$nTics
    axz[['tick0']] <- responseAxisConfig$firstTick
    axz[['dtick']] <- responseAxisConfig$tickSizeVal
    #axz[['dtickrange']] <- list(-400, NULL)
  }

  if(!is.null(responseAxisConfig)) {
    axz[['autotick']] <- F
    axz[['tickmode']] <- 'linear'
    axz[['range']] <- list(responseAxisConfig$min, responseAxisConfig$max)
    axz[['tick0']] <- responseAxisConfig$firstTick
    axz[['dtick']] <- responseAxisConfig$tickSizeVal
  }

  if(!is.null(concAxisConfig)) {
    axx[['autotick']] <- F
    axx[['tickmode']] <- 'linear'
    axx[['range']] <- list(concAxisConfig$min, concAxisConfig$max)
    axx[['tick0']] <- concAxisConfig$firstTick
    axx[['dtick']] <- concAxisConfig$tickSizeVal
  }


  if(showCurveNumberLabels) {
    axy[['ticks']] <- 'outside'
  }

  TimeJustBeforePlotlyCall <- Sys.time()

  p <- plotly::plot_ly(wfl, x=~x, y=~z, z=~y, color=~readout, colors=linePal) %>% group_by(readout)

  p <- p %>% plotly::layout(scene = list(aspectratio = aspect, xaxis=axx, yaxis=axy, zaxis=axz, camera = list(eye = list(x = 2.25, y = 2.25, z = 0.3))))

  p <- p %>% plotly::add_lines(line=list(width=lineWeight)) %>% style(hoverinfo='none')

  p <- p %>% plotly::add_trace(x=~wfp$x, y=~wfp$z, z=~wfp$y, color=~wfp$readout, colors=pointPal,
                                 type='scatter3d', mode='markers', marker = list(size=pointSize), inherit=F)

  return(p)
}


evaluateInputFile <- function(filePath) {

  fileExt <- tools::file_ext(filePath)

  fileFormats <- c("generic", "ncats_qhts")

  status <- list()
  status$valid <- TRUE

  status$inputFile <- filePath

  if(tolower(fileExt) == 'csv') {
    formatConcHeader <- scan(filePath, nlines = 1, what = character())
    formatConcHeader <- unlist(strsplit(formatConcHeader, ","))
  } else {
    formatConcHeader <- openxlsx::read.xlsx(filePath, sheet=1, colNames=F, rows = as.numeric(c(1)))
    formatConcHeader <- as.character(formatConcHeader[1,])
  }

  for(i in 1:length(formatConcHeader)) {
    formatConcHeader[i] <- tolower(trimws(formatConcHeader[i]))
  }

  # error checking

  if(!is.null(formatConcHeader) && length(formatConcHeader) > 3) {
    formatTag <- formatConcHeader[1]
    status$fileFormat <- formatConcHeader[2]

    if(formatTag != 'format') {
      status$valid <- FALSE
      status$problem <- "The file is missing the 'Format:' tag as the first row and first column."
      return(status)
    }


    if(!('log_conc_m' %in% formatConcHeader)) {
      status$valid <- FALSE
      status$problem <-"No 'Log_Conc_M' tag. <br>The first row should have this tag and a series of log molar concentrations."
      return(status)
    }

    # made it through... so far....

  } else {
    status$valid <- FALSE
    status$problem <- paste0("The first row should contain the 'Format:' tag, a format (generic_qhts or ncats_qhts) <br> and Log Molar concentrations over data columns.<br>
                       This file only contains ", length(formatConcHeader), " values.")
    return(status)
  }

  # Error checking done

  # get log conc vals
    haveLogConcTag <- FALSE
    concVector <- c()
    for(header in formatConcHeader) {
      if(haveLogConcTag) {

        conc <- as.numeric(header)

        if(!is.na(conc)) {
          concVector <- c(concVector, conc)
        }
      }
      if(tolower(header) == 'log_conc_m') {
        haveLogConcTag = TRUE
      }
    }


  status$logConc <- concVector
  status$concLength <- length(concVector)

  # capture optimal initial concRange
  minConc <- min(concVector)
  maxConc <- max(concVector)

  minConcRange <- floor(minConc)
  maxConcRange <- ceiling(maxConc)

  if(minConc - minConcRange > 0.5) {
    minConcRange = minConcRange + 0.5
  }

  if(maxConcRange-maxConc > 0.5) {
    maxConcRange = maxConcRange - 0.5
  }

  status$minConc <- minConcRange
  status$maxConc <- maxConcRange



  # set reasonable bounds for the range min and max
  respLimits <- evalDataRange(filePath, concVector)
  status$minResp <- respLimits[1]
  status$maxResp <- respLimits[2]
  # skip the first header and read table

  if(tolower(fileExt)  == 'csv') {
    cdata <- read.csv(filePath, skip=1, header=T, na.string="null")
  } else {
    cdata <- openxlsx::read.xlsx(filePath, sheet=1, startRow=2)
  }

  heads <- colnames(cdata)

  if(status$fileFormat == 'ncats_qhts') {
    readoutColName <- "Sample.Data.Type"
  } else {
    readoutColName <- "Readout"
  }

    readoutCol <- heads[grep(readoutColName, heads, ignore.case = T)]

    if(!is.null(readoutCol) && length(readoutCol > 0)) {
       readouts <- unique(cdata[[readoutCol]])
       status$readouts <- readouts
    } else {
      status$valid <- FALSE
      status$problem <- "Can't find a 'Readout' (generic_qhts format) or 'Sample Data Type' (ncats_qhts format) column<br>in the input file. Check column names."
      return(status)
    }


    # set default colors
    defaultColors <<- c('darkgreen', 'blue4', 'red4', 'gold3', 'darkseagreen4', 'lightsalmon4', 'darkorchid3',
                        'aquamarine4', 'darkorange3', 'sienna4', 'seagreen3', 'lemonchiffon4', 'lightskblue2',
                        'palegreen3', 'palevioletred3', 'peru', 'deeppink3', 'orangered3', 'purple4', 'gray20',
                        'plum3', 'wheat4', 'steelblue4', 'lightgoldenrod')


    status$defaultColors <- defaultColors[1:length(readouts)]

  return(status)
}

evalDataRange <- function(filePath, conc) {

  fileExt <- tools::file_ext(filePath)

  if(tolower(fileExt)  == 'csv') {
    cdata <- read.csv(filePath, header=TRUE, skip=1, na.string="null")
  } else {
    cdata <- openxlsx::read.xlsx(filePath, sheet=1, startRow=2)
  }

  dataCols <- c()

  for(i in 1:length(conc)){
    dataCols <- c(dataCols, paste("Data",(i-1), sep=""))
  }

  responseData <- as.matrix(cdata[,dataCols[length(dataCols)]])
  rangeQuantiles <- quantile(responseData, probs <- c(0.01, 0.99), na.rm=T)
  lowerLimit <- rangeQuantiles[[1]]
  if(lowerLimit < 0) {
    lowerLimit <- ceiling(lowerLimit)
  } else {
    lowerLimit <- floor(lowerLimit)
  }

  lowerLimit <- lowerLimit - (lowerLimit %% 50)

  upperLimit <- rangeQuantiles[[2]]
  if(upperLimit > 0) {
    upperLimit <- ceiling(upperLimit)
  } else {
    upperLimit <- floor(upperLimit)
  }

  upperLimit <- upperLimit - (upperLimit %% 50) + 50

  return(list(lowerLimit, upperLimit))
}

extractConcFromFile <- function(inputFile) {

  res <- list()

  fileExt <- tools::file_ext(inputFile)

  if(tolower(fileExt) == 'csv') {
    formatConcHeader <- scan(filePath, nlines = 1, what = character())
    formatConcHeader <- unlist(strsplit(formatConcHeader, ","))
  } else {
    formatConcHeader <- openxlsx::read.xlsx(filePath, sheet=1, colNames=F, rows = as.numeric(c(1)))
    formatConcHeader <- as.character(formatConcHeader[1,])
  }

  # lower case for some key tags
  for(i in 1:length(formatConcHeader)) {
    formatConcHeader[i] <- tolower(trimws(formatConcHeader[i]))
  }

  # error check block
  if(!is.null(formatConcHeader) && length(formatConcHeader) > 3) {
    formatTag <- formatConcHeader[1]
    res$fileFormat <- formatConcHeader[2]

    if(formatTag != 'format') {
      res$valid <- FALSE
      res$problem <- "The file is missing the 'Format:' tag as the first row and first column."
      return(res)
    }


    if(!('log_conc_m' %in% formatConcHeader)) {
      res$valid <- FALSE
      res$problem <-"No 'Log_Conc_M' tag. <br>The first row should have this tag and a series of log molar concentrations."
      return(res)
    }

    # made it through... so far....

  } else {
    res$valid <- FALSE
    res$problem <- paste0("The first row should contain the 'Format:' tag, a format (generic_qhts or ncats_qhts) <br> and Log Molar concentrations over data columns.<br>
                       This file only contains ", length(formatConcHeader), " values.")
    return(res)
  }

  haveLogConcTag <- FALSE
  concVector <- c()
  for(header in formatConcHeader) {
    if(haveLogConcTag) {

      conc <- as.numeric(header)

      if(!is.na(conc)) {
        concVector <- c(concVector, conc)
      }
    }
    if(tolower(header) == 'log_conc_m') {
      haveLogConcTag = TRUE
    }
  }

  res$logConc <- concVector

  return(res)
}


#EXPORTING THE IMAGE FILES -----------------------------------------------------

## the following code is put as a comment to allow the user to position the interactive
## graph however they please
## NAME THE FILE HOWEVER YOU LIKE WITHIN THE "" MARKS BELOW

# rgl.snapshot(filename = ".png")
# rgl.postscript(".svg", fmt="svg")

