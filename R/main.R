#' @import dplyr
#' @import shiny
#' @import bnutil
#' @import ggplot2

#
#' @export
operatorProperties = function() {

}

#' @export
shinyServerRun = function(input, output, session, context) {

  output$body = renderUI({
    mainPanel(
      h4("Creating CV plots"),
      p("Please wait ..."),
      verbatimTextOutput("banner")
    )
  })

  getDataReactive = context$getData()
  getFolderReactive = context$getRunFolder()
  getPropertiesReactive = context$getPropertiesAsMap()

  observe({
    getData=getDataReactive$value
    getFolder = getFolderReactive$value
    #getProperties = getPropertiesReactive$value

    if (is.null(getData)){
      return()
    }

    if(is.null(getFolder)){
      return()
    }

    annotatedData = getData()
    folder = getFolder()

    print(folder)

    output$banner = renderText({

      if(annotatedData$hasColors){
        Color = droplevels(interaction(annotatedData$getColors()))
      } else {
        Color = "Main"
      }
      data = data.frame(annotatedData$getData(outlier=FALSE), Color = Color)
      aResult = computeCVData(data)
      forBn = !(colnames(aResult) %in% c("pane", "lvar"))
      meta = data.frame(labelDescription = colnames(aResult)[forBn],
                        groupingType = c("rowSeq", "colSeq", "QuantitationType","QuantitationType","QuantitationType","QuantitationType", "QuantitationType"))


      result = AnnotatedData$new(data = as.data.frame(aResult[,forBn ]), metadata = meta)
      context$setResult(result)
      save(file = file.path(folder, "runData.RData"), aResult)
      return("Done!!")
    })

  })
}

#' @export
shinyServerShowResults = function(input, output, session, context) {

  output$body = renderUI({
    fluidPage(
      titlePanel("CV plots"),
      sidebarLayout(
        sidebarPanel(
          wellPanel(
            checkboxInput("dofit", "Show error model fit", value = TRUE),
            sliderInput("phigh", label = h5("Quantile for high signal spots"),min = 0.8, max = 1, value = 0.98),
            sliderInput("plow" , label = h5("Quantile for low signal spots"), min = 0, max = 0.2, value = 0.05)
          ),
          wellPanel(
            selectInput("plottype", label = h4("Select plot type"),
                        choices = list("CV plot" = 1, "SNR plot" = 2, "SD plot" = 3), selected = 1),
            checkboxInput("collapse", "Collapse panes", value = FALSE),
            checkboxInput("logx" , "Logarithmic x-axis", value = FALSE),
            textInput("xmin",    label = "x-axis lower limit", value = "0"),
            textInput("xmax",    label=  "x-axis upper limit", value = "auto"),
            textInput('ymin',    label = "y-axis lower limit", value = "0"),
            textInput('ymax',    label = "y-axis upper limit", value = "auto")
          )
        ),
        mainPanel(plotOutput("cvplot"),
                  tableOutput("fitresult")
        )
      )
    )
  })
  getFolderReactive = context$getRunFolder()

  observe({

    getFolder = getFolderReactive$value
    if (is.null(getFolder)) return()
    folder = getFolder()

    print(folder)

    load(file.path(folder, "runData.RData"))

    doFit = reactive({
      if(!input$collapse){
        aResult = aResult %>% group_by(pane) %>% do(cvmodel(., pLow = input$plow, pHigh = input$phigh))
      } else {
        aResult = cvmodel(aResult, pLow = input$plow, pHigh = input$phigh)
      }
      return(aResult)
    })

    cfgPlot = function(){
      suppressWarnings({
        if (input$dofit){
          aResult = doFit()
        }
        xLim = as.numeric(c(input$xmin, input$xmax))
        yLim = as.numeric(c(input$ymin, input$ymax))


        if(input$plottype == 1){
          aPlot = cvPlot(aResult, showFit = input$dofit,
                         xLog = input$logx,
                         xLim = xLim,
                         yLim = yLim,
                         collapse = input$collapse)

        } else if (input$plottype == 2){
          aPlot = snrPlot(aResult, showFit = input$dofit,
                         xLog = input$logx,
                         xLim = xLim,
                         yLim = yLim,
                         collapse = input$collapse)
        } else if (input$plottype == 3){
          aPlot = sdPlot(aResult, showFit = input$dofit,
                          xLog = input$logx,
                          xLim = xLim,
                          yLim = yLim,
                          collapse = input$collapse)
        }
        aPlot = aPlot + theme_bw()
      })
    }

    output$cvplot = renderPlot({
      suppressWarnings({
        aPlot = cfgPlot()
      })
      return(aPlot)
    })

    output$fitresult = renderTable({
      if(input$dofit){
        aResult = doFit()
        if(!input$collapse){
        aTable = aResult %>% group_by(pane) %>%
          summarise(.,std.low = sqrt(identity1(ssq0)) , CV.high = sqrt(identity1(ssq1)))
        } else {
          aTable  = data.frame(pane = "All panes combined", std.low = aResult$ssq0[1], CV.high = sqrt(aResult$ssq1[1]))
        }
      } else {
        aTable = data.frame()
      }
      if (dim(aTable)[2] == 3)
      {
        aTable = data.frame(aTable, snr.high = -10*log10(aTable$CV.high))
        colnames(aTable) = c("Pane", "Std low signals", "CV high signals", "SNR high signals (dB)")
        return(aTable)
      }
    })

  })
}
