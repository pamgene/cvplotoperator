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
  getFolderReactive = context$getFolder()
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

    data = getData()
    folder = getFolder()

    output$banner = renderText({
      aResult = computeCVData(data)
      forBn = !(colnames(aResult) %in% c("pane", "lvar"))
      meta = data.frame(labelDescription = colnames(aResult)[forBn],
                        groupingType = c("rowSeq", "colSeq", "QuantitationType","QuantitationType","QuantitationType", "QuantitationType"))

      edit(aResult[,forBn])
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
            checkboxInput("dofit", "Show CV model fit", value = TRUE),
            sliderInput("phigh", label = h5("Quantile for high signal spots"),min = 0.8, max = 1, value = 0.98),
            sliderInput("plow" , label = h5("Quantile for low signal spots"), min = 0, max = 0.2, value = 0.05)
          ),
          wellPanel(
            checkboxInput("logx" , "Logarithmic x-axis", value = FALSE),
            textInput("xmin",    label = "x-axis lower limit", value = "0"),
            textInput("xmax",    label=  "x-axis upper limit", value = "auto"),
            textInput('ymin',    label = "y-axis lower limit", value = "0"),
            textInput('ymax',    label = "y-axis upper limit", value = "1")
          )
        ),
        mainPanel(plotOutput("cvplot"),
                  tableOutput("fitresult")
        )
      )
    )
  })
  getFolderReactive = context$getFolder()

  observe({

    getFolder = getFolderReactive$value
    if (is.null(getFolder)) return()
    folder = getFolder()

    load(file.path(folder, "runData.RData"))

    doFit = reactive({
      aResult %>% group_by(pane) %>% do(cvmodel(., pLow = input$plow, pHigh = input$phigh))

    })

    cfgPlot = function(){
      if (input$dofit){
        aResult = doFit()
      }
      xLim = as.numeric(c(input$xmin, input$xmax))
      yLim = as.numeric(c(input$ymin, input$ymax))
      aPlot = cvPlot(aResult, showFit = input$dofit, xLog = input$logx, xLim = xLim, yLim = yLim)
    }


    output$cvplot = renderPlot({
      aPlot = cfgPlot()
      return(aPlot)
    })

    output$fitresult = renderTable({
      if(input$dofit){
        aResult = doFit()
        aResult %>% group_by(pane) %>%
        summarise(.,std.low = sqrt(identity1(ssq0)) , CV.high = sqrt(identity1(ssq1)) )
      } else {
        data.frame()
      }
    })

  })
}
