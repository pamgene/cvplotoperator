#' @import dplyr
#' @import shiny
#' @import bnutil
#' @import ggplot2

#
#' @export
operatorProperties = function() {
  aPropList = list(
    list('Fit CV model', list('Yes', 'No') )
  )
  return(aPropList)
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
    getProperties = getPropertiesReactive$value

    if (is.null(getData)){
      return()
    }

    data = getData()
    output$banner = renderText({
      aResult = computeCVData(data)
      forBn = !(colnames(aResult) %in% c("pane", "lvar"))
      meta = data.frame(labelDescription = colnames(aResult)[forBn],
                        groupingType = c("rowSeq", "colSeq", "QuantitationType","QuantitationType","QuantitationType", "QuantitationType"))
      result = AnnotatedData$new(data = aResult[,forBn ], metadata = meta)
      context$setResult(result)
      folder = getFolder()
      save(file = file.path(folder, "runData.RData"), aResult)
      return(folder)
    })
    #stopApp()
  })
}

#' @export
shinyServerShowResults = function(input, output, session, context) {

  output$body = renderUI({
    fluidPage(
      titlePanel("CV plots"),
      sidebarLayout(
        sidebarPanel(h4("Controls")),
        mainPanel(plotOutput("cvplot"))
      )
    )
  })
  getFolderReactive = context$getFolder()

  observe({

    getFolder = getFolderReactive$value
    if (is.null(getFolder)) return()
    folder = getFolder()

    load(file.path(folder, "runData.RData"))

    output$cvplot = renderPlot({
      aResult = aResult %>% group_by(pane) %>% do(cvmodel(.))
      aPlot = ggplot(aResult, aes(x = m, y = cv) ) + geom_point() + facet_wrap(~pane)
      aPlot = aPlot + ylim(c(0, 0.5))
      #print(aPlot)
      return(aPlot)
    })
  })
}
