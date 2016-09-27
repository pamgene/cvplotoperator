#' @import dplyr
#' @import shiny
#' @import bnutil
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
      verbatimTextOutput("done")
    )
  })

  getDataReactive = context$getData()

  observe({

    getData=getDataReactive$value
    if (is.null(getData)){
      print('getData is null')
      return()
    }

    data = getData()

    output$done = renderText({
      result = computeCVData(data, progress=progress)
      context$setResult(result)
      renderPrint({ 'done' })()
    })
  })
}





