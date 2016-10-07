library(testthat)
library(bnutil)
library(cvplotoperator)

getdata = function() {

  return(AnnotatedData$new(data=validation, metadata=validation.meta))
}

setResult = function(annotatedResult){
#result = annotatedResult$data
return(annotatedResult)
}

bnMessageHandler = bnshiny::BNMessageHandler$new()
bnMessageHandler$getDataHandler = getdata
bnMessageHandler$setResultHandler = setResult


bnshiny::startBNTestShiny('cvplotoperator', sessionType='run', bnMessageHandler=bnMessageHandler)
