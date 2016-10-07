library(testthat)
library(bnutil)
library(cvplotoperator)

getdata = function() {

  return(AnnotatedData$new(data=validation, metadata=validation.meta))
}

setResult = function(annotatedResult){

  result = annotatedResult$data


  # expect_equal(dim(result), c(2,8))
  # expect_equal(colnames(result), c('rowSeq',
  #                                  'colSeq',
  #                                  'slope',
  #                                  'intercept',
  #                                  'R2',
  #                                  'nPoints',
  #                                  'Result',
  #                                  'fractionPresent'))
}

bnMessageHandler = bnshiny::BNMessageHandler$new()
bnMessageHandler$getDataHandler = getdata
bnMessageHandler$setResultHandler = setResult


bnshiny::startBNTestShiny('cvplotoperator', sessionType='run', bnMessageHandler=bnMessageHandler)
