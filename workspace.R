library(testthat)
library(bnutil)
library(cvplotoperator)

getdata = function() {

 metadata = data.frame(labelDescription= colnames(RepData),
                           groupingType=c("value",
                                          "IsOutlier",
                                          "QuantitationType",
                                          "ids",
                                          "sids",
                                          "rowSeq",
                                          "colSeq",
                                          "Color",
                                          "Array",
                                          "Spot"))

  return(AnnotatedData$new(data=RepData, metadata=metadata))
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
