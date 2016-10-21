library(testthat)
library(bnutil)
library(cvplotoperator)

getdata = function() AnnotatedData$new(data=validation, metadata=validation.meta)

getRunfolder = function() file.path(getwd(), 'run')

setResult = function(annotatedResult){

  print(annotatedResult)

  data = annotatedResult$data
  meta = annotatedResult$metadata

  expect_equal(dim(data), c(11010,7))
  expect_equal(colnames(data),c("rowSeq","colSeq","m","stdev","cv","snr_db","nreps" ))

}

bnMessageHandler = bnshiny::BNMessageHandler$new()
bnMessageHandler$getRunFolderHandler = getRunfolder
bnMessageHandler$getDataHandler = getdata
bnMessageHandler$setResultHandler = setResult


bnshiny::startBNTestShiny('cvplotoperator', sessionType='show', bnMessageHandler=bnMessageHandler)
