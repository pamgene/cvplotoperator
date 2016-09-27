#'computeCvData.
#'
#'Computes stats per BN cell.
#'
#'@param annotatedData A annotatedData (bnUtil)  with replicate values per cell.
#'@return
#'aResult, a data frame with various per cell stats as columns.
#' @export
computeCVData = function(annotatedData) {

  data = annotatedData$data %>% select(rowSeq,
                                       colSeq,
                                       y=value,
                                       bOut=IsOutlier)
  data = subset(data, !bOut)
  aResult = data %>%group_by(rowSeq, colSeq) %>% summarise(m = mean(y), stdev = sd(y), cv = stdev/m, nreps = length(y))
  return(aResult)
}





