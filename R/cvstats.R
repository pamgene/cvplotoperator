#'computeCvData.
#'
#'Computes stats per BN cell.
#'
#'@param annotatedData A annotatedData (bnUtil)  with replicate values per cell.
#'@return
#'aResult, a data frame with various per cell stats as columns.
#' @export
computeCVData = function(annotatedData) {
  colors = annotatedData$metadata$groupingType %in% "Color"
  data = subset(annotatedData$data, !IsOutlier)
  if(any(colors)){
    Color = droplevels(interaction(data[, colors]))
  } else {
    Color = "Main"
  }
  data = data.frame(data, Color = Color)
  aResult = data %>%group_by(rowSeq, colSeq) %>%
    summarise( m = mean(value),
               stdev = sd(value),
               cv = stdev/m,
               nreps = length(value),
               lvar = var(log(value)),
               pane = identity1(Color))
  return(aResult)
}

cvmodel = function(aResult, pLow = 0.05, pHigh = 0.98, maxIter = 25){
  bLow 	= aResult$m <= quantile(aResult$m, pLow);
  bHigh 	= aResult$m >= quantile(aResult$m, pHigh);
  var = aResult$stdev^2

  bModel = bLow|bHigh

  ssq0 = NaN
  ssq1 = NaN
  pres = NaN
  iter = 0
  while(any(bModel)){
    ssq0 = pooledVarEst( var[bLow], aResult$nreps[bLow]);
    lssq1 = median(aResult$lvar[bHigh], na.rm = TRUE)
    ssq1 = exp(lssq1) - 1
    #calculate presence values for all spots
    m0 = aResult$m;
    m0[m0<0] = 0
    pres = ( sqrt(ssq1) * m0) /(sqrt(ssq1)*m0 + sqrt(ssq0))

    bLow = pres < pLow
    bHigh = pres > pHigh

    if (all(!bLow) | all(!bHigh)){
      ssq0 = NaN
      ssq1 = NaN
      break
    }
    if ( all( (bLow|bHigh) == bModel) ){
      break
    } else {
      bModel = bLow|bHigh
      iter = iter + 1
    }
    if (iter > maxIter){
      break
    }
  }
  aResult = data.frame(aResult, ssq0 = ssq0, ssq1 = ssq1, pres = pres)
  return(aResult)
}


identity1 = function(.).[1]

pooledVarEst<-function(s2, n){
  est = sum(s2 * (n-1), na.rm = TRUE ) / (sum(n) - length(n))
  return(est)
}




