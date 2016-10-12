#'computeCvData.
#'
#'Computes stats per BN cell.
#'
#'@param annotatedData A annotatedData (bnUtil)  with replicate values per cell.
#'@return
#'aResult, a data frame with various per cell stats as columns.
#' @export
computeCVData = function(df, value='value', color='Color', groupBy=c('rowSeq','colSeq')) {

  data = data.frame(value=df[[value]], Color=df[[color]], df[groupBy])

  suppressWarnings({
    aResult = data %>%group_by_(.dots=groupBy) %>%
      summarise( m = mean(value),
                 stdev = sd(value),
                 cv = stdev/m,
                 nreps = as.double(length(value)),
                 lvar = var(log(value)),
                 pane = identity1(Color))
  })
  return(aResult)
}

cvmodel = function(aResult, pLow = 0.05, pHigh = 0.95, maxIter = 25){
  bLow 	= aResult$m <= quantile(aResult$m, pLow);
  bHigh 	= aResult$m >= quantile(aResult$m, pHigh);
  var = aResult$stdev^2

  bModel = bLow|bHigh

  ssq0 = NaN
  ssq1 = NaN
  pres = NaN
  iter = 0
  tryCatch({
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
  }
  , error = function(e)e
  )
  if (!is.nan(ssq0) & !is.nan(ssq1)){
    cvFit = sqrt( ( (ssq1)*(aResult$m^2) + (ssq0))/(aResult$m^2) )
  } else {
    cvFit = NaN
  }
  aResult = data.frame(aResult, ssq0 = ssq0, ssq1 = ssq1, cvFit = cvFit, bHigh = bHigh,  presence = pres)
  return(aResult)
}

cvPlot = function(aFrame, xLim = c(NA,NA),xLog = FALSE, yLim = c(0, 0.5), showFit = TRUE){
  if (showFit){
    aPlot = ggplot(aFrame, aes(x = m, y = cv, colour = presence, shape = bHigh) ) + geom_point()  + geom_line(aes(x = m, y = cvFit) , colour = "red")
  } else {
    aPlot = ggplot(aFrame, aes(x = m, y = cv)) + geom_point(colour ="blue")
  }
  aPlot = aPlot + facet_wrap(~pane)
  aPlot = aPlot + ylim(yLim)
  aPlot = aPlot + xlim(xLim)
  if(xLog){
    aPlot = aPlot + scale_x_log10()
  }

  return(aPlot)
}


identity1 = function(.).[1]

pooledVarEst<-function(s2, n){
  est = sum(s2 * (n-1), na.rm = TRUE ) / (sum(n) - length(n))
  return(est)
}





