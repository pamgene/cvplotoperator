% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cvstats.R
\name{computeCVData}
\alias{computeCVData}
\title{computeCvData.}
\usage{
computeCVData(df, value = "value", color = "Color", groupBy = c("rowSeq",
  "colSeq"))
}
\arguments{
\item{annotatedData}{A annotatedData (bnUtil)  with replicate values per cell.}
}
\value{
aResult, a data frame with various per cell stats as columns.
}
\description{
Computes stats per BN cell.
}

