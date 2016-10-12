library(cvplotoperator)

context("cvplotoperator test")

test_that("simple", {
  data = data.frame(x=c(1.0,2.0,3.0,4.0), mycolor=c(1,2,1,1), id=c(1,1,2,2))
  result = computeCVData(data, value='x',color='mycolor',groupBy='id')
  expect_equal(colnames(result), c('id','m','stdev','cv', 'nreps', 'lvar', 'pane' ))
  expect_equal(result[['id']], c(1,2));
})


