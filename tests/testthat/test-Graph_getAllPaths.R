
test_that("Expected Errors",{
  testthat::expect_error(Graph_getAllPaths(Rgraph=NULL, startNode=NULL, endNode=NULL, onlyActive=T))
  testthat::expect_error(Graph_getAllPaths(Rgraph=NULL, startNode='gnu', endNode='gnu', onlyActive=T))
  
  testthat::expect_error(Graph_getAllPaths(Rgraph, startNode= 'Risk', endNode='Risk', onlyActive=TRUE))         # start and end node are the same
  testthat::expect_error(Graph_getAllPaths(Rgraph, startNode= 'Gnu', endNode='Monetary_Loss', onlyActive=TRUE)) # start node doesnt exist
  testthat::expect_error(Graph_getAllPaths(Rgraph, startNode= 'Risk', endNode='Gnu', onlyActive=TRUE))          # end node doesnt exist
})



test_that("onlyActive tests",{

  Rgraph=Graph_Build()
  pathsActive <- Graph_getAllPaths(Rgraph, startNode= 'Risk', endNode='Monetary_Loss', onlyActive=TRUE)
  pathsAll    <- suppressWarnings(Graph_getAllPaths(Rgraph, startNode= 'Risk', endNode='Monetary_Loss', onlyActive=F))
  testthat::expect_lte(length(pathsActive),length(pathsAll))
  
})



test_that("Rgraph results",{
  #save(Rgraph,file = '~/Repos/RisksOfLife/tests/testthat/test_sample-Rgraph-2021-01-02.Rdata') # in case Rgraph needs updating
  load(file = '~/Repos/RisksOfLife/tests/testthat/test_sample-Rgraph-2021-01-02.Rdata')
  pathsActive <- Graph_getAllPaths(Rgraph, startNode= 'Risk', endNode='Monetary_Loss', onlyActive=TRUE)
  testthat::expect_equal(length(pathsActive),3)
  
  
})
