
test_that("Probabilities equal 100%, severities strictly positive",{
  
  Rgraph       = Graph_Build()
  paths        = Graph_getAllPaths(Rgraph, startNode= 'Risk', endNode='Monetary_Loss', onlyActive=TRUE)
  path_results = Graph_calculate_paths(paths)
  
  for (i in 1:length(path_results)){
    for (j in 1:length(path_results[[i]])){
      testthat::expect_equal(sum(path_results[[i]][[j]][,'Probability']),1) # check sum prob equal 1
      #print(paste0(i, " / ", j))
      for (k in 1:nrow(path_results[[i]][[j]])){
        testthat::expect_gte(path_results[[i]][[j]][k,'Probability'],0) # check each Probability >= 0
        testthat::expect_gte(path_results[[i]][[j]][k,'Severity'],0) # check each Severity >= 0
      }
    }
  }
})
