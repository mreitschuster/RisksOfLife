#' Main R file to show the dataflow of RisksOfLife
#' 
#' This function is the main function for RisksOfLife. not sure if I will keep it. I used it to check whether the whole concept actually calculates plausible results.
#' It will likely be replaced by jaavascript that then directly accesses the "Graph*"-functions
#' 
#' @return path_results
#' @export 
#' 


RisksOfLife_main <- function(x){
  # Build the graph. 
  Rgraph=Graph_Build()

  # get all active routes between risk and loss
  paths<-Graph_getAllPaths(Rgraph, startNode= 'Risk', endNode='Monetary_Loss', onlyActive=TRUE)

  #  calculate risks for all given paths
  path_results=Graph_calculate_paths(paths)
  
  y=list(bla='I am desperate to get JS running....')
  return(y)
}