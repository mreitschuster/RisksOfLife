#' Impact - Stock Devaluation
#' 
#' given the natural units input it calculates the monetary loss. Not implemented yet. Just a placeholder
#' 
#' @param input_name nothing
#' @param input nothing
#' @return data_monetaryLoss  
#' @export 
#' 
Impact_StockDevaluation <- function(input_name,input)
{

  return(data_monetaryLoss)
}


#' Impact - Stock Devaluation
#' 
#' build the nodes and edges in the graph object necessary to include the Loss Aggregator
#' @param Rgraph The Rgraph object (package specific object to save the graph)
#' @return Rgraph
#' @export 
#' 
build_graph_Impact_StockDevaluation<-function(Rgraph){
  Rgraph=rbind(Rgraph,c('Event_StockCrash','Data_per_StockLoss','Impact_StockDevaluation',F))
  return(Rgraph)
}