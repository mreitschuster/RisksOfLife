#' Event - Stock Market Crash
#' 
#' calculates the probability of a stock crash. Not implemented yet. Just a placeholder
#' 
#' @param input_name nothing
#' @param input nothing
#' @return Data_per_StockLoss  
#' @export 
#' 

Event_StockCrash <- function(input_name,input)
{

  
  return(Data_per_StockLoss)
  
}

#' Event - Stock Market Crash
#' 
#' build the nodes and edges in the graph object necessary to include the Loss Aggregator
#' @param Rgraph The Rgraph object (package specific object to save the graph)
#' @return Rgraph
#' @export 
#' 
build_graph_Event_StockCrash<-function(Rgraph){
  Rgraph=rbind(Rgraph,c('Risk','Data_none','Event_StockCrash',F))
  Rgraph=rbind(Rgraph,c('Event_War','Data_bool_War','Event_StockCrash',F))
  
  return(Rgraph)
}