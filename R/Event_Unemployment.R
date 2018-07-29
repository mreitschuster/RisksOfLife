#' Event - Unemployment
#' 
#' calculates the probability of a unemplyoment. Not implemented yet. Just a placeholder
#' 
#' @param input_name nothing
#' @param input nothing
#' @return Data_per_unemployment  
#' @export 
#' 

Event_Unemployment <- function(input_name,input)
{

  
  return(Data_per_unemployment)
  
}

#' Event - Stock Market Crash
#' 
#' build the nodes and edges in the graph object necessary to include the Loss Aggregator
#' @param Rgraph The Rgraph object (package specific object to save the graph)
#' @return Rgraph
#' @export 
#' 
build_graph_Event_Unemployment<-function(Rgraph){
  Rgraph=rbind(Rgraph,c('Risk','Data_none','Event_Unemployment',F))
  Rgraph=rbind(Rgraph,c('Event_Illness','per_disability','Event_Unemployment',F))
  Rgraph=rbind(Rgraph,c('Event_Accident','per_disability','Event_Unemployment',F))
  Rgraph=rbind(Rgraph,c('Event_StockCrash','Data_per_StockLoss','Event_Unemployment',F))
  return(Rgraph)
}