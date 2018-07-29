#' Event - Illness
#' 
#' calculates the probability of an Illness. Not implemented yet. Just a placeholder
#' 
#' @param input_name nothing
#' @param input nothing
#' @return per_disability  
#' @export 
#' 

Event_Ilness <- function(input_name,input)
{

  
  return(per_disability)
  
}

#' Event - Illness
#' 
#' build the nodes and edges in the graph object necessary to include the Loss Aggregator
#' @param Rgraph The Rgraph object (package specific object to save the graph)
#' @return Rgraph
#' @export 
#' 
build_graph_Event_Ilness<-function(Rgraph){
  Rgraph=rbind(Rgraph,c('Risk','Data_none','Event_Illness',F))
  Rgraph=rbind(Rgraph,c('Event_Unemployment','Data_per_unemployment','Event_Illness',F))
  
  return(Rgraph)
}