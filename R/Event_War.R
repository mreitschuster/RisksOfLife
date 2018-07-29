#' Event - War
#' 
#' calculates the probability of a war.
#' 
#' @param input_name currently only 'Data_none' is supported
#' @param input currently unused
#' @return bool_War  
#' @export 
#' 


Event_War <- function()
{
  if (input_name=='Data_none'){
  } else {
    stop('Error: unknown input.')
  }
  
  probability_of_war=0.05 # per year. for a war in a particular country. this is based purely on gut feeling

  bool_war=matrix(c(1- probability_of_war, probability_of_war,0,1),nrow=2,ncol=2)
  dimnames(bool_war) = list(c("no War", "War"),c("Probability", "Severity"))
  # with severity currently we have only 2 options. 0 and 1. But there could also be a severity measure for the war.
  
  return(bool_war)
}


#' Event - War
#' 
#' build the nodes and edges in the graph object necessary to include the Loss Aggregator
#' @param Rgraph The Rgraph object (package specific object to save the graph)
#' @return Rgraph
#' @export 
#' 
build_graph_Event_War<-function(Rgraph){
  Rgraph=rbind(Rgraph,c('Risk','Data_none','Event_War',F))
  return(Rgraph)
}