#' Impact -  Cost of Care
#' 
#' given the natural units input it calculates the  the monetary loss.
#' 
#' @param input_name currently only 'per_disability' is supported
#' @param input the input data, e.g. the variable 'per_disability' 
#' @return data_monetaryLoss  
#' @export 
Impact_CoC <- function(input_name,input)
{
  if (input_name=='per_disability'){
    per_disability=input
    x=Val_Data_per_disability(per_disability)
  } else {
    stop('Error: unknown input.')
  }
  
  # assuming CoC is linear in disability
  costs_per_disability_degree=1e4
  data_monetaryLoss=per_disability

  data_monetaryLoss[,2]=per_disability[,2]*costs_per_disability_degree
  return(data_monetaryLoss)
}

#' Impact -  Cost of Care
#' 
#' build the nodes and edges in the graph object necessary to include the Loss Aggregator
#' @param Rgraph The Rgraph object (package specific object to save the graph)
#' @return Rgraph
#' @export 
#' 
build_graph_Impact_CoC<-function(Rgraph){
  Rgraph=rbind(Rgraph,c('Event_Accident','Data_per_disability','Impact_CoC',T))
  Rgraph=rbind(Rgraph,c('Event_Illness','Data_per_disability','Impact_CoC',F))
  return(Rgraph)
}