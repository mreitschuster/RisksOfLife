#' Impact - Loss of Earnings
#' 
#' given the natural units input it calculates the monetary loss.
#' 
#' @param input_name currently only 'bool_dead' is supported
#' @param input the input data, e.g. the variable 'bool_dead' 
#' @return data_monetaryLoss  
#' @export 
#' 
Impact_LoE <- function(input_name,input)
{
  if (input_name=='bool_dead'){
    bool_dead=input
    x=Val_Data_bool_dead(bool_dead)
  } else {
    stop('Error: unknown input.')
  }
  # assuming loss is 100'€ if dead. 0 if alive
  cost_of_death=1e5
  data_monetaryLoss=bool_dead
  dimnames(data_monetaryLoss) = list(c("no loss", "loss"),c("Probability", "Severity"))
  data_monetaryLoss[,2]=bool_dead[,2]*cost_of_death
  return(data_monetaryLoss)
}


#' Impact - Loss of Earnings
#' 
#' build the nodes and edges in the graph object necessary to include the Loss Aggregator
#' @param Rgraph The Rgraph object (package specific object to save the graph)
#' @return Rgraph
#' @export 
#' 
build_graph_Impact_LoE<-function(Rgraph){
  Rgraph=rbind(Rgraph,c('Event_Death','Data_bool_dead','Impact_LoE',T))
  Rgraph=rbind(Rgraph,c('Event_Unemployment','Data_per_unemployment','Impact_LoE',F))
  return(Rgraph)
}