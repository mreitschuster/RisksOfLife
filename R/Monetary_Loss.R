#' Loss Aggregator
#' 
#' not functional yet. the intention is to have this as the last node for all paths. This node should then aggregate the results.
#' 
#' @param data_monetaryLoss This is an object containing frequency and severity of monetary losses. 
#' @return monetary, aggregated Loss 
#' @export 
#' 
Monetary_Loss <- function(data_monetaryLoss)
{
# still need to figure out what to do here
}


#' Loss Aggregator
#' 
#' build the nodes and edges in the graph object necessary to include the Loss Aggregator
#' @param Rgraph The Rgraph object (package specific object to save the graph)
#' @return Rgraph
#' @export 
#' 
build_graph_Monetary_Loss<-function(Rgraph){
  Rgraph=rbind(Rgraph,c('Impact_LoE','Data_per_monetaryLoss','Monetary_Loss',T))
  Rgraph=rbind(Rgraph,c('Impact_CoC','Data_per_monetaryLoss','Monetary_Loss',T))
  Rgraph=rbind(Rgraph,c('Impact_StockDevaluation','Data_per_monetaryLoss','Monetary_Loss',F))
  return(Rgraph)
}