#' Build causality Graph for RisksOfLife
#' 
#' This will hopefully be replaced by a load functionality on existing graphs, together with a more intuitive way to create the graphs.
#' 

#' @return Rgraph The object that represents the graph structure / the causal chain
#' @export 
#' 
Graph_Build <- function(){

    Rgraph=matrix(ncol=4)
    colnames(Rgraph)=c('In','DataType','Out','Active')
    Rgraph=build_graph_Event_Accident(Rgraph)
    Rgraph=build_graph_Event_Death(Rgraph)
    Rgraph=build_graph_Event_StockCrash(Rgraph)
    Rgraph=build_graph_Event_Unemployment(Rgraph)
    Rgraph=build_graph_Event_Ilness(Rgraph)
    Rgraph=build_graph_Event_War(Rgraph)
    Rgraph=build_graph_Impact_StockDevaluation(Rgraph)
    Rgraph=build_graph_Impact_LoE(Rgraph)
    Rgraph=build_graph_Impact_CoC(Rgraph)
    Rgraph=build_graph_Monetary_Loss(Rgraph)
    
    Rgraph=Rgraph[-1,] # remove the NA row
    return(Rgraph)
}


#' Convert the RiskOfLife specific graph format to igraph
#' 
#' 

#' @return igraph The object that represents the graph structure / the causal chain
#' @export 
#' 
Graph_convert_to_igraph<-function(Rgraph, onlyActive){
  #plot(graph_from_literal(a--+b, b+--c))
  #g4 <- set_graph_attr(g4, "name", "Email Network")
  
  if (FALSE){
    onlyActive=TRUE
  }
  Nodes=as.data.frame(unique(c(Rgraph[(!onlyActive | as.logical(Rgraph[,'Active'])),"In"],Rgraph[(!onlyActive | as.logical(Rgraph[,'Active'])),"Out"])))
  Edges=as.data.frame(Rgraph[(!onlyActive | as.logical(Rgraph[,'Active'])),])
  Edges$DataType=NULL
  #colnames(Edges)[colnames(Edges) %in% "In"]="from"
  #colnames(Edges)[colnames(Edges) %in% "Out"]="to"

  g <- graph_from_data_frame(Edges, directed=TRUE, vertices=Nodes)
  return(g)
  
  
  
}
