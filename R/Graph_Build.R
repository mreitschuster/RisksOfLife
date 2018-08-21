#' Build causality Graph for RisksOfLife
#' 
#' This will hopefully be replaced by a load functionality on existing graphs, together with a more intuitive way to create the graphs.
#' @param none
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


#' Convert the RiskOfLife specific graph format to a igraph compatible node dataframe
#' @param Rgraph the Rgraph RoL object
#' @param onlyActive only consider active relations
#' @return Nodes  data frame that should be compatible with igraph's graph_from_data_frame function
#' @export 
#' 
Graph_convert_to_igraph_nodes <- function(Rgraph, onlyActive){

  if (FALSE){
    onlyActive=TRUE
  }
  Nodes=as.data.frame(unique(c(Rgraph[(!onlyActive | as.logical(Rgraph[,'Active'])),"In"],Rgraph[(!onlyActive | as.logical(Rgraph[,'Active'])),"Out"])))
  #Edges=as.data.frame(Rgraph[(!onlyActive | as.logical(Rgraph[,'Active'])),])
  #Edges$DataType=NULL
  return(Nodes)

 # g <- graph_from_data_frame(Edges, directed=TRUE, vertices=Nodes)
  #return(g)
}


#' Convert the RiskOfLife specific graph format to an igraph compatbile edges dataframe
#' @param Rgraph the Rgraph RoL object
#' @param onlyActive only consider active relations
#' @return edges data frame that should be compatible with igraph's graph_from_data_frame function
#' @export 
#' 
Graph_convert_to_igraph_edges <- function(Rgraph, onlyActive){
  
  if (FALSE){
    onlyActive=TRUE
  }
  #Nodes=as.data.frame(unique(c(Rgraph[(!onlyActive | as.logical(Rgraph[,'Active'])),"In"],Rgraph[(!onlyActive | as.logical(Rgraph[,'Active'])),"Out"])))
  Edges=as.data.frame(Rgraph[(!onlyActive | as.logical(Rgraph[,'Active'])),])
  #Edges$DataType=NULL
  return(Edges)
  #g <- graph_from_data_frame(Edges, directed=TRUE, vertices=Nodes)
  #return(g)
}
