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