
#' Convert the RiskOfLife specific graph format to a igraph compatible node dataframe
#' @param Rgraph the Rgraph RoL object
#' @param onlyActive only consider active relations
#' @return Nodes  data frame that should be compatible with igraph's graph_from_data_frame function
#' @export 
#' 
Graph_convert_to_igraph_nodes <- function(Rgraph, onlyActive=TRUE){
  
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
  
  #Nodes=as.data.frame(unique(c(Rgraph[(!onlyActive | as.logical(Rgraph[,'Active'])),"In"],Rgraph[(!onlyActive | as.logical(Rgraph[,'Active'])),"Out"])))
  Edges=as.data.frame(Rgraph[(!onlyActive | as.logical(Rgraph[,'Active'])),])

  #Edges$DataType=NULL
  return(Edges)
  #g <- graph_from_data_frame(Edges, directed=TRUE, vertices=Nodes)
  #return(g)
}


#' Convert the RiskOfLife specific graph format to a visNetwork compatible node dataframe
#' @param Rgraph the Rgraph RoL object
#' @param onlyActive only consider active relations
#' @return Nodes  data frame that should be compatible with visNetwork's graph_from_data_frame function
#' @export 
#' 
Graph_convert_to_visNetwork_nodes <- function(Rgraph, paths, onlyActive=TRUE){
  id = unique(c(Rgraph[(!onlyActive | as.logical(Rgraph[,'Active'])),"In"],Rgraph[(!onlyActive | as.logical(Rgraph[,'Active'])),"Out"]))
  label=id
  Nodes=as.data.frame(cbind(id,label))
  
  #use the paths as group (not visgroup) names - add attribute paths to Nodes
  Nodes$paths=rep(NA,nrow(Nodes))
  for (i in 1:length(paths)){
    for (j in 1:nrow(Nodes)){
      if(Nodes$id[j] %in% unique(rbind(paths[[i]][,"In"],paths[[i]][,"Out"]))){
        if(is.na(Nodes$paths[j])){
          Nodes$paths[j]=names(paths)[i]
        }else{
          Nodes$paths[j]=paste(Nodes$paths[j],",",names(paths)[i])
        }
      }
    }
  }  
  return(Nodes)
  
}


#' Convert the RiskOfLife specific graph format to an visNetwork compatbile edges dataframe
#' @param Rgraph the Rgraph RoL object
#' @param onlyActive only consider active relations
#' @return edges data frame that should be compatible with visNetwork's graph_from_data_frame function
#' @export 
#' 
Graph_convert_to_visNetwork_edges <- function(Rgraph, paths, onlyActive){
  
  
  Edges=as.data.frame(Rgraph[(!onlyActive | as.logical(Rgraph[,'Active'])),])
  Fields=colnames(Edges)
  Fields[Fields=="In"]="from"
  Fields[Fields=="Out"]="to"
  Fields[Fields=="DataType"]="label"
  colnames(Edges)=Fields
  
  # we add attribute 'paths'. use the paths as group (not visgroup) names - add attribute paths to Edges
  Edges$paths=rep(NA,nrow(Edges))
  for (i in 1:length(paths)){
    for (j in 1:nrow(Edges)){
      if(any((paths[[i]][,"In"] %in%  Edges[j,"from"]) & (paths[[i]][,"Out"] %in%  Edges[j,"to"]))){
        if(is.na(Edges$paths[j])){
          Edges$paths[j]=names(paths)[i]
        }else{
          Edges$paths[j]=paste(Edges$paths[j],",",names(paths)[i])
        }
      }
    }
  } 
  
  Edges$id=as.numeric(as.character(Edges$id))
  #Edges$Id=1:nrow(Edges)
  return(Edges)
  
}


#' Convert the RiskOfLife specific Paths format to a map from paths to nodes
#' @param Paths the Paths RoL object
#' @return table that has paths and corresponding nodes
#' @export 
#' 
Paths_convert_to_paths_nodes_map <- function(paths){
  paths.to.nodes.map=NULL
  for (i in 1:length(paths)){
    nodes.tmp=unique(rbind(paths[[i]][,"In",drop=F],paths[[i]][,"Out",drop=F]))
    paths.tmp=rep(names(paths)[i],length(nodes.tmp))
    paths.to.nodes.map=rbind(paths.to.nodes.map,data.frame(Path=paths.tmp,Node=nodes.tmp))
  }
  
  colnames(paths.to.nodes.map)=c('Path','Node')
  return(paths.to.nodes.map)
}

#' Convert the RiskOfLife specific Paths format to a map from paths to edges
#' @param Paths the Paths RoL object
#' @param Edges
#' @return table that has paths and corresponding edges
#' @export 
#' 
Paths_convert_to_paths_edges_map <- function(paths,Edges){
  paths.to.edges.map=NULL
  for (i in 1:length(paths)){
    edges.id.tmp=as.numeric(as.character(paths[[i]][,"id",drop=F]))
    paths.tmp=rep(names(paths)[i],length(edges.id.tmp))
    paths.to.edges.map=rbind(paths.to.edges.map,data.frame(Path=paths.tmp,Id=edges.id.tmp))
    
  }
  colnames(paths.to.edges.map)=c('Path','Edge.Id')
  return(paths.to.edges.map)
}

