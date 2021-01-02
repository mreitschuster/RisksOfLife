#' Get all paths from RiskOfLife graph object
#' 
#' Currently no circular paths are allowed, e.g. in one path the same node cannot be hit more than once.
#' This might be subject to change. Currently I assume such circular dependencies are not relevant in the overall calculation as their probability is extremly small.
#' 
#' 
#' @param Rgraph The Rgraph object (package specific object to save the graph)
#' @return Rgraph The object that represents the graph structure / the causal chain
#' @export 
#' 
#' 
Graph_getAllPaths<-function(Rgraph, startNode, endNode, onlyActive){

  # just some unused code, which I use to quickly setup the variables needed to play with the function
  if (FALSE){
    startNode='Risk'
    endNode='Monetary_Loss'
    Rgraph=Graph_Build()
    onlyActive=FALSE
  }
  
  # Starting positions
  # show all next hops that have the correct start node and if onlyActive is true, show only the active ones
  paths_next_step=Rgraph[Rgraph[,'In']==startNode & (!onlyActive | as.logical(Rgraph[,'Active'])),] 
  if (nrow(paths_next_step)==0){stop('Graph_getAllPaths: no paths found from startNode')}
  
  # create a list for the immediate 1st steps
  paths=list(paths_next_step[1,,drop=FALSE])
  if (nrow(paths_next_step)>1){
    for (j in 2:nrow(paths_next_step)){
      paths=c(paths,list(paths_next_step[j,,drop=FALSE]))
    }
  }  
  rm(paths_next_step)
  
  # start the recursion
  hop_number=1
  new_paths=list()
  for (i in 1:length(paths)){
    new_paths=c(new_paths,recursiveStep_getPath(Rgraph,endNode,onlyActive,hop_number+1,list(paths[[i]])))
  }
  
  # add names to the paths
  paths.name=vector(length = length(new_paths))
  for (i in 1:length(new_paths)){
    paths.name[i]=paste(new_paths[[i]][,'In'],collapse=' -> ')
    paths.name[i]=paste(paths.name[i],new_paths[[i]][nrow(new_paths[[i]]),'Out'],sep =' -> ')
  }
  #paths.name=cbind(paths.id,paths.name)
  #paths.name=data.frame(paths.id,paths.name)
  
  new_paths=setNames(new_paths,paths.name)
  
  return(new_paths)
}
  
#' recursive function to find the next step from node to node
#' 
#' @param Rgraph The Rgraph object (package specific object to save the graph)
#' @param endNode Node to find
#' @param onlyActive Flag if we should consider only active edges of the graph
#' @param hop_number just counting. used to maintain integrity of data structure
#' @param paths the path we already use. this is necessary to avoid circular references
#' @return new_paths 
#' @export 
#' 
#' 
recursiveStep_getPath<-function(Rgraph, endNode, onlyActive, hop_number, paths){

  #print(paste('recursiveStep_getPath: Hop Number:' ,hop_number))
  
  if (FALSE){
    paths=list(paths[[1]])
  }
  
  if (length(paths)>1){stop('Graph_getAllPaths: in this recursive function we expect always only 1 path as input. The output will be more paths.')}
  
  startNode=paths[[1]][hop_number-1,'Out']
  paths_next_step=Rgraph[Rgraph[,'In']==startNode & (!onlyActive | as.logical(Rgraph[,'Active'])),,drop=FALSE]
  
  # if we are in a dead end return nothing
  if (nrow(paths_next_step)==0){
    return(NULL)
  }
  

  # for each new found paths_next_step we add a path by copying the former path and appending the last step
  old_path=paths
  paths[[1]]=NULL
  index_offset=0
  for (j in 1:nrow(paths_next_step)){
    if (paths_next_step[j,'Out'] %in% old_path[[1]][,'Out']){ # this is to check if we go into a circular reference
      
      #print(paths_next_step[j,'Out'])
      #print(old_path[[1]])
      index_offset=index_offset-1
      warning(paste('recursiveStep_getPath: Found a circular reference for Node',paths_next_step[j,'Out']))
    }else{
      paths=c(paths,old_path)
      paths[[j+index_offset]]=rbind(paths[[j+index_offset]],paths_next_step[j,,drop=FALSE])
    }
  }
  
  new_paths=list()
  for (j in 1:length(paths)){
    if (paths[[j]][hop_number,'Out']==endNode){
      # wuhu. we found a valid path. nothing to do any more. expect following up on all the other loose ends
      new_paths=c(new_paths,list(paths[[j]]))
    }else{
      new_paths=c(new_paths,recursiveStep_getPath(Rgraph,endNode,onlyActive,hop_number+1,list(paths[[j]])))
      #print(new_paths)
      
    }
  }

#  print(paths)
#  print(new_paths)
  return(new_paths)
}
