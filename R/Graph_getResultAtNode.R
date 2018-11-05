#' calculate the overall distribution at a given node for a given set of paths
#' 
#' 
#' @param paths The paths object representing the causality chains
#' @param node At which node should the result be calculated
#' @return path_results
#' @export 
#' 

Graph_getResultAtNode <- function(paths,path_results,node){
  
  
  # Todo: validation if all results at same node have the same support
  # Todo: validation if node name exists in at least one path
  # Todo: last if should check if result object exists
  # todo: rethink how col1 is to be added. renormalize and ignore 1st row?
  index_found=0
  for (i_path in 1:length(paths)){
    for (j_node in 1:nrow(paths[[i_path]])){
      #print(paste(i_path," ", j_node))
      if (paths[[i_path]][j_node,'Out']==node){
        index_found=index_found+1
        if (index_found==1){
          result=path_results[[i_path]][[j_node]]
        } else {
          if (all(result[,2]==path_results[[i_path]][[j_node]][,2])){
            result[,1]=path_results[[i_path]][[j_node]][,1]
          } else {
            stop("The support of at least 2 results for the same node is different.")
          }
        }
      } 
    }
  }
  #if (!exists(result)){stop("No node with that name was found in the paths object.")}
  return(result)
}