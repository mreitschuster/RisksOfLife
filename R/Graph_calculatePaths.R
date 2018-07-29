#' calculate given paths
#' 
#' 
#' @param paths The paths object representing the causality chains
#' @return path_results
#' @export 
#' 


Graph_calculate_paths <- function(){
  
  print(paste('Number of different paths:', length(paths)))
  for (i_path in 1:length(paths)){
    print(paste('i_path:',i_path))
    print(paths[[i_path]])
    var_obj=NULL
    
    for (j_node in 1:nrow(paths[[i_path]])){
      print(paste('j_node:',j_node))
      print(paths[[i_path]][j_node,])
      
      if (!( paths[[i_path]][j_node,'Out']=='Monetary_Loss')){
        
        code=paths[[i_path]][j_node,'Out']
        data_name=paths[[i_path]][j_node,'DataType']
        funct=match.fun(code)
        var_obj=funct(input_name=data_name,input=var_obj)
        
        paths[[i_path]][[j_node]]$output=var_obj
        
      } 
    }
  }
}