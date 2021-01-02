#' calculate given paths
#' 
#' 
#' @param paths The paths object representing the causality chains
#' @return path_results
#' @export 
#' 

Graph_calculate_paths <- function(paths){
  
  path_results=list()
 # print(paste('Number of different paths:', length(paths)))
  
  for (i_path in 1:length(paths)){
    
    #print(paste('i_path=',i_path))
    #print(paths[[i_path]])
    tmp_results=list()
    var_obj=NULL  # the variable is used iteratively within a path, so lets set it to NULL in the beginning
    for (j_node in 1:nrow(paths[[i_path]])){
      #print(paste('j_node=',j_node))
      #print(paths[[i_path]][j_node,])
      
      if (!( paths[[i_path]][j_node,'Out']=='Monetary_Loss')){
        code=paths[[i_path]][j_node,'Out']
        data_name=paths[[i_path]][j_node,'DataType']
        funct=match.fun(code)
        var_obj=funct(input_name=data_name,input=var_obj)
        #tmp_results=c(tmp_results,list(var_obj))
        var_obj_list=list(var_obj)
        var_obj_list=setNames(var_obj_list,code)
        tmp_results=c(tmp_results,var_obj_list)
        
        
       # paths[[i_path]][[j_node]]$output=var_obj
        
      } 
    }
    path_results=c(path_results,list(tmp_results))
  }
  path_results=setNames(path_results,names(paths))
  return(path_results)
}