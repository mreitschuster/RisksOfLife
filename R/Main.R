#' calculate stuff for RisksOfLife
#' 
#' This function is the main function for RisksOfLife. not sure if I will keep it. I used it to check whether the whole concept actually calculates plausible results.
#' It will likely bee repacled by a more sophisticated API connecting the results to an GUI
#' 
#' @param graph The graph object representing the neo4j database as produced by Rneo4j
#' @return nothing
#' @export 
#' 


#inspired by
#https://www.youtube.com/watch?v=bdQ90y9Pefo&_ga=2.4855219.143141403.1531656343-614646524.1531656343

#install.packages("devtools")
#library(devtools)
#devtools::install_git("https://github.com/nicolewhite/RNeo4j")
#
#library(RNeo4j)

#     rm(list=ls())
#working_folder='/mnt/maRAIDfs/Projekte/2018 Risks of Life/'


RisksOfLife_main <- function(){

  # Build the graph. 
  Rgraph=Graph_Build()

  # get all active routes between risk and loss
  paths<-Graph_getAllPaths(Rgraph, 'Risk', 'Monetary_Loss', FALSE)
  

  print(paste('Number of different paths:', length(data_nodes)))
  for (i_path in 1:length(data_nodes)){
    print(i_path)
    var_obj=NULL
    
    for (j_node in 1:length(data_nodes[[i_path]])){
      print(data_nodes[[i_path]][[j_node]]$name)
      
      if (!(data_nodes[[i_path]][[j_node]]$name=='Risk' || data_nodes[[i_path]][[j_node]]$name=='Monetary Loss')){
        #print(data_nodes[[i_path]][[j_node]]$name)
        query="match (n{name:{name1}})-[:hasOutput]->(o)-[:isInputFor]->(m{name:{name2}}) return o.name"
        data_name=cypher(graph,query,name1=data_nodes[[i_path]][[j_node-1]]$name,name2=data_nodes[[i_path]][[j_node]]$name)
        code=data_nodes[[i_path]][[j_node]]$code
        code=substr(code,1,nchar(code)-2)
        funct=match.fun(code)
        var_obj=funct(input_name=data_name,input=var_obj)
        
        data_nodes[[i_path]][[j_node]]$output=var_obj
        
      } 
    }
  }
}