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

#rm(list=ls())
#working_folder='/mnt/maRAIDfs/Projekte/2018 Risks of Life/'


RisksOfLife_main <- function(){
  graph=startGraph("http://localhost:7474/db/data", username="neo4j", password="dumdidumneo4j")
  
  # Build the graph. this hopefully will get replaced in the future by a more user-friednly way to input the graph structure to R
  #source(paste(working_folder,'Neo4j_test v4 Build Graph.R',sep=''))
  #source('Build Graph.R')
  error_description=Graph_Build(graph)
  #rm('Graph_Build')
  
  # Validation Checks on Graph integrity
  #source(paste(working_folder,'Neo4j_test v4 Graph Validation.R',sep=''))
  #source('Graph Validation.R')
  error_description=Graph_Validation(graph)
  #rm('Graph_Validation')
  
  # get all active routes between risk and loss
  query='MATCH p=(o{name:"Risk"})-[r:causes*]->(x{name:"Monetary Loss"}) WHERE ALL(y IN rels(p) WHERE y.active = TRUE) RETURN p'
  data_paths=getPaths(graph,query) # paths are a special object in Neo4j (i think)
  data_nodes=lapply(data_paths,nodes) # nodes is a function to extract the node information from a path. we want the node inofs from all paths.
  
  
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