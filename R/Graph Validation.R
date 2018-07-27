
library(RNeo4j)


Graph_Validation<-function(graph){
# Validation Checks on Graph integrity    

  

    
  # gives both pathways, one over casue, one over input&output
  #match (n)-[c:causes]->(m) MATCH (n)-[:hasOutput]->(o)-[:isInputFor]->(m) where n.name='Accident' and m.name='Unemployment' RETURN n,m,o
  
  
    # find nodes that have a causal relationship, but where we do not havbe matching input and output data in the active part of the graph
    #simplyfied querty: match (n)-[c:causes]->(m) where (not (n)-[:hasOutput]->()-[:isInputFor]->(m)) RETURN n,m
    error_data=cypher(graph,'match (n{active:TRUE})-[c:causes]->(m{active:TRUE}) where (not (n)-[:hasOutput]->({active:TRUE})-[:isInputFor]->(m)) RETURN n.name, m.name')
    if (!is.null(error_data)) {
      error_description=paste('Error: active parts of the Graph are inconsistent. some outputs do not match the inputs. Check the variable error_data')
      stop(error_description)
      return(error_description)
    }
    
    # find nodes that have a causal relationship, but where we do not havbe matching input and output data in the whole graph (also the non-active part)
    warning_data=cypher(graph,'match (n)-[c:causes]->(m) where (not (n)-[:hasOutput]->()-[:isInputFor]->(m)) RETURN n.name, m.name')
    if (!is.null(warning_data)) {
      warning(paste('Warning: the non-active Graph is inconsistent. some outputs do not match the inputs. Check the variable warning_data'))
    }
    
    # find nodes that having fitting in and output (in the active part of the graph), but are not connected
    warning_data=cypher(graph,'match  (n{active:TRUE})-[:hasOutput]->(o{active:TRUE})-[:isInputFor]->(m{active:TRUE}) where (not (n)-[:causes]->(m)) RETURN n.name,o.name,m.name')
    if (!is.null(warning_data)) {
      warning(paste('Warning: the active Graph could be inconsistent. some outputs match the inputs of other nodes, but there is no causal relationship. Check the variable warning_data'))
    }
    
    
    # checking if all active nodes have code & load the code
    # TODO: this does not find obvious errors in the code. maybe we should run all codes once....
    # TODO: check if the function is named per convention the same as the filename
    #code_list=cypher(graph,'match (n{active:TRUE}) where (n:Loss or n:Event or n:Impact) return n.name, n.code')
    #for (i in 1:nrow(code_list)){
     # if (is.na(code_list[i,2]) || code_list[i,2]==''){stop(paste('Error: the code for',code_list[i,1], 'does not exist'))
    #  }else{
        #source(paste(working_folder,code_list[i,2],sep=''))
      #  source(code_list[i,2])
     # }
    #}
    
    # TODO create a check that makes sure that for everey causal link there is exactly one connecting data type
    
    # checking if all active data types have code & load the code
    # TODO: this does not find obvious errors in the code. maybe we should run all codes once....
    # TODO: check if the function is named per convention the same as the filename
    #code_list=cypher(graph,'match ((n)-[:hasOutput]->(o{active:TRUE})-[:isInputFor]->(m)) where (n{active:TRUE})-[:causes]->(m{active:TRUE})  RETURN distinct(o.code)')
    #for (i in 1:nrow(code_list)){
    #  if (is.na(code_list[i,1]) || code_list[i,1]==''){stop(paste('Error: the code for',code_list[i,1], 'does not exist'))
    #  }else{
    #    #source(paste(working_folder,code_list[i,1],sep=''))
    #    source(code_list[i,1])
    #  }
    #}
        
  return(0)
}