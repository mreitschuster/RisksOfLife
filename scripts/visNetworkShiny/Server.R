#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


require(shiny)
#require(igraph)
require(visNetwork)



update_edges_per_selected_paths <- function(selection,paths.to.edges.map){
  if (is.null(selection)){
    visNetworkProxy("network") %>%
      visUnselectAll()
  }else{
    edges_selection=paths.to.edges.map[paths.to.edges.map[,'Path'] %in% selection,'Edge.Id']
    visNetworkProxy("network") %>%
      visSelectEdges(id = edges_selection)
  }
}


calculate_path_weight <- function(path_results, threshold=0, SeverityMeasure='mean'){
  #calculate the size of the edges 
  path_weight=vector(mode='numeric',length = length(path_results))
  names(path_weight)=names(path_results)
  for (i in 1:length(path_results)){
    last_node=length(path_results[[i]])
    probability = path_results[[i]][[last_node]][,'Probability']
    severity = path_results[[i]][[last_node]][,'Severity']
    
    if (SeverityMeasure=='mean'){ # use the mean
      xs_severity = pmax(0,severity-threshold)
      path_weight[i]=sum(probability*xs_severity)/sum(probability)
    }else if (SeverityMeasure=='prob'){ # use the probability of exceeding the threshold
      path_weight[i]=sum((severity>=threshold)*probability)
    }else{
      stop(paste0("selected severity measure:'",SeverityMeasure,"' is unknown"))
    }
  }
  return(path_weight)
}
  

#### Server ##########################################################################
shinyServer(function(input, output, session) {
    
  
  #### prepare the data ##########################################################################
  # Build the graph. 
  Rgraph=Graph_Build()
  
  # get all active routes between risk and loss
  paths<-Graph_getAllPaths(Rgraph, startNode= 'Risk', endNode='Monetary_Loss', onlyActive=TRUE)
  
  # calculate risks for all given paths
  path_results=Graph_calculate_paths(paths)
  
  # get visnetwork objects
  Edges=Graph_convert_to_visNetwork_edges(Rgraph,paths,T)
  Nodes=Graph_convert_to_visNetwork_nodes(Rgraph,paths,T)
  Edges$value=1
  
  # maps to easily look up with path has which nodes and edges
  paths.to.nodes.map=Paths_convert_to_paths_nodes_map(paths)
  paths.to.edges.map=Paths_convert_to_paths_edges_map(paths,Edges)
  
  
  ######## build inputs ###########################################################################
  updateCheckboxGroupInput(session, "SelectPath",
                           label="Select Paths",
                           choices=names(paths),selected = names(paths), inline=F)
  
  ######## read inputs ###########################################################################  
  
  observe({ 
    selection_nodes=input$current_node_id # just added to ensure the edges are updated once a node is selected. 
    # somehow visnetwork changes edges highlight/selection when nodes are selected, and I havent figured out how to prevent it.
    
    #### Trying to select nodes. failed #######
    #visNetworkProxy("network") %>%
      #visSelectNodes(selection_nodes, clickEvent = F,highlightEdges=T)
    # I tried to also highlight the nodes. But it messes with the edges selection. if highlightEdges si True then it select the neighbouring edges
    # if highlightEdges is F it deselects all edges
    
    #### Select edges ###############
    selection_edges = input$SelectPath
    #save(selection,file='dummy.Rdata')

    update_edges_per_selected_paths(selection_edges,paths.to.edges.map)
  })
  
  ######## output initialization ###########################################################################  
  output$network <- renderVisNetwork({

      visNetwork(Nodes, Edges) %>%
      visEdges(arrows='to',smooth=F)%>%
      visPhysics(solver='barnesHut', # 'barnesHut', 'repulsion', 'hierarchicalRepulsion', 'forceAtlas2Based'. 
                 maxVelocity=50,
                 minVelocity=0.1,
                 timestep=0.5,
                 stabilization=list(
                   enabled = TRUE,
                   onlyDynamicEdges = TRUE,
                   fit = TRUE),
                 enabled = F)  %>% 
      visOptions(highlightNearest = F) %>%
      visInteraction(navigationButtons = TRUE,
                    # selectConnectedEdges=F,
                     selectable = T) %>%
      visNodes(color = list(color = "grey",highlight ="blue")) %>%
      #visEdges(color = list(color = "grey",highlight ="blue"))%>%
      visEdges(color   = list(color = "grey",highlight ="blue"),
               scaling = list(label                 = list(enabled=FALSE),
                              customScalingFunction = 'function (min,max,total,value) {
                                            if (max === min) {
                                              return 0.5;
                                            }
                                            else {
                                              return Math.max(0,0.5* value / max);
                                            }
                                          }') ) %>%
      visEvents(selectNode = "function(nodes) {Shiny.setInputValue('current_node_id', nodes.nodes,{priority: 'event'});}") %>%
      visEvents(dragStart = "function(nodes)  {Shiny.setInputValue('current_node_id', nodes.nodes,{priority: 'event'});}")
  })
  
  ######## output updates ###########################################################################   
#  observe({
#    if (input$SeverityScale=='sameSize'){
#      visNetworkProxy("network") %>% visEdges(scaling = list(customScalingFunction = "function (min,max,total,value) {return 0.5;}")) 
#    } else if (input$SeverityScale=='log'){
#      visNetworkProxy("network") %>% visEdges(scaling = list(customScalingFunction = "function (min,max,total,value) {return Math.log(value); }")) 
#    } else if (input$SeverityScale=='linear'){
#      visNetworkProxy("network") %>% visEdges(scaling = list(customScalingFunction = "function (min,max,total,value) {return value*scale; }")) 
#    } else {stop(paste0("severity scale: ", input$SeverityScale, " is unknown"))}
#  })
  

  observe({
    if (FALSE){
      input=data.frame(Threshold=0,SeverityMeasure='prob',SeverityScale='log')
    }

      
    Edges.Updated=Edges
    if (input$SeverityScale!='sameSize'){
      Edges.Updated$value=NULL
      #calculate the size of the edges
      path_weight=calculate_path_weight(path_results, threshold=input$Threshold, SeverityMeasure=input$SeverityMeasure)
      x=merge(x=paths.to.edges.map,y=data.frame(paths=names(path_weight),path.weight=path_weight),by.x='Path',by.y='paths')
      y=aggregate(x$path.weight,by=list(x$Edge.Id), FUN=sum)
      names(y)=c('Edge.Id', 'value')
      Edges.Updated=merge(x=Edges.Updated,y=y,by.x='id',by.y='Edge.Id')
      if (input$SeverityScale=='log'){
        Edges.Updated$value=log(Edges.Updated$value+1)
      } else if (input$SeverityScale=='linear'){
        #Edges.Updated$value=Edges.Updated$value /max(Edges.Updated$value)
      } else {stop(paste0("severity scale: ", input$SeverityScale, " is unknown"))}
    }
    #Edges.Updated$value=Edges.Updated$value * input$ScalingFactor
    
    
    if (!input$checkLabelEdges){
      Edges.Updated$label=''
    }
    
    Nodes.Updated=Nodes
    if (!input$checkLabelNodes){
      Nodes.Updated$label='' 
    }
    
    visNetworkProxy("network") %>%
      visUpdateEdges(edges = Edges.Updated) %>%
      visUpdateNodes(nodes = Nodes.Updated)
  })
  
  output$info <- renderText({
    # I currently use this for debugging
    #graph=visNetworkProxy("network")
    #paste0("visgetselectededges ", visGetSelectedEdges(graph, input = paste0(graph$id, "_selectedEdges")))
    
    #paste0(input$current_node_id)
    #paste0(input$checkLabelEdges)
    
    #paste0(str(input$ScalingFactor))
    paste0(input$ScalingFactor)
    
    #paste0(input$SeverityScale)
    #paste0("visgetselectededges ", visGetSelectedEdges(graph, input = paste0(graph$id, "_selectedEdges")))
    
  })
  
  # Add to your server 
  observeEvent(input$browser,{
    browser()
  })
  
})



#shinyApp(ui, server)

