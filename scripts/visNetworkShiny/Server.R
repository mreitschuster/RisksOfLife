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


calculate_path_weight <- function(path_results, threshold=0, flag_mean_xs_severity=T){
  #calculate the size of the edges 
  path_weight=vector(mode='numeric',length = length(path_results))
  names(path_weight)=names(path_results)
  for (i in 1:length(path_results)){
    last_node=length(path_results[[i]])
    probability = path_results[[i]][[last_node]][,'Probability']
    severity = path_results[[i]][[last_node]][,'Severity']
    
    if (flag_mean_xs_severity==T){ # use the mean
      xs_severity = pmax(0,severity-threshold)
      path_weight[i]=sum(probability*xs_severity)/sum(probability)
    }else{ # use the probability of exceeding the threshold
      path_weight[i]=sum((severity>=threshold)*probability)
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
  
  # maps to easily look up with path has which nodes and edges
  paths.to.nodes.map=Paths_convert_to_paths_nodes_map(paths)
  paths.to.edges.map=Paths_convert_to_paths_edges_map(paths,Edges)
  
  
  ######## build inputs ###########################################################################
  updateCheckboxGroupInput(session, "SelectPath",
                           label="Select Paths",
                           choices=names(paths),selected = names(paths), inline=F)
  
  ######## read inputs ###########################################################################  
  observe({
    x=input$current_node_id # just added to ensure the edges are updated once a node is selected. 
    # somehow visnetwork changes edges highlight/selection when nodes are selected, and I havent figured out how to prevent it.
    selection = input$SelectPath
    #save(selection,file='dummy.Rdata')
    update_edges_per_selected_paths(selection = input$SelectPath,paths.to.edges.map)

  })
  
  ######## output ###########################################################################  
  output$network <- renderVisNetwork({

    path_weight=calculate_path_weight(path_results, threshold=0, flag_mean_xs_severity=T)
    # add some code here
    x=merge(x=paths.to.edges.map,y=data.frame(paths=names(path_weight),path.weight=path_weight),by.x='Path',by.y='paths')
    y=aggregate(x$path.weight,by=list(x$Edge.Id), FUN=sum)
    names(y)=c('Edge.Id', 'value')
    Edges.with.Width=merge(x=Edges,y=y,by.x='id',by.y='Edge.Id')
    Edges.with.Width$value=Edges.with.Width$value/max(Edges.with.Width$value)*1
    
    visNetwork(Nodes, Edges.with.Width) %>%
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
      visEdges(color = list(color = "grey",highlight ="blue")) %>%
      visEvents(selectNode = "function(nodes) {Shiny.setInputValue('current_node_id', nodes.nodes,{priority: 'event'});}") %>%
      visEvents(dragStart = "function(nodes)  {Shiny.setInputValue('current_node_id', nodes.nodes,{priority: 'event'});}")
  })
  
  output$info <- renderText({
    # I currently use this for debugging
    #graph=visNetworkProxy("network")
    #paste0("visgetselectededges ", visGetSelectedEdges(graph, input = paste0(graph$id, "_selectedEdges")))
    paste0(input$current_node_id)
    #paste0("visgetselectededges ", visGetSelectedEdges(graph, input = paste0(graph$id, "_selectedEdges")))
    
  })
  
  # Add to your server 
  observeEvent(input$browser,{
    browser()
  })
  
})



#shinyApp(ui, server)

