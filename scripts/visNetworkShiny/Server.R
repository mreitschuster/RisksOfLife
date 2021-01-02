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



update_edges_per_selected_paths <- function(selection,paths.name,paths.to.edges.map){

  #save(selection,file='dummy.Rdata')
  if (is.null(selection)){
    visNetworkProxy("network") %>%
      visUnselectAll()
  }else{
    selection1=merge(x=as.data.frame(x=selection),y=paths.name,by.x='selection',by.y='paths.name')
    selection2=merge(x=paths.to.edges.map,y=selection1,by.x='Path',by.y='paths.id')
    edges_selection=selection2$Edge.Id
    visNetworkProxy("network") %>%
      visSelectEdges(id = edges_selection)
  }
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
  
  # get paths as strings
  paths.id=1:length(paths)
  paths.name=names(paths)
  paths.name=data.frame(paths.id,paths.name)

  
  # get static igraph object
  Edges=Graph_convert_to_visNetwork_edges(Rgraph,paths,T)
  Nodes=Graph_convert_to_visNetwork_nodes(Rgraph,paths,T)
  
  
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
    update_edges_per_selected_paths(selection = input$SelectPath,paths.name,paths.to.edges.map)

  })
  
  ######## output ###########################################################################  
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
      #visOptions(selectedBy = list(variable = "paths", multiple = T, highlight = T),
      #           nodesIdSelection = list(enabled = T, useLabels = T))  %>% 
      visNodes(color = list(color = "grey",highlight ="red")) %>%
      visEdges(color = list(color = "grey",highlight ="blue"))   %>%
      visEvents(selectNode = "function(nodes) {Shiny.setInputValue('current_node_id', nodes.nodes,{priority: 'event'});}") %>%
      visEvents(dragStart = "function(nodes)  {Shiny.setInputValue('current_node_id', nodes.nodes,{priority: 'event'});}")
    #%>%
#      visEvents(selectNode="function() {update_edges_per_selected_paths(selection = input$SelectPath,paths.name,paths.to.edges.map)}")
  })
  
  output$info <- renderText({
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

