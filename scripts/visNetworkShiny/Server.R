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
  #paths.id=paste("Path",1:length(paths),sep = "")
  paths.id=1:length(paths)
  paths.name=vector(length = length(paths))
  for (i in 1:length(paths)){
    paths.name[i]=paste(paths[[i]][,'In'],collapse=' -> ')
    paths.name[i]=paste(paths.name[i],paths[[i]][nrow(paths[[i]]),'Out'],sep =' -> ')
  }
  #paths.name=cbind(paths.id,paths.name)
  paths.name=data.frame(paths.id,paths.name)
  
  
  # get static igraph object
  Edges=Graph_convert_to_visNetwork_edges(Rgraph,paths,T)
  Nodes=Graph_convert_to_visNetwork_nodes(Rgraph,paths,T)
  
  
  paths.to.nodes.map=Paths_convert_to_paths_nodes_map(paths)
  paths.to.edges.map=Paths_convert_to_paths_edges_map(paths,Edges)
  
  
  ######## build inputs ###########################################################################
  
  updateCheckboxGroupInput(session, "SelectPath",
                           label="Select Paths",
                           choices=paths.name$paths.name,selected = paths.name$paths.name, inline=F)
                           
  #output$ui_SelectPaths <- renderUI({
  #  checkboxGroupInput("SelectPath","Select Paths",choices=paths.name$paths.name,selected = paths.name$paths.name, inline=F)
  #})
  
  #output$ui_SelectNode <- renderUI({
  # radioButtons("SelectNode","Select Node",choices=as.matrix(Nodes$label),selected = input$current_node_id[[1]], inline=F)
  #})  
  
  ######## read inputs ###########################################################################  
  observe({
    selection <- input$SelectPath
    #save(selection,file='dummy.Rdata')
    if (is.null(selection)){
      edges_selection=NA
    }else{
      selection1=merge(x=as.data.frame(x=selection),y=paths.name,by.x='selection',by.y='paths.name')
      selection2=merge(x=paths.to.edges.map,y=selection1,by.x='Path',by.y='paths.id')
      edges_selection=selection2$Edge.Id
    }
    
    #load(file='/home/hendl/Repos/RisksOfLife/scripts/visNetworkShiny/dummy.Rdata')
    visNetworkProxy("network") %>%
      visSelectEdges(id = edges_selection)
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
                 enabled=F)  %>% 
      #visOptions(selectedBy = list(variable = "paths", multiple = T, highlight = T),
      #           nodesIdSelection = list(enabled = T, useLabels = T))  %>% 
      visEdges(color = list(color = "grey",highlight ="blue"))
  })
  
  output$info <- renderText({
    #graph=visNetworkProxy("network")
    #paste0("visgetselectededges ", visGetSelectedEdges(graph, input = paste0(graph$id, "_selectedEdges")))
    
    paste0(visNetworkProxy("network")  %>% visGetSelectedEdges())
      
    #paste0("visgetselectededges ", visGetSelectedEdges(graph, input = paste0(graph$id, "_selectedEdges")))
    
  })
  
  # Add to your server 
  observeEvent(input$browser,{
    browser()
  })
  
})



#shinyApp(ui, server)

