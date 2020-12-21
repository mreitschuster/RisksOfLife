#' Draw Rshiny Graph
#' 
#' This is to visualize the Risk of Life Graph in Rshiny
#' 
#' @param iGraph_object The Rgraph object (package specific object to save the graph)
#' @return nothing
#' @export 
#' 

require(shiny)
#require(igraph)
require(visNetwork)

ui <- fluidPage(
  titlePanel("Risks of Life"),
  sidebarLayout(
    sidebarPanel(
#      textInput("PathFilter","Filter"),
      uiOutput("ui_SelectPaths")#,
      #uiOutput("ui_SelectNode")
      
    ),
    mainPanel(
      visNetworkOutput("network", height="1000px")  ,
      br(),br(),
      verbatimTextOutput("info")
    )
  )  
)


#' Draw Rshiny Graph
#' 
#' This is to visualize the Risk of Life Graph in Rshiny
#' 
#' @param input input
#' @param output output
#' @return nothing
#' @export 
#' 
server <- function(input, output) {

  
#### prepare the data ##########################################################################
  # Build the graph. 
  Rgraph=Graph_Build()

  # get all active routes between risk and loss
  paths<-Graph_getAllPaths(Rgraph, startNode= 'Risk', endNode='Monetary_Loss', onlyActive=TRUE)
  
  # calculate risks for all given paths
  path_results=Graph_calculate_paths(paths)
  
  # get paths as strings
  paths.id=paste("Path",1:length(paths),sep = "")
  paths.name=vector(length = length(paths))
  for (i in 1:length(paths)){
    paths.name[i]=paste(paths[[i]][,1],collapse=' -> ')
    paths.name[i]=paste(paths.name[i],paths[[i]][nrow(paths[[i]]),3],sep =' -> ')
  }

  # get static igraph object
  Edges=Graph_convert_to_visNetwork_edges(Rgraph,T)
  Nodes=Graph_convert_to_visNetwork_nodes(Rgraph,T)
  
  #use the paths as group (not visgroup) names - add attribute paths to Nodes
  Nodes$paths=rep(NA,nrow(Nodes))
  for (i in 1:length(paths)){
    for (j in 1:nrow(Nodes)){
      if(Nodes$id[j] %in% unique(rbind(paths[[i]][,"In"],paths[[i]][,"Out"]))){
        if(is.na(Nodes$paths[j])){
          Nodes$paths[j]=paths.id[[i]]
        }else{
          Nodes$paths[j]=paste(Nodes$paths[j],",",paths.id[[i]])
        }
      }
    }
  }  

  #use the paths as group (not visgroup) names - add attribute paths to Edges
  Edges$paths=rep(NA,nrow(Edges))
  for (i in 1:length(paths)){
    for (j in 1:nrow(Edges)){
      if(any((paths[[i]][,"In"] %in%  Edges[j,"from"]) & (paths[[i]][,"Out"] %in%  Edges[j,"to"]))){
        if(is.na(Edges$paths[j])){
          Edges$paths[j]=paths.id[[i]]
        }else{
          Edges$paths[j]=paste(Edges$paths[j],",",paths.id[[i]])
        }
      }
    }
  }  
  
  
######## build inputs ###########################################################################
  output$ui_SelectPaths <- renderUI({
    checkboxGroupInput("SelectPath","Select Paths",choices=paths.name,selected = paths.name, inline=F)
  })

  #output$ui_SelectNode <- renderUI({
   # radioButtons("SelectNode","Select Node",choices=as.matrix(Nodes$label),selected = input$current_node_id[[1]], inline=F)
  #})  
  
######## output ###########################################################################  
  output$network <- renderVisNetwork({
    ###select by Node
    if (FALSE){
      visNetwork(Nodes, Edges) %>%
        visEdges(arrows='to',smooth=F)%>%
        #  visOptions(manipulation = TRUE)  %>%
        visPhysics(solver='barnesHut') %>% # 'barnesHut', 'repulsion', 'hierarchicalRepulsion', 'forceAtlas2Based'. 
        visPhysics(maxVelocity=50) %>% 
        visPhysics(minVelocity=0.1) %>% 
        visPhysics(timestep=0.5) %>% 
        visPhysics(stabilization=list(
          enabled = TRUE,
          onlyDynamicEdges = TRUE,
          fit = TRUE)) %>% 
        visPhysics(enabled=F)  %>% 
        visOptions(nodesIdSelection = TRUE)%>% 
        #visInteraction(hover = TRUE) %>%      # needed for input current_node_id
        #visEvents(hoverNode = "function(nodes) {Shiny.onInputChange('current_node_id', nodes);;}") # needed for input current_node_id
        visInteraction(selectable = TRUE) %>%      # needed for input current_node_id
        visEvents(selectNode = "function(nodes) {Shiny.onInputChange('current_node_id', nodes);;}") # needed for input current_node_id
    }
    
    ### select by path
    if (FALSE){
      visNetwork(Nodes, Edges) %>%
        visEdges(arrows='to',smooth=F)%>%
        visPhysics(solver='barnesHut') %>% # 'barnesHut', 'repulsion', 'hierarchicalRepulsion', 'forceAtlas2Based'. 
        visPhysics(maxVelocity=50) %>% 
        visPhysics(minVelocity=0.1) %>% 
        visPhysics(timestep=0.5) %>% 
        visPhysics(stabilization=list(
          enabled = TRUE,
          onlyDynamicEdges = TRUE,
          fit = TRUE)) %>% 
        visPhysics(enabled=F)  %>% 
        visOptions(selectedBy = list(variable = "paths", multiple = T)) #%>%
    }
    
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
      visOptions(selectedBy = list(variable = "paths", multiple = T, highlight = T),
                 nodesIdSelection = list(enabled = T, useLabels = T))   %>% 
      visInteraction(selectConnectedEdges=F)
    

    
    })
  
  output$info <- renderText({
    #paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
    paste0("Node is ",input$current_node_id)
  })
  
}



shinyApp(ui, server)

