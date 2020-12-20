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
      textInput("PathFilter","Filter"),
      uiOutput("ui_SelectPaths"),
      uiOutput("ui_SelectNode")
      
    ),
    mainPanel(
      visNetworkOutput("network", height="1200px")  #,
      #br(),br(),
      #verbatimTextOutput("info")
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
  names_of_paths=vector(length = length(paths))
  for (i in 1:length(paths)){
    names_of_paths[i]=paste(paths[[i]][,1],collapse=' -> ')
    names_of_paths[i]=paste(names_of_paths[i],paths[[i]][nrow(paths[[i]]),3],sep =' -> ')
  }

  # get static igraph object
  Edges=Graph_convert_to_visNetwork_edges(Rgraph,T)
  Nodes=Graph_convert_to_visNetwork_nodes(Rgraph,T)

  
######## build inputs ###########################################################################
  output$ui_SelectPaths <- renderUI({
    checkboxGroupInput("SelectPath","Select Paths",choices=names_of_paths,selected = names_of_paths, inline=F)
  })

  output$ui_SelectNode <- renderUI({
    radioButtons("SelectNode","Select Node",choices=as.matrix(Nodes),selected = "Monetary_Loss", inline=F)
  })  
  
######## output ###########################################################################  
  output$network <- renderVisNetwork({

    #visNetwork(Nodes, Edges, width = "100%")
    visNetwork(Nodes, Edges) %>%
    visEdges(arrows='to',smooth=F)%>%
    visOptions(manipulation = TRUE)  %>%
    visPhysics(solver='barnesHut') %>% # 'barnesHut', 'repulsion', 'hierarchicalRepulsion', 'forceAtlas2Based'. 
    visPhysics(maxVelocity=50) %>% 
    visPhysics(minVelocity=0.1) %>% 
    visPhysics(timestep=0.5) %>% 
    visPhysics(stabilization=list(
      enabled = TRUE,
      onlyDynamicEdges = TRUE,
      fit = TRUE)) %>% 
    visPhysics(enabled=F)
 #   visOptions(nodesIdSelection = TRUE)
  })
  
  output$info <- renderText({
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
  })
  
}



shinyApp(ui, server)

