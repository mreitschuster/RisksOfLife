#' Draw Rshiny Graph
#' 
#' This is to visualize the Risk of Life Graph in Rshiny
#' 
#' @param iGraph_object The Rgraph object (package specific object to save the graph)
#' @return nothing
#' @export 
#' 

library(shiny)
library(igraph)


ui <- fluidPage(
  titlePanel("Risks of Life"),
  sidebarLayout(
    sidebarPanel(
      textInput("PathFilter","Filter"),
      uiOutput("ui_SelectPaths"),
      uiOutput("ui_SelectNode")
      
    ),
    mainPanel(
      plotOutput("plot1", click = "plot_click"),
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
  names_of_paths=vector(length = length(paths))
  for (i in 1:length(paths)){
    names_of_paths[i]=paste(paths[[i]][,1],collapse=' -> ')
    names_of_paths[i]=paste(names_of_paths[i],paths[[i]][nrow(paths[[i]]),3],sep =' -> ')
  }

  # get static igraph object
  Edges=Graph_convert_to_igraph_edges(Rgraph,T)
  Edges=Edges[,c(1,3,2,4)]
  Nodes=Graph_convert_to_igraph_nodes(Rgraph,T)
  # modify the Rgraph object to make it look nice
  
  # convert to igraph object
  #g <- graph_from_data_frame(Edges, directed=TRUE, vertices=Nodes)
  g <- igraph::graph_from_data_frame(Edges, directed=TRUE)
  l <- igraph::layout_nicely(g)  
  l <- igraph::norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
  
  
######## build inputs ###########################################################################
  output$ui_SelectPaths <- renderUI({
    checkboxGroupInput("SelectPath","Select Paths",choices=names_of_paths,selected = names_of_paths, inline=F)
  })

  output$ui_SelectNode <- renderUI({
    radioButtons("SelectNode","Select Node",choices=as.matrix(Nodes),selected = "Monetary_Loss", inline=F)
  })  
  
######## output ###########################################################################  
  output$plot1 <- renderPlot({

    selected_edges=Edges
    selected_edges[,"color"]=rep('grey',nrow(selected_edges))
    selected_edges[,"width"]=rep(1,nrow(selected_edges))

    for (i in 1:nrow(selected_edges)){
      flag_found=F
      tmp_str=paste(selected_edges[i,1],selected_edges[i,2],sep =' -> ')
      if(any(grepl(tmp_str,input$SelectPath))){
        selected_edges[i,"color"]="red"
        selected_edges[i,"width"]=5
      }
    }
    g <- igraph::graph_from_data_frame(selected_edges, directed=TRUE)
    igraph::plot.igraph(g, layout=l,rescale=F, canvas.width = 450, canvas.height = 450,edge.arrow.size=2)
  })
  
  output$info <- renderText({
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
  })
  
}



shinyApp(ui, server)

