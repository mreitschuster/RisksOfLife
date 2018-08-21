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


ui <- basicPage(
  plotOutput("plot1", click = "plot_click"),
  verbatimTextOutput("info")
)



server <- function(input, output) {

  # Build the graph. 
  Rgraph=Graph_Build()


  # get all active routes between risk and loss
  paths<-Graph_getAllPaths(Rgraph, startNode= 'Risk', endNode='Monetary_Loss', onlyActive=TRUE)
  
  #  calculate risks for all given paths
  path_results=Graph_calculate_paths(paths)
  
  
  Edges=Graph_convert_to_igraph_edges(Rgraph,T)
  Edges=Edges[,c(1,3,2,4)]
  Nodes=Graph_convert_to_igraph_nodes(Rgraph,T)
  # modify the Rgraph object to make it look nice
  
  # convert to igraph object
  g <- graph_from_data_frame(Edges, directed=TRUE, vertices=Nodes)
  
  l <- layout_nicely(g)  
  l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
  
  output$plot1 <- renderPlot({
    
  #  nodes <- data.frame(name=c("Risk", "Accident", "Death", "LoE","CoC","Loss"),
  #                      color=c("yellow", "red", "red", "green","green","blue"),
  #                      size=rep(40,6))

    plot.igraph(g, layout=l,rescale=F, canvas.width = 450, canvas.height = 450)
  })
  
  
  
  output$info <- renderText({
    
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
    
  })
  
}



shinyApp(ui, server)

