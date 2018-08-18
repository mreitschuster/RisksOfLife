#' Draw Rshiny Graph
#' 
#' This is to visualize the Risk of Life Graph in Rshiny
#' 
#' @param iGraph_object The Rgraph object (package specific object to save the graph)
#' @return nothing
#' @export 
#' 

library(shiny)



ui <- basicPage(
  
  plotOutput("plot1", click = "plot_click"),
  
  verbatimTextOutput("info")
  
)



server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    
    nodes <- data.frame(name=c("Risk", "Accident", "Death", "LoE","CoC","Loss"),
                        
                        color=c("yellow", "red", "red", "green","green","blue"),
                        
                        size=rep(40,6))
    
    edges <- data.frame(   bla=c("Risk",     "Risk",  "Accident", "Death",  "Accident", "LoE", "CoC"),
                           
                           
                           
                           blalba=c("Accident", "Death", "Death",    "LoE",    "CoC",      "Loss","Loss"),
                           
                           stuff=c("data_none", "data_none", "data_disability",    "data_death",    "data_disability",      "data_money","data_money")
                           
                           
                           
    )
    
    g <- graph_from_data_frame(edges, directed=TRUE, vertices=nodes)
    
    plot.igraph(g, canvas.width = 450, canvas.height = 450)
    
  })
  
  
  
  output$info <- renderText({
    
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
    
  })
  
}



shinyApp(ui, server)

