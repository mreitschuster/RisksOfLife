#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
require(shiny)
require(visNetwork)

server <- function(input, output) {
  output$vis <- renderVisNetwork({
    nodes <- data.frame(id = 1:15, label = paste("Label", 1:15),
                        group = sample(LETTERS[1:3], 15, replace = TRUE))
    
    edges <- data.frame(id = 1:15, from = trunc(runif(15)*(15-1))+1,
                        to = trunc(runif(15)*(15-1))+1)
    
    
    visNetwork(nodes, edges) %>% 
        visEdges(color = list(color = "white",highlight ="red",hover = "blue")) %>%
      visInteraction(hover = T)
  })
  
  observe({
    nodes_selection <- input$selnodes
    visNetworkProxy("vis") %>%
      visSelectNodes(id = nodes_selection)
  })
    
  observe({
    edges_selection <- input$seledges
    visNetworkProxy("vis") %>%
      visSelectEdges(id = edges_selection)
  })
  
}

ui <- fluidPage(
  fluidRow(
    column(
      width = 4,
      selectInput(inputId = "selnodes", label = "Nodes selection", choices = 1:15, multiple = TRUE),
      selectInput(inputId = "seledges", label = "Edges selection", choices = 1:15, multiple = TRUE)
    ),
    column(
      width = 8,
      visNetworkOutput("vis", height = "400px")
    )
  )
)

shinyApp(ui = ui, server = server)