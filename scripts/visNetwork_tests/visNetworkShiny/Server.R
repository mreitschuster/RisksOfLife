#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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
