
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
require(visNetwork)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
 
  titlePanel("Risks of Life"),
  
  
  #### Debug Shiny ####
  #https://www.r-bloggers.com/2019/02/a-little-trick-for-debugging-shiny/
  # Add to your UI: 
  actionButton("browser", "browser"),
  #tags$script("$('#browser').hide();"),
  

  sidebarLayout(
    #### Sidebar #####
    sidebarPanel(
      #checkboxGroupInput("SelectPath","Select Paths", inline=F)
      checkboxGroupInput("SelectPath","Select Paths", inline=F),
      radioButtons("SeverityScale","Severity Scale (Edges)", 
                   choices = list("Same size for all" = 'sameSize', 
                                  "Linear" = 'linear',
                                  "Logarithmic" = 'log') ,selected = 'sameSize'),
      radioButtons("SeverityMeasure","Severity Measure (Edges)", 
                   choices = list("(Conditional) Mean" = 'mean', 
                                  "Probabilitiy" = 'prob'),selected = 'mean'),
      numericInput("Threshold","Threshold",value=0),
      checkboxInput("checkLabelEdges", "Edge Labels", value = FALSE),
      checkboxInput("checkLabelNodes", "Node Labels", value = TRUE)
      
    ),
    mainPanel(
      visNetworkOutput("network", height="800px")  ,
      br(),br(),
      verbatimTextOutput("info")
    )
  )  
))
