
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
  
  
  # Debug Shiny
  #https://www.r-bloggers.com/2019/02/a-little-trick-for-debugging-shiny/
  # Add to your UI: 
  actionButton("browser", "browser"),
  #tags$script("$('#browser').hide();"),
  
  sidebarLayout(
    sidebarPanel(
      #checkboxGroupInput("SelectPath","Select Paths",choices=paths.name$paths.name,selected = paths.name$paths.name, inline=F)
      checkboxGroupInput("SelectPath","Select Paths", inline=F)
      
      #uiOutput("ui_SelectPaths"),
      
      #      textInput("PathFilter","Filter"),
      #uiInput("ui_SelectPaths")
      #uiOutput("ui_SelectNode")
      
    ),
    mainPanel(
      visNetworkOutput("network", height="800px")  ,
      br(),br(),
      verbatimTextOutput("info")
    )
  )  
))
