
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
 
  titlePanel("Risks of Life"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("SelectPath","Select Paths",choices=paths.name$paths.name,selected = paths.name$paths.name, inline=F)
      
      #uiOutput("ui_SelectPaths"),
      
      #      textInput("PathFilter","Filter"),
      #uiInput("ui_SelectPaths")
      #uiOutput("ui_SelectNode")
      
    ),
    mainPanel(
      visNetworkOutput("network", height="1000px")  ,
      br(),br(),
      verbatimTextOutput("info")
    )
  )  
))
