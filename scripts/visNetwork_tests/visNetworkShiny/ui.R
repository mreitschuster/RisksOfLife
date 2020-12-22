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