library(shiny)
library(tidyverse)
library(shinydashboard)
library(maps)

dashboardPage(
  dashboardHeader(title = "My Dashboard"),
  dashboardSidebar(
    sliderInput("size", "Size of Points:", min=0.2, max=5, value=2)
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(width=6, 
          status="info", 
          title="Myplot",
          solidHeader = TRUE,
          plotOutput("myplot")
      ),
      box(width=6, 
          status="warning", 
          title = "Data Frame",
          solidHeader = TRUE, 
          collapsible = TRUE, 
          footer="Read Remotely from File",
          tableOutput("mydata")
      )
    ),
    ## Add some more info boxes

  )
)
