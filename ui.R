library(shiny)
library(tidyverse)
library(shinydashboard)
library(maps)
library(plotly)
library(DT)

header <- dashboardHeader(title = "United States Coronavirus")


sidebar <- dashboardSidebar(
    sidebarMenu( id = "tabs",
    menuItem("United States (total)", 
             tabName = "usa", 
             icon = icon("th"),
             badgeLabel = "new", badgeColor = "green"),
    
    menuItem("Regional Map",
             tabName = "region",
             icon =  icon("th"),
             badgeLabel = "new",
             badgeColor = "yellow"),
    
    menuItem("Modeling",                                    #Modeling Tab
             tabName = "models",
             icon = icon("th"),
             badgeLabel = "new", badgeColor = "blue"),      #End of Modeling Tab
    
    conditionalPanel("input.tabs == 'region'",              #Placement of regional panel
                     selectInput("state",
                                 label = "Select State",
                                 choices = c("Alabama", "Arizona","Arkansas", "California", "Colorado",
                                             "Connecticut","Delaware", "Florida","Georgia","Idaho", 
                                             "Illinois", "Indiana","Iowa", "Kansas", "Kentucky", "Louisiana", 
                                             "Maine","Maryland", "Massachusetts","Michigan", "Minnesota","Mississippi", 
                                             "Missouri", "Montana","Nebraska", "Nevada", "New Hampshire","New Jersey",
                                             "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma",
                                             "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", 
                                             "Texas","Utah", "Vermont","Virginia", "Washington", "West Virginia",
                                             "Wisconsin","Wyoming"),
                                 selected = "Florida"
                     )
    ),
    conditionalPanel("input.tabs == 'usa' | input.tabs == 'region'",                 #Buttons for Total United States
                     radioButtons("deaths-cases", 
                                  label = h4("Plot:"),
                                  choices = list("Deaths" = "deaths", "Cases" = "cases"),
                                  selected = "cases"),
                     radioButtons("log-normal",
                                  label = h4("Color Scale"),
                                  choices = list("Logarithmic" = "log", "Linear" = "line"))
                     
                     ),                                      #End of Buttons for United States
    conditionalPanel("input.tabs == 'region'",
                     uiOutput("county_slider")
                     )
    )
)

body <- dashboardBody(
  tabItems(
  # Boxes need to be put in a row (or column)
    tabItem(tabName = "usa",                              #Layout for Total USA map
      fluidRow(
        box(width=6, 
            status="info", 
            title="United States Coronavirus Spread",
            solidHeader = TRUE,
            plotlyOutput("usaplot")
        ),
        box(width=6, 
            status="warning", 
            title = "Raw Data",
            solidHeader = TRUE, 
            collapsible = FALSE, 
            footer="Read Remotely from Github.com/nytimes/covid-19-data/",
            DTOutput("usadata")
        )
      )                                                     #End layout for USA map
      ## Add some more info boxes
      
    ),
    
    tabItem(tabName = "region",      #Regional plots and data
      fluidRow(
        box(width = 6, 
            status = "info",
            title = "Options",
            solidHeader = TRUE,
            collapsible = FALSE,
            plotlyOutput("county")
            )
        ),
      fluidRow(
        box(width = 6, 
            status = "warning",
            title = "Options",
            solidHeader = TRUE,
            collapsible = FALSE,
            h2("As of Right now, I cannot get the plot to dynamically update with the slider bar."),
            h4("It probably has something to do with the fact that I can't call the value from the dynamically rendered date slider"),
            h4("Notice that when you change the state, the date from first infection changes"),
            h4("However, I can't filter the data based on that value (and hence a slow load time).")
            #DTOutput("poopy")
            )
        )
      ),                              #End Of Regional Plots and Data
    tabItem(tabName = "models",       #Modeling Tab
            h2("modeling Outputs")
    )
  )
)


dashboardPage(header, sidebar, body)
