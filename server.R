library(shiny)
library(shinydashboard)
library(ggplot2)
library(readr)
library(tidyverse)
library(maps)
#install.packages("mapproj")
library(mapproj)
library(plotly)
library(DT)

shinyServer(function(input, output, session) {
  #output$menu <- renderMenu({
    
  #})
  
  
  county_data <- reactiveFileReader(
    intervalMillis = 10000,
    session = session,
    filePath = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv",
    readFunc = read_csv
  )
  
  
  state_data <- reactiveFileReader(
          intervalMillis = 10000, 
          session = session,
          filePath = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv",
          readFunc = read_csv)
  
  output$usadata <-renderDT({
    
    state_data() %>% 
      arrange(desc(cases), state) %>% 
      mutate(cases = as.integer(cases), deaths) })
  
  output$usaplot <- renderPlotly({                              #Beginning of Plotly Renderings For United States.
    statedata <- state_data()
    statedata <- statedata %>% mutate(state_alt = setNames(state.abb,state.name)[state], 
                                      date = as.numeric(as.Date(date))-18282,
                                      #cases = ifelse(state == "New York"| state == "NY" | state == "NJ", 0, cases),
                                      log_cases = ifelse(cases == 0,0, log(cases)),
                                      log_deaths = ifelse(deaths == 0, 0 , log(deaths))
                                      )
    #statedata$hover <- with(statedata, paste(state, "<br>", "Total Cases:", cases, "<br>", "Logarithm Scale:", round(log_cases, digits = 3), "<br>", "Deaths:", deaths))
    
    l <- list(color = toRGB("white"), width = 2)
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    
    #fig <- plot_geo(statedata, locationmode = "USA-states")
    if(input$`deaths-cases` == "deaths"){
      if(input$`log-normal` == "log"){
        statedata$hover <- with(statedata, 
                                paste(state, "<br>", 
                                      "Total Cases:", cases, "<br>", 
                                      "Logarithm Scale:", round(log_cases, digits = 3), 
                                      "<br>", "Total Deaths:", deaths))
        fig <- plot_geo(statedata, locationmode = "USA-states")
        fig <- fig %>% add_trace(
              z = ~log_deaths, text = ~hover, locations = ~state_alt, color = ~log_deaths, frame = ~date
              ) %>% layout(geo = g)
      } else { #deaths, normal representation
          statedata$hover <- with(statedata, 
                                paste(state, "<br>", 
                                      "Total Cases:", cases, "<br>", 
                                      #"Logarithm Scale:", round(log_cases, digits = 3), 
                                      "<br>", "Total Deaths:", deaths))
          fig <- plot_geo(statedata, locationmode = "USA-states")
          fig <- fig %>% add_trace(
            z = ~deaths, text = ~hover, locations = ~state_alt, color = ~deaths, frame = ~date
          ) %>% layout(geo = g)
      }
    } else { # Display cases
      if(input$`log-normal` == "log"){
        statedata$hover <- with(statedata, 
                                paste(state, "<br>", 
                                      "Total Cases:", cases, "<br>", 
                                      "Logarithm Scale:", round(log_cases, digits = 3), 
                                      "<br>", "Total Deaths:", deaths))
        fig <- plot_geo(statedata, locationmode = "USA-states")
        fig <- fig %>% add_trace(
          z = ~log_cases, text = ~hover, locations = ~state_alt, color = ~log_cases, frame = ~date
        ) %>% layout(geo = g)
      } else { # Display cases without a logarithmic scale
        statedata$hover <- with(statedata, 
                                paste(state, "<br>", 
                                      "Total Cases:", cases, "<br>", 
                                      #"Logarithm Scale:", round(log_cases, digits = 3), 
                                      "<br>", "Total Deaths:", deaths))
        fig <- plot_geo(statedata, locationmode = "USA-states")
        fig <- fig %>% add_trace(
          z = ~cases, text = ~hover, locations = ~state_alt, color = ~cases, frame = ~date
        ) %>% layout(geo = g)
      }
    }
    
    return(fig)
  })                                      #End of plotly Graphs for Total United STates
  
  output$county_slider <- renderUI({
    data <- county_data() %>% filter(state == input$state) %>% mutate(date = as.Date(date))
    smallest_date <- min(data$date)
    #data <- data %>% mutate(date = date)
    object <- sliderInput("date",
                          label = "Select Days from Zero-Day",
                          min = min(data$date, na.rm = TRUE),
                          max = max(data$date, na.rm = TRUE),
                          value = max(data$date, na.rm = TRUE),
                          animate = TRUE)
  })
  
  
  output$county <- renderPlotly({
    corona <- county_data()
    `%nin%` = Negate(`%in%`)
    corona <- corona %>% #filter(state == input$state) %>% 
      mutate(county = tolower(county), 
             state = tolower(state),
             ) %>% 
      filter(state %nin% setdiff(unique(tolower(corona$state)), unique(map_data("county")$region)) )
    corona <- corona %>% filter(date == "2020-04-09")
    corona.cases <- corona %>% mutate(county = fct_recode(county, 
                                                        `de kalb` = "dekalb",
                                                        `dona ana` = "doña ana",
                                                        `du page` = "dupage",
                                                        `la porte` = "laporte",
                                                        `la salle` = "lasalle",
                                                        `new york` = "new york city",
                                                        `obrien` = "o'brien",
                                                        `prince georges` = "prince george's",
                                                        `queen annes` = "queen anne's",
                                                        `st bernard` = "st. bernard",
                                                        `st charles` = "st. charles",
                                                        `st clair` = "st. clair",
                                                        `st francis` ="st. francis",
                                                        `st francois` = "st. francois",
                                                        `st helena` = "st. helena",
                                                        `st james` = "st. james",
                                                        `st john the baptist` = "st. john the baptist",
                                                        `st johns` = "st. johns",
                                                        `st joseph` = "st. joseph",
                                                        `st landry` = "st. landry",
                                                        `st lawrence` = "st. lawrence",
                                                        `st louis` =  "st. louis",
                                                        `st louis city` = "st. louis city",
                                                        `st lucie` = "st. lucie",
                                                        `st martin` = "st. martin",
                                                        `st mary` = "st. mary",
                                                        `st marys` = "st. mary's",
                                                        `st tammany` = "st. tammany",
                                                        `ste genevieve` = "ste. genevieve"),
                                      log_cases = log(cases),
                                      log_deaths = log(deaths)) %>% 
      filter(county != "unknown" & state == tolower(input$state)) #%>% filter(date == Sys.Date()-1)
    print(str(corona.cases$date))
    #changing state mapping
    mapping <- map_data("county") %>% filter(region == tolower(input$state))
    
    mapitup <- left_join(mapping,corona.cases, by = c(subregion = "county") )
    
    if(input$`deaths-cases` == "deaths"){
      if(input$`log-normal` == "log"){
        thingy <- ggplot(mapitup, aes(x = long,
                                      y = lat, 
                                      group = group,
                                      fill = log_deaths, 
                                      text = paste0(subregion, "<br>", "Cases: ", cases, "<br>", "Deaths: ", deaths))) +
          geom_polygon(color = "black", 
                       size = 0.5) + 
          theme_minimal() + 
          scale_fill_viridis_c() + 
          labs(title = paste("Coronavirus", input$`deaths-cases`, "in" ,input$state, "by County with", input$`log-normal`, "scale"),
               fill = paste("Number of", input$`deaths-cases`)) + 
          coord_map(projection = "albers", 
                    lat0 = 25, 
                    lat1 = 31)
      }else{
        thingy <- ggplot(mapitup, aes(x = long,
                                      y = lat, 
                                      group = group,
                                      fill = deaths, 
                                      text = paste0(subregion, "<br>", "Cases: ", cases, "<br>", "Deaths: ", deaths))) +
          geom_polygon(color = "black", 
                       size = 0.5) + 
          theme_minimal() + 
          scale_fill_viridis_c() + 
          labs(title = paste("Coronavirus", input$`deaths-cases`, "in" ,input$state, "by County with", input$`log-normal`, "scale"),
               fill = paste("Number of", input$`deaths-cases`)) + 
          coord_map(projection = "albers", 
                    lat0 = 25, 
                    lat1 = 31)
      }
    }else{
      if(input$`log-normal` == "log"){
        thingy <- ggplot(mapitup, aes(x = long,
                                      y = lat, 
                                      group = group,
                                      fill = log_cases, 
                                      text = paste0(subregion, "<br>", "Cases: ", cases, "<br>", "Deaths: ", deaths))) +
          geom_polygon(color = "black", 
                       size = 0.5) + 
          theme_minimal() + 
          scale_fill_viridis_c() + 
          labs(title = paste("Coronavirus", input$`deaths-cases`, "in" ,input$state, "by County with", input$`log-normal`, "scale"),
               fill = paste("Number of", input$`deaths-cases`)) + 
          coord_map(projection = "albers", 
                    lat0 = 25, 
                    lat1 = 31)
      } else {
        thingy <- ggplot(mapitup, aes(x = long,
                                      y = lat, 
                                      group = group,
                                      fill = cases, 
                                      text = paste0(subregion, "<br>", "Cases: ", cases, "<br>", "Deaths: ", deaths))) +
          geom_polygon(color = "black", 
                       size = 0.5) + 
          theme_minimal() + 
          scale_fill_viridis_c() + 
          labs(title = paste("Coronavirus", input$`deaths-cases`, "in" ,input$state, "by County with", input$`log-normal`, "scale"),
               fill = paste("Number of", input$`deaths-cases`)) + 
          coord_map(projection = "albers", 
                    lat0 = 25, 
                    lat1 = 31)
      }
    }
    return(ggplotly(thingy, tooltip = "text"))
  })
  

})
