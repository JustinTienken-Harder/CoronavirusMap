library(shiny)
library(shinydashboard)
library(ggplot2)
library(readr)

shinyServer(function(input, output, session) {
  
  df <- reactiveFileReader(
          intervalMillis = 10000, 
          session = session,
          filePath = "https://raw.githubusercontent.com/bklingen/DataViz2020_Maps/master/CoronaCases.csv",
          readFunc = read_csv)
  
  output$mydata <-renderTable({
    corona <- df()
    corona.cases <- corona %>% count(County) %>% mutate(County = tolower(County))
    corona.cases <- corona.cases %>% mutate(County = fct_recode(County, 
                                                                `miami-dade` = "dade", 
                                                                `st johns` = "st. johns", 
                                                                `st lucie` = "st. lucie")) %>% 
      filter(County != "unknown") %>% arrange(desc(n), County)
  })
  
  output$myplot <- renderPlot({
    corona <- df()
    corona.cases <- corona %>% count(County) %>% mutate(County = tolower(County))
    corona.cases <- corona.cases %>% mutate(County = fct_recode(County, 
                                                                `miami-dade` = "dade", 
                                                                `st johns` = "st. johns", 
                                                                `st lucie` = "st. lucie")) %>% 
      filter(County != "unknown")
    covidFLmap <- left_join(floridamapdata, corona.cases, by = c(subregion = "County"))
    
    
    plot <- ggplot(covidFLmap, aes(x = long, y = lat, group = group, fill = n)) +
      geom_polygon(color = "black", 
                   size = 0.5) + 
      theme_minimal() + 
      scale_fill_viridis_c() + 
      labs(title = "Coronavirus Cases in Florida, by County",
           fill = "Number of Cases") + 
      coord_map(projection = "albers", 
                lat0 = 25, 
                lat1 = 31)
    return(plot)
  })
  

})
