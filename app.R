library(tidyverse)
library(ggplot2)
library(shiny)
library(plyr)
library(purrr)
library(zoo)
library(timeSeries)
library(lubridate)
library(ggfortify)
library(leaflet)
setwd("C:/Users/virch/Desktop/Data Work/Refugee Data Analysis/Refugee Time Series")


world <- geojsonio::geojson_read("world.json",what = "sp")

#leaflet(world) %>% addTiles() %>% addPolygons()

all.files <- list.files(pattern = "*.csv")

for (i in seq_along(all.files)) {
    assign(all.files[i], map_df(all.files[i], read.csv, skip = 3))
}

#View(asylum_seekers_all_data.csv)
#View(asylum_seekers_monthly_all_data.csv)

detach(package:plyr)

asylum_seekers_monthly <- asylum_seekers_monthly_all_data.csv %>%
    mutate(Date = as.Date(paste(Month, Year, sep = " "), "%B %d %y")) %>%
    subset(Value != "*", select = -c(Year, Month)) %>%
    group_by(Origin, Date) %>%
    summarise(val = sum(as.numeric(Value), na.rm = T))




#time_s <- asylum_seekers_monthly %>% subset(Origin == "Afghanistan", select = c("val")) %>% ts(frequency = 12, start = c(1999,1), end = c(2018,1))

#autoplot(time_s)



# shiny app refugee time series
ui <- fluidPage(
    
    # App title ----
    headerPanel("Refugee Time Series Analysis by Nation"),
    
    # select country
    selectInput("country",
                "Choose Country",
                choices = unique(asylum_seekers_monthly$Origin)),
    
    # Plot
    plotOutput("plot"),
    
    # Map
    leafletOutput("map")
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
    output$plot <- renderPlot ({
        
       asylum_seekers_monthly %>%
            subset(Origin == input$country) %>%
            subset(select = "val") %>%
            ts(frequency = 12, start = c(1999,1), end = c(2018,1)) %>%
            autoplot()
        
        
    })
    
    output$map <- renderLeaflet({ 
        
       world %>%
            subset(world@data$geounit == input$country) %>%
            leaflet() %>%
            addTiles() %>%
            addPolygons(highlightOptions = highlightOptions(color = "white", weight = 2,
                                           bringToFront = TRUE), label= ~as.character(geounit))
                         
        
        
        })
    
    
}

shinyApp(ui, server)