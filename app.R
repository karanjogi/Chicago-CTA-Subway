library(shiny)
library(shinythemes)
library(ggplot2)
library(lubridate)
library(dplyr)
library(scales)

temp_df = list.files(pattern="*.tsv")
cta_temp <- lapply(temp_df, read.delim)
cta_data <- do.call(rbind, cta_temp)

#Checking if the data was read correctly or no
dim(cta_data)

#Check if O'Hare was read correctly or no
any(cta_data == "O'Hare Airport")

cta_stations = cta_data %>% filter(stationname %in% c("UIC-Halsted", "O'Hare Airport", "Rosemont"))

cta_stations <- cta_stations %>% 
  mutate(date = mdy(date))

#Create the app UI using shiny
ui <- fluidPage(
  
  theme = shinytheme("readable"),
  
  splitLayout(plotOutput(outputId = "station1_year_plot",
                         hover = TRUE),
              plotOutput(outputId = "station2_year_plot",
                         hover = TRUE)
              ),
  
  
  
  splitLayout(selectInput(inputId = "station_1",
                          label = "Select a Station:",
                          choices = c("UIC Halsted" = "UIC-Halsted", "O'Hare" = "O'Hare Airport", "Rosemont" = "Rosemont"),
                          selectize = FALSE),
              
              selectInput(inputId = "station_2",
                          label = "Select a Station:",
                          choices = c("O'Hare" = "O'Hare Airport", "UIC Halsted" = "UIC-Halsted", "Rosemont" = "Rosemont"),
                          selectize = FALSE)
              ),
   
  splitLayout(
    dateInput(inputId = "year_1",
              label = "Select a Year:",
              startview = "decade",
              format = "yyyy",
              min = "2000-01-01",
              max = "2021-12-31",
              value = "2021-01-01"),
    
    dateInput(inputId = "year_2",
              label = "Select a Year:",
              startview = "decade",
              format = "yyyy",
              min = "2000-01-01",
              max = "2021-12-31",
              value = "2021-01-01"),
  ),
   
  splitLayout(
    selectInput(inputId = "station1_timeframe",
                label = "Select a timeframe:",
                choices = c("Date", "Month", "Day"),
                selectize = FALSE),
    
    selectInput(inputId = "station2_timeframe",
                label = "Select a timeframe:",
                choices = c("Date", "Month", "Day"),
                selectize = FALSE),
  ),
  
  splitLayout(
    plotOutput(outputId = "station1_date_plot",
               hover = TRUE),
    
    plotOutput(outputId = "station2_date_plot",
               hover = TRUE)
  ),
  
  
  splitLayout(
    dataTableOutput("station_1_table"),
    dataTableOutput("station_2_table")
  )
)

server <- function(input, output, session) {
  
  station1 <- reactive({
    cta_stations %>% filter(stationname == input$station_1)
    })
  station2 <- reactive({
    cta_stations %>% filter(stationname == input$station_2)
    })
  
  station1_year <- reactive({
    cta_stations %>% filter(stationname == input$station_1, year(date) == year(input$year_1))
  })
  
  station2_year <- reactive({
    cta_stations %>% filter(stationname == input$station_2, year(date) == year(input$year_2))
  })
  
  output$station1_year_plot <- renderPlot({
  #  uic <- cta_stations %>% filter(stationname == "UIC-Halsted")
    
    ggplot(station1(), aes(x=year(date), y=rides))  +
      geom_bar(stat = "identity", fill="steelblue") +
      theme_gray() +
      labs(title = "Yearly Entries", x = "Year", y = "Number of entries") +
      scale_y_continuous(labels = comma)
    
  })
  
  output$station2_year_plot <- renderPlot({
  #  uic <- cta_stations %>% filter(stationname == "UIC-Halsted")
    
    ggplot(station2(), aes(x=year(date), y=rides))  +
      geom_bar(stat = "identity", fill="steelblue") +
      theme_gray() +
      labs(title = "Yearly Entries", x = "Year", y = "Number of entries") +
      scale_y_continuous(labels = comma)
  })
  
  
  
  output$station1_date_plot <- renderPlot({
    # station_1 <- cta_stations %>% filter(stationname == input$station_1, year(date) == year(input$year_1))
    switch (input$station1_timeframe,
            
            "Date" = ggplot(station1_year(), aes(x= date, y= rides)) +
                             geom_bar(stat = "identity", fill="steelblue") +
                             theme_gray() +
                             labs(title = "Daily Entries", x = "Date", y = "Number of entries") +
                             scale_y_continuous(labels = comma),
            
            "Month" = ggplot(station1_year(), aes(x=month(date, label = TRUE), y=rides)) +
                             geom_bar(stat = "identity", fill="steelblue") +
                             theme_gray() +
                             labs(title = "Monthly Entries", x = "Month", y = "Number of entries") +
                             scale_y_continuous(labels = comma),
            
            "Day" = ggplot(station1_year(), aes(x=wday(date, label = TRUE), y=rides)) +
                               geom_bar(stat = "identity", fill="steelblue") +
                               theme_gray() +
                               labs(title = "Entries for days", x = "Days", y = "Number of entries") +
                               scale_y_continuous(labels = comma)
    )
  }
  )
  
  output$station2_date_plot <- renderPlot({
    # station_2 <- cta_stations %>% filter(stationname == input$station_2, year(date) == year(input$year_2))
    switch (input$station2_timeframe,
            
            "Date" = ggplot(station2_year(), aes(x= date, y= rides)) +
              geom_bar(stat = "identity", fill="steelblue") +
              theme_gray() +
              labs(title = "Daily Entries", x = "Date", y = "Number of entries") +
              scale_y_continuous(labels = comma),
            
            "Month" = ggplot(station2_year(), aes(x=month(date, label = TRUE), y=rides)) +
              geom_bar(stat = "identity", fill="steelblue") +
              theme_gray() +
              labs(title = "Monthly Entries", x = "Month", y = "Number of entries") +
              scale_y_continuous(labels = comma),
            
            "Day" = ggplot(station2_year(), aes(x=wday(date, label = TRUE), y=rides)) +
              geom_bar(stat = "identity", fill="steelblue") +
              theme_gray() +
              labs(title = "Entries for days", x = "Days", y = "Number of entries") +
              scale_y_continuous(labels = comma)
    )
  }
  )
  
  output$station_1_table <- renderDataTable({
    # station_1 <- subset(cta_stations, stationname == input$station_1)
    switch (input$station1_timeframe,
            
            "Date" = station1_year() %>% 
              select(date, rides),
            
            "Month" = station2_year() %>% 
              group_by(month(date, label = TRUE)) %>% 
              summarise(sum(rides)),
            
            "Day" = station2_year() %>%
              group_by(wday(date, label = TRUE)) %>% 
              summarise(entries = sum(rides))
    )
    
  }, options = list(pageLength = 3))
  
  
  output$station_2_table <- renderDataTable({
    # station_2 <- subset(cta_stations, stationname == input$station_2)
    switch (input$station2_timeframe,
            
            "Date" = station2_year() %>% 
              select(date, rides),
            
            "Month" = station2_year() %>% 
              group_by(month(date, label = TRUE)) %>% 
              summarise(sum(rides)),
            
            "Day" = station2_year() %>%
              group_by(wday(date, label = TRUE)) %>%
              summarise(entries = sum(rides))
    )
    
  }, options = list(pageLength = 3))
  
}

shinyApp(ui, server)
