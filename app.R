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

cta_uic <- cta_stations %>% filter(stationname == "UIC-Halsted") %>% select("date", "rides")
cta_ros <- cta_stations %>% filter(stationname == "Rosemont") %>% select("date", "rides")
cta_oha <- cta_stations %>% filter(stationname == "O'Hare Airport") %>% select("date", "rides")

cta_uic_2020 <- cta_uic %>% filter(year(date) == 2020)
cta_uic_2021 <- cta_uic %>% filter(year(date) == 2021)
cta_uic_2014 <- cta_oha %>% filter(year(date) == 2014)
cta_uic_2002 <- cta_oha %>% filter(year(date) == 2002)

cta_ros_2019 <-  cta_ros %>% filter(year(date) == 2019)
cta_ros_2001 <- cta_ros %>% filter(year(date) == 2001)
cta_ros_2006 <-  cta_ros %>% filter(year(date) == 2006)
cta_ros_2008 <-  cta_ros %>% filter(year(date) == 2008)

cta_oha_2019 <- cta_oha %>% filter(year(date) == 2019)
cta_oha_2017 <- cta_oha %>% filter(year(date) == 2017)
cta_oha_2014 <- cta_oha %>% filter(year(date) == 2014)


#Create the app UI using shiny
ui <- fluidPage(
  
  theme = shinytheme("readable"),
  
  navlistPanel(
    widths = c(3,9),
    header = h2("Chicago Subway"),
    
    fluidRow(),
    fluidRow(),
    fluidRow(),
    fluidRow(),
    fluidRow(),
    fluidRow(),
    fluidRow(),
    fluidRow(),
    fluidRow(),
    fluidRow(),
    fluidRow(),
    fluidRow(),
    fluidRow(),
    
    tabPanel(style = "padding-top:240px",
             "About",
             p("This app is build as part of, ", strong("Project 1"), " for the course ", strong("CS 424: Visualization and Visual Analytics"), "for the term Spring 2022 at UIC."),
             p("The dashboard below visualizes CTA Subway entries over the years 2001-2021."),
             p("The dataset used here is taken from ", strong("Chicago Data Portal"), " and can be downloaded from ", a("here", href = "https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f"), "."),
             p("It has around ",strong("1 Million"), " rows which contains number of entries for each CTA Subway station, for each day since 2001"),
             p(strong("Author:"), a("Karan Jogi", href = "https://karanjogi.github.io")),
             p(strong("Packages Used:"), "shiny, shinythemes, ggplot2, lubridate, dplyr, scales"),
             p(strong("Created Using:") ," Rstudio, R, Shiny")
    ),
    
    tabPanel("Visualization",
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
                         value = "2021-01-01")
             ),
             
             splitLayout(
               selectInput(inputId = "station1_timeframe",
                           label = "Select a timeframe:",
                           choices = c("Date", "Month", "Day"),
                           selectize = FALSE),
               
               selectInput(inputId = "station2_timeframe",
                           label = "Select a timeframe:",
                           choices = c("Date", "Month", "Day"),
                           selectize = FALSE)
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
             
      
    ),
    
    tabPanel(
      "Interesting Dates",
      plotOutput(outputId = "interesting_dates_plot",
                 hover = TRUE),
      selectInput(inputId = "interesting_dates",
                  label = "Select an interesting date:",
                  choices = c("Covid 19 hits" = 1,
                              "College going online due to covid" = 2,
                              "Begining of Fall Sem 2021" = 3,
                              "Rosemont surge due to ohare shutdown" = 4,
                              "July 3 Pantera Setlist concert" = 5,
                              "NLDS Series Cubs matchup" = 6,
                              "Train Collision on March 24" = 7,
                              "UIC Halsted reconstruction as part of Circle Interchange project" = 8,
                              "Wizard World Chicago Convention on August 4" = 9,
                              "Suspension of part of the Blue Line for Three Weeks" = 10
                              ),
                  selectize = FALSE)
      )
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
            
            "Month" = station1_year() %>% 
              group_by("month" = month(date, label = TRUE)) %>% 
              summarise("rides" = sum(rides)),
            
            "Day" = station1_year() %>%
              group_by("day" = wday(date, label = TRUE)) %>% 
              summarise("rides" = sum(rides))
    )
    
  }, options = list(pageLength = 3))
  
  
  output$station_2_table <- renderDataTable({
    # station_2 <- subset(cta_stations, stationname == input$station_2)
    switch (input$station2_timeframe,
            
            "Date" = station2_year() %>% 
              select(date, rides),
            
            "Month" = station2_year() %>% 
              group_by("month" = month(date, label = TRUE)) %>% 
              summarise("rides" = sum(rides)),
            
            "Day" = station2_year() %>%
              group_by("day" = wday(date, label = TRUE)) %>%
              summarise("rides" = sum(rides))
    )
    
  }, options = list(pageLength = 3))
  
  # " = 1,
  #                             "" = 2,
  #                             "" = 3,
  #                             "Rosemont surge due to ohare shutdown" = 4,
  #                             "" = 5,
  #                             "" = 6,
  #                             "" = 7,
  #                             "t" = 8,
  # 
  output$interesting_dates_plot <- renderPlot({
    switch (input$interesting_dates,
      "1" = ggplot(cta_uic,
                   aes(x=year(date),
                       y=rides)
                   ) +
        geom_bar(stat = "identity", fill = "steelblue") +
        theme_gray() +
        labs(title = "Covid 19 hits", x = "Date", y = "Number of entries") +
        scale_y_continuous(labels = comma) +
        theme(plot.title = element_text(face = "bold"))
            ,
      
      "2" = ggplot(cta_uic_2020,
                   aes(x=date,
                       y=rides)
                   ) +
        geom_bar(stat = "identity", fill = "steelblue") +
        theme_gray() +
        labs(title = "College going online due to covid", x = "Date", y = "Number of entries") +
        scale_y_continuous(labels = comma) +
        theme(plot.title = element_text(face = "bold"))
      ,
      
      "3" = ggplot(cta_uic_2021,
                   aes(x=date,
                       y=rides)
                   ) +
        geom_bar(stat = "identity", fill = "steelblue") +
        theme_gray() +
        labs(title = "Begining of Fall Sem 2021", x = "Date", y = "Number of entries") +
        scale_y_continuous(labels = comma) +
        theme(plot.title = element_text(face = "bold"))
      ,
      
      "4" = ggplot(cta_ros_2019,
                   aes(x=date,
                       y=rides)
                   ) +
        geom_bar(stat = "identity", fill = "steelblue") +
        theme_gray() +
        labs(title = "Rosemont surge due to ohare shutdown", x = "Date", y = "Number of entries") +
        scale_y_continuous(labels = comma) +
        theme(plot.title = element_text(face = "bold"))
      ,
      
      "5" = ggplot(cta_ros_2001,
                   aes(x=date,
                       y=rides)
                   ) +
        geom_bar(stat = "identity", fill = "steelblue") +
        theme_gray() +
        labs(title = "July 3 Pantera Setlist concert", x = "Date", y = "Number of entries") +
        scale_y_continuous(labels = comma) +
        theme(plot.title = element_text(face = "bold"))
      ,
      
      "6" = ggplot(cta_oha_2017,
                   aes(x=date,
                       y=rides)
                   ) +
        geom_bar(stat = "identity", fill = "steelblue") +
        theme_gray() +
        labs(title = "NLDS Series Cubs matchup", x = "Date", y = "Number of entries") +
        scale_y_continuous(labels = comma) +
        theme(plot.title = element_text(face = "bold"))
      ,
      
      "7" = ggplot(cta_oha_2014,
                   aes(x=date,
                       y=rides)
                   ) +
        geom_bar(stat = "identity", fill = "steelblue") +
        theme_gray() +
        labs(title = "Train Collision on March 24", x = "Date", y = "Number of entries") +
        scale_y_continuous(labels = comma) +
        theme(plot.title = element_text(face = "bold"))
      ,
      
      "8" = ggplot(cta_uic_2014,
                   aes(x=date,
                       y=rides)
                   ) +
        geom_bar(stat = "identity", fill = "steelblue") +
        theme_gray() +
        labs(title = "UIC Halsted reconstruction as part of Circle Interchange project", x = "Date", y = "Number of entries") +
        scale_y_continuous(labels = comma) +
        theme(plot.title = element_text(face = "bold"))
      ,
      
      "9" = ggplot(cta_ros_2006,
                   aes(x=date,
                       y=rides)
                   ) +
        geom_bar(stat = "identity", fill = "steelblue") +
        theme_gray() +
        labs(title = "Wizard World Chicago Convention on August 4", x = "Date", y = "Number of entries") +
        scale_y_continuous(labels = comma) +
        theme(plot.title = element_text(face = "bold"))
      ,
      
      "10" = ggplot(cta_ros_2008,
                    aes(x=date,
                        y=rides)
                    ) +
        geom_bar(stat = "identity", fill = "steelblue") +
        theme_gray() +
        labs(title = "Suspension of part of the Blue Line for Three Weeks", x = "Date", y = "Number of entries") +
        scale_y_continuous(labels = comma) +
        theme(plot.title = element_text(face = "bold"))
    )
  }
    
  )
  
}

shinyApp(ui, server)
