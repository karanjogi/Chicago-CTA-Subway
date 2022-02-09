library(shiny)
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

cta_stations = subset(cta_data, stationname == c("UIC-Halsted", "O'Hare Airport", "Rosemont"))

cta_stations <- cta_stations %>% 
  mutate(date = mdy(date))

#Create the app UI using shiny
ui <- fluidPage(
    selectInput(inputId = "station_1",
                label = "Select a Station:",
                choices = c("UIC Halsted" = "UIC-Halsted", "O'Hare" = "O'Hare Airport", "Rosemont" = "Rosemont"),
                selectize = FALSE),
    selectInput(inputId = "station1_timeframe",
                label = "Select a timeframe:",
                choices = c("Date", "Month", "Day"),
                selectize = FALSE),
   dataTableOutput("station_1_table"),
   plotOutput(outputId = "station1_date_plot",
              hover = TRUE)
)

server <- function(input, output, session) {
  output$station_1_table <- renderDataTable({
    subset(cta_data, stationname == input$station_1)
  }, options = list(pageLength = 5))
  
  uic_2021 <- subset(cta_stations, year(date) == 2021)
  
  output$station1_date_plot <- renderPlot({
    switch (input$station1_timeframe,
            "Date" = ggplot(uic_2021, aes(x= date, y= rides)) +
                             geom_bar(stat = "identity", fill="steelblue") +
                             theme_minimal() +
                             labs(title = "Entries per Date in UIC for year 2021", x = "Date", y = "Number of entries") +
                             scale_y_continuous(labels = comma),
            "Month" = ggplot(uic_2021, aes(x=month(date, label = TRUE), y=rides)) +
                             geom_bar(stat = "identity", fill="steelblue") +
                             theme_minimal() +
                             labs(title = "Entries per Month in UIC for year 2021", x = "Month", y = "Number of entries") +
                             scale_y_continuous(labels = comma)
                            ,
            "Day" = ggplot(uic_2021, aes(x=wday(date, label = TRUE), y=rides)) +
                               geom_bar(stat = "identity", fill="steelblue") +
                               theme_minimal() +
                               labs(title = "Entries per Day in UIC for year 2021", x = "Days", y = "Number of entries") +
                               scale_y_continuous(labels = comma)
    )
  }
  )
  
  
  
}

shinyApp(ui, server)