library(shiny)
library(dplyr)
library(ggplot2)
library(shinycssloaders)
library(jsonlite)
library(forcats)
library(plotly)
#data
streaming_history_df <- fromJSON("data/mikołaj/StreamingHistory0.json")
dfDates <- streaming_history_df%>% 
  mutate(endTime = as.Date(endTime)) %>% 
  mutate(
    endYear = strftime(endTime, "%Y"),
    endMonth = strftime(endTime, "%m"),
    endDay = strftime(endTime, "%d")
  )

#UI1

ui1 <- fluidPage(
  titlePanel("Visualization 1."),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "bins", 
        label = "Select number of bins: ", 
        min = 5, 
        max = 50, 
        value = 30
      )
    ),
    mainPanel(
      plotOutput("plot1")
    )
  )
)

#UI2

ui2 <- fluidPage(
  titlePanel("Visualization 2."),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "person", 
        label = "Select person: ", 
        choices = c("Krzysiek", "Mikołaj", "Daniel"), 
        selected = "Mikołaj"
      )
    ),
    mainPanel(
      plotOutput("plot2"),
      width = 9
    )
  )
)

#UI3

ui3 <- fluidPage(
  titlePanel("Visualization 3."),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "n", 
        label = "Select number of displayed artists: ", 
        min = 5, 
        max = 15, 
        value = 10
      )
    ),
    mainPanel(
      plotly::plotlyOutput("plot3"),
      width = 9
    )
  )
)

app_ui <- navbarPage(
  id = "id",
  title = "Prototype",
  tabPanel("Plot1", ui1),
  tabPanel("Plot2", ui2),
  tabPanel("Plot3", ui3),
  theme = bslib::bs_theme(bootswatch = 'cosmo')
)



shinyApp(app_ui,server)

