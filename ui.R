library(shiny)
library(dplyr)
library(ggplot2)
library(shinycssloaders)
library(jsonlite)
library(fresh)
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
  use_theme(
    create_theme(
      theme = "default",
      bs_vars_navbar(
        default_bg = "#444444",
        default_color = "#FFFFFF",
        default_link_color = "#FFFFFF",
        default_link_active_color = "#75b8d1",
        default_link_active_bg = "#000000",
        default_link_hover_color = "firebrick"
      ),
      bs_vars_modal(
        md = "60%",
        backdrop_opacity = 0.7,
        header_border_color = "#112446",
        footer_border_color = "#112446",
        content_bg = "#444444"
      )
    )
  )
)



shinyApp(app_ui,server)

