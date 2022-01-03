library(shiny)
library(dplyr)
library(ggplot2)
library(shinycssloaders)
library(jsonlite)
library(forcats)

#data
streaming_history_df <- fromJSON("data/my_spotify_data/StreamingHistory0.json")
#server

server <- function(input, output, session){
  
  output$plot1 <- renderPlot({
    ggplot(streaming_history_df) + 
      geom_histogram(aes(x = msPlayed / (1000 * 60)), bins = input$bins) + 
      scale_x_continuous(expand = c(0, 0), minor_breaks = 0:45) + 
      scale_y_continuous(expand = c(0, 0)) + 
      labs(
        title = "Distribution of time of listened tracks",
        x = "Track length [min]", 
        y = "Count"
      ) + 
      theme_bw() 
  })
  
  output$plot2 <- renderPlot({
    if (input$person == "Mikołaj"){
    p <- streaming_history_df %>% 
      mutate(endTime = as.Date(endTime)) %>% 
      mutate(
        endYear = strftime(endTime, "%Y"),
        endMonth = strftime(endTime, "%m"),
        endDay = strftime(endTime, "%d")
      ) %>% 
      group_by(endMonth) %>% 
      summarise(totalTime = sum(msPlayed) / (1000 * 60)) %>% 
      mutate(endMonth = factor(endMonth, labels = month.abb)) %>% 
      ggplot() + 
      geom_col(aes(endMonth, totalTime)) + 
      scale_y_continuous(expand = c(0, 0)) +
      labs(
        x = "Month", 
        y = "Total time [min]"
      ) +
      theme_bw()
    }
    else {
      p <- ggplot(data = iris) + 
        geom_point(aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + 
        theme_bw()
    }
    p
    
  })
  
  output$plot3 <- renderPlot({
    streaming_history_df %>% 
      group_by(artistName) %>% 
      summarise(count = n()) %>% 
      mutate(artistName = fct_reorder(artistName, count)) %>% 
      slice_max(count, n = input$top_n) %>% 
      ggplot() + 
      geom_col(aes(x = count, y = artistName)) + 
      theme_bw() + 
      theme(
        panel.grid.major.y = element_blank()
      ) + 
      labs(
        title = "Most popular artists",
        x = "Count", 
        y = "Artist"
      )
    
  })
  
}

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
        inputId = "top_n", 
        label = "Select number of displayed artists: ", 
        min = 5, 
        max = 15, 
        value = 10
      )
    ),
    mainPanel(
      plotOutput("plot3"),
      width = 9
    )
  )
)

app_ui <- navbarPage(
  title = "Prototype",
  tabPanel("Plot1", ui1),
  tabPanel("Plot2", ui2),
  tabPanel("Plot3", ui3),
  theme = bslib::bs_theme(bootswatch = 'cosmo')
)



shinyApp(app_ui,server)
