library(jsonlite)
library(stringr)
library(dplyr)
library(ggplot2)
library(shiny)
library(shinycssloaders)
library(stringr)
library(shinyWidgets)
library(plotly)

# data
wdays <- c("Niedziela", "Poniedziałek", "Wtorek", "Środa", "Czwartek", "Piątek", "Sobota")
hours <- sprintf("%02d:00-%02d:00", 0:23, 1:24)
p2_ed <- NULL

df_mikolaj <- fromJSON("data/mikołaj/endsong.json") %>% 
  select(ts,
         platform,
         ms_played,
         master_metadata_track_name,
         master_metadata_album_artist_name,
         master_metadata_album_album_name,
         shuffle) %>% 
  mutate(ts = as.POSIXlt(str_replace_all(ts, regex("[TZ]"), " "), tz="UTC")) %>% 
  mutate(weekday = ts$wday + 1,
         hour = ts$hour,
         min = ts$min,
         s = ts$s) %>% 
  mutate(platform = case_when(
    platform %in% c("iOS 12.1.2 (iPhone8,4)",
                    "iOS 14.7 (iPhone8,4)") ~ "Phone",
    platform %in% c("Windows 10 (10.0.19043; x64)",
                    "Windows 8.1 (6.3.9600; x64)") ~ "PC",
    TRUE ~ "Other"
  ))
  
df_krzysiek <- fromJSON("data/krzysiek/endsong.json") %>% 
  select(ts,
         platform,
         ms_played,
         master_metadata_track_name,
         master_metadata_album_artist_name,
         master_metadata_album_album_name,
         shuffle) %>% 
  mutate(ts = as.POSIXlt(str_replace_all(ts, regex("[TZ]"), " "), tz="UTC")) %>% 
  mutate(weekday = ts$wday + 1,
         hour = ts$hour,
         min = ts$min,
         s = ts$s) %>% 
  mutate(platform = case_when(
    platform %in% c("Android OS 9 API 28 (samsung, SM-J530F)",
                    "iOS 14.7.1 (iPhone13,1)",
                    "iOS 14.8 (iPhone13,1)",
                    "iOS 15.1 (iPhone13,1)",
                    "iOS 15.1.1 (iPhone13,1)") ~ "Phone",
    platform %in% c("Windows 10 (10.0.19041; x64; AppX)",
                    "Windows 10 (10.0.19043; x64; AppX)") ~ "PC",
    TRUE ~ "Other"
  ))
  
# tymczasowe rozwiązanie dopuki nie ma rozszerzonych danych
df_daniel <- bind_rows(df_mikolaj, df_krzysiek)
  
  
# ------------------------------------------------------------------------------

server <- function(input, output, session){
  
  output$p2_density <- renderPlotly({
    p2_ed <- event_data("plotly_click", source="p2_comp")
    if(is.null(p2_ed)){
      df <- bind_rows(list(df_daniel,
                      df_mikolaj,
                      df_krzysiek),
                      .id="person") %>%
        filter(ts > input$p2_time[1], ts < input$p2_time[2])
      
      p <- ggplot(df, aes(x=hour*60*60 + min*60 + s, color=person)) +
        geom_density(aes(weight=ms_played)) +
        scale_y_continuous(name="", breaks=c()) +
        scale_x_continuous(name = "Hour", breaks = (0:24)*60*60, labels=0:24)
      
      ggplotly(p, source = "p2_comp") %>%
        layout(yaxis = list(ticks="", showticklabels=FALSE)) %>% 
        config(displayModeBar=FALSE)
    }
    else{
      p2_person <- c("daniel", "krzysiek", "mikolaj")[p2_ed$curveNumber + 1]
      if(p2_person == "daniel"){
        df <- df_daniel
      }
      else if(p2_person == "mikolaj"){
        df <- df_mikolaj
      }
      else {
        df <- df_krzysiek
      }
      df <- df %>% filter(ts > input$p2_time[1], ts < input$p2_time[2])
      
      p <- ggplot(df, aes(x=hour*60*60 + min*60 + s, y=..count../2000, color=platform)) +
        geom_density(aes(weight=ms_played)) +
        scale_y_continuous(name="", breaks=c()) +
        scale_x_continuous(name = "Hour", breaks = (0:24)*60*60, labels=0:24)
      
      ggplotly(p, source = "p2_density") %>%
        layout(yaxis = list(ticks="", showticklabels=FALSE)) %>% 
        config(displayModeBar=FALSE)
    }
  })
  
  output$p2_UI_time_input <- renderUI({
    p2_ed <- event_data("plotly_click", source="p2_comp")
    if(is.null(p2_ed)){
      df <- bind_rows(list(df_daniel,
                           df_mikolaj,
                           df_krzysiek),
                      .id="person")
    }
    else{
      p2_person <- c("daniel", "krzysiek", "mikolaj")[p2_ed$curveNumber + 1]
      if(p2_person == "Daniel"){
        df <- df_daniel
      }
      else if(p2_person == "Mikołaj"){
        df <- df_mikolaj
      }
      else {
        df <- df_krzysiek
      }
    }
    min_time <- min(df$ts)
    max_time <- max(df$ts)
    
    sliderInput(
      inputId = "p2_time",
      label = "Select time period:",
      min = min_time,
      max = max_time,
      value = c(min_time,
                max_time),
      timeFormat = "%b %Y"
    )
  })
  
  output$p2_heatmap <- renderPlot({
    p2_ed <- event_data("plotly_click", source="p2_comp")
    p2_person <- c("daniel", "krzysiek", "mikolaj")[p2_ed$curveNumber + 1]
    if(p2_person == "Daniel"){
      df <- df_daniel
    }
    else if(p2_person == "Mikołaj"){
      df <- df_mikolaj
    }
    else {
      df <- df_krzysiek
    }
    df <- df %>% filter(ts > input$p2_time[1], ts < input$p2_time[2])
    
    df_heatmap <- expand.grid(weekday = 1:7, hour = 0:23)
    df_heatmap <- df_heatmap %>% 
      merge(
        df %>%
          group_by(weekday, hour) %>% 
          summarise(z = sum(ms_played)),
        by = c("weekday", "hour"),
        all.x=TRUE
      ) %>% 
      mutate(z = ifelse(is.na(z), 0, z))
    
    ggplot(df_heatmap) + 
      geom_tile(aes(x=weekday, y=hour, fill=z)) +
      scale_x_continuous(breaks=1:7, labels=wdays) +
      scale_y_reverse(breaks=0:23, labels=hours) +
      theme(
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      ) +
      scale_fill_gradient(high = "green", low = "black") +
      coord_fixed(ratio = 2/5)
  })
  
  output$p2_UI_heatmap <- renderUI({
    p2_ed <- event_data("plotly_click", source="p2_comp")
    if(is.null(p2_ed)){
      p("Click on the graph to choose a person and see their heatmap.")
    }
    else{
      plotOutput("p2_heatmap")
    }
  })
}

ui1 <- fluidPage()

ui2 <- fluidPage(
  titlePanel("Visualization 2."),
    
  fluidRow(
    column(8,
           h4("O której godzinie słuchamy muzyki?"),
           plotlyOutput("p2_density")
    ),
    
    column(4,
           h4("Kiedy w tygodniu słuchamy muzyki?"),
           uiOutput("p2_UI_heatmap")
    )
  ),
  
  fluidRow(
    column(3,
           actionButton(inputId = "p2_reset",
                        label = "",
                        icon = icon("backward"))
    ),
    column(5,
           uiOutput("p2_UI_time_input") 
    )
  )
)

ui3 <- fluidPage()  
  
app_ui <- navbarPage(
  id = "id",
  title = "Prototype",
  tabPanel("Plot1", ui1),
  tabPanel("Plot2", ui2),
  tabPanel("Plot3", ui3),
  theme = bslib::bs_theme(bootswatch = 'cosmo')
)

shinyApp(app_ui,server)
