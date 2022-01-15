library(jsonlite)
library(stringr)
library(dplyr)
library(ggplot2)
library(shiny)
library(shinycssloaders)
library(stringr)

# data
wdays <- c("Niedziela", "Poniedziałek", "Wtorek", "Środa", "Czwartek", "Piątek", "Sobota")
hours <- sprintf("%02d:00-%02d:00", 0:23, 1:24)


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
  
  output$p2_density <- renderPlot({
    if(input$p2_person == "Daniel"){
      df <- df_daniel
    }
    else if(input$p2_person == "Mikołaj"){
      df <- df_mikolaj
    }
    else {
      df <- df_krzysiek
    }
    df <- df %>% filter(ts > input$p2_time[1], ts < input$p2_time[2])
    ggplot(df, aes(x=hour*60*60 + min*60 + s, y=..count../2000, color=platform)) +
      geom_density(aes(weight=ms_played), alpha=0.1) +
      scale_y_continuous(name = "", breaks=NULL) + 
      scale_x_continuous(name = "Hour", breaks = (0:24)*60*60, labels=0:24)
  })
  
  output$p2_time_input <- renderUI({
    if(input$p2_person == "Daniel"){
      df <- df_daniel
    }
    else if(input$p2_person == "Mikołaj"){
      df <- df_mikolaj
    }
    else {
      df <- df_krzysiek
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
}

ui1 <- fluidPage()

ui2 <- fluidPage(
  titlePanel("Visualization 2."),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "p2_person", 
        label = "Select person:", 
        choices = c("Daniel", "Mikołaj", "Krzysiek"),
        selected = "Krzysiek"
      ),
      uiOutput("p2_time_input")
    ),
    mainPanel(
      plotOutput("p2_density")
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
