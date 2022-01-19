library(jsonlite)
library(stringr)
library(dplyr)
library(ggplot2)
library(shiny)
library(shinycssloaders)
library(stringr)
library(shinyWidgets)
library(plotly)
library(shinyjs)
library(kit)
library(fresh)

# data
wdays <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
wdays2 <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
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
      df <- bind_rows(list(Daniel=df_daniel,
                      Mikołaj=df_mikolaj,
                      Krzysiek=df_krzysiek),
                      .id="person") %>%
        filter(ts > input$p2_time[1], ts < input$p2_time[2])
      
      p <- ggplot(df, aes(x=hour*60*60 + min*60 + s,
                          color=person)) +
        geom_density(aes(weight=ms_played,
                         text=paste("Person:", person, "\nClick to see details."))) +
        scale_y_continuous(name="") +
        scale_x_continuous(name = "Hour",
                           breaks = (0:8)*60*60*3,
                           labels = seq(from=0, to=24, by=3)) +
        scale_color_manual(values = c("#1ED760", "#00F5D2", "#23F500")) +
        theme(panel.background = element_rect(fill = "#444444"),
              plot.background = element_rect(fill = "#444444"),
              text = element_text(color = "#FFFFFF"),
              axis.text = element_text(color = "#FFFFFF"),
              legend.text = element_text(colour = "#FFFFFF"),
              legend.background = element_rect(fill="#444444", colour="#888888"),
              axis.ticks = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_line(size = 0.3, colour = "#888888"),
              axis.title.y = element_blank(),
              axis.title.x = element_text()) +
        labs(colour = "Person")
      
      ggplotly(p, source = "p2_comp", tooltip="text") %>%
        layout(yaxis = list(ticks="", showticklabels=FALSE)) %>% 
        config(displayModeBar=FALSE)
    }
    else{
      p2_person <- c("Daniel", "Krzysiek", "Mikołaj")[p2_ed$curveNumber + 1]
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
      df$platform <- factor(df$platform, levels = c("PC", "Phone", "Other"))
      
      p <- ggplot(df, aes(x=hour*60*60 + min*60 + s, y=..count../2000, color=platform)) +
        geom_density(aes(weight=ms_played,
                         text = paste("Device:", platform))) +
        scale_y_continuous(name="") +
        scale_x_continuous(name = "Hour",
                           breaks = (0:8)*60*60*3,
                           labels = seq(from=0, to=24, by=3)) +
        scale_color_manual(values = c("#1ED760", "#00F5D2", "#23F500")) +
        theme(panel.background = element_rect(fill = "#444444"),
              plot.background = element_rect(fill = "#444444"),
              text = element_text(color = "#FFFFFF"),
              axis.text = element_text(color = "#FFFFFF"),
              legend.text = element_text(colour = "#FFFFFF"),
              legend.background = element_rect(fill="#444444", colour="#888888"),
              axis.ticks = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_line(size = 0.3, colour = "#888888"),
              axis.title.y = element_blank(),
              axis.title.x = element_text()) +
        labs(colour = "Device")
      
      ggplotly(p, source = "p2_density", tooltip = "text") %>%
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
      p2_person <- c("Daniel", "Krzysiek", "Mikolaj")[p2_ed$curveNumber + 1]
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
      timeFormat = "%b %Y",
      width="100%"
    ) 
  })
  
  output$p2_heatmap <- renderPlotly({
    p2_ed <- event_data("plotly_click", source="p2_comp")
    p2_person <- c("Daniel", "Krzysiek", "Mikołaj")[p2_ed$curveNumber + 1]
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
    
    p <- ggplot(df_heatmap) + 
      geom_tile(aes(x=weekday,
                    y=hour,
                    fill=z,
                    text=paste("Day:", wdays[weekday],
                               "\nTime:", hours[hour],
                               "\nTotal listening time:",
                               strftime(
                                 as.POSIXlt.numeric(z/1000,
                                                    format="%OS",
                                                    origin="")-3600,
                                 format="%H:%M:%OS"))),
                color = "#444444", #powinny być przerwy między kafelkami
                lwd = 1) +         #ale plotly nie dziła :(((
      scale_x_continuous(breaks=1:7, labels=wdays2) +
      scale_y_reverse(breaks=0:23, labels=hours) +
      scale_fill_gradient(high = "#1ED760",
                          low = "black",
                          breaks = c(max(df_heatmap$z), min(df_heatmap$z)),
                          labels = c("More", "Less")) +
      theme(panel.background = element_rect(fill = "#444444"),
            plot.background = element_rect(fill = "#444444"),
            text = element_text(color = "#FFFFFF"),
            axis.text.x = element_text(color = "#FFFFFF"),
            axis.text.y = element_text(color = "#FFFFFF"),
            legend.text = element_text(colour = "#FFFFFF"),
            legend.background = element_rect(fill="#444444", colour="#888888"),
            axis.ticks = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            axis.title.x = element_text()) +
      #coord_fixed(ratio = 1) +
      labs(x = "Day of Week",
           y = "Hour",
           fill = "")
    
    ggplotly(p, source="p2_tile", tooltip="text") %>% 
      config(displayModeBar=FALSE)
  })
  
  output$p2_UI <- renderUI({
    
    p2_ed <- event_data("plotly_click", source="p2_comp")
    if(is.null(p2_ed)) { tagList(
      h4("What time of the day do we listen to music?"),
      plotlyOutput("p2_density"),
      uiOutput("p2_UI_time_input") 
    )}
    else {
      p2_person <- c("Daniel", "Krzysiek", "Mikołaj")[p2_ed$curveNumber + 1]
      tagList(
        fluidRow(
          column(6,
                 h4(paste("What time of the day does", p2_person, "listen to music?")),
                 plotlyOutput("p2_density")
          ),
          
          column(6,
                 h4(paste("What time of the week does", p2_person, "listen to music?")),
                 plotlyOutput("p2_heatmap")
          )
        ),
        
        fluidRow(
          column(2,
                 actionButton(inputId = "p2_reset",
                              label = "",
                              icon = icon("backward"),
                              style = "color: #444444; background-color: #1ED760; border-color: #888888")
          ),
          column(4,
                 uiOutput("p2_UI_time_input") 
          )
        ) 
      )
    }
  })
  
  observeEvent(input$p2_reset, {
    runjs("Shiny.setInputValue('plotly_click-p2_comp', null);")
  })
}

ui1 <- fluidPage()

ui2 <- fluidPage(
  useShinyjs(),
  
  titlePanel("Listening time analysis"),
    
  uiOutput("p2_UI")
)

ui3 <- fluidPage()  
  
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
