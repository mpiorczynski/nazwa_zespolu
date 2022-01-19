library(shiny)
library(gt)
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
  
  output$plot3 <- plotly::renderPlotly({
    df1 <- streaming_history_df %>% 
      group_by(artistName) %>% 
      summarise(Time = sum(msPlayed)/60000) %>% 
      arrange(-Time) %>% 
      head(input$n) %>% 
      mutate(artistName = fct_reorder(artistName, Time))
    
    
    p <- ggplot(df1,aes(x = Time, y = artistName))+
      geom_col(fill = "#1ED760")+
      theme(panel.background = element_rect(fill = "#444444"),
            plot.background = element_rect(fill = "#444444"),
            text = element_text(color = "#FFFFFF"),
            axis.text = element_text(color = "#FFFFFF"),
            legend.text = element_text(colour = "#FFFFFF"),
            axis.ticks = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(size = 0.3, colour = "#888888"),
            axis.title.y = element_blank(),
            axis.title.x = element_text())+
      labs(title = "2021 - Favourite artists", x = "Minutes listened")
    
    plotly::ggplotly(p, source = "1")
  })
  
  output$plot4 <- plotly::renderPlotly({
    df1 <- streaming_history_df %>% 
      group_by(artistName) %>% 
      summarise(Time = sum(msPlayed)/60000) %>% 
      arrange(-Time) %>% 
      head(input$n) %>% 
      mutate(artistName = fct_reorder(artistName, Time))
    
    
    p <- ggplot(df1,aes(x = Time, y = artistName))+
      geom_col(fill = "#1ED760")+
      theme(panel.background = element_rect(fill = "#444444"),
            plot.background = element_rect(fill = "#444444"),
            text = element_text(color = "#FFFFFF"),
            axis.text = element_text(color = "#FFFFFF"),
            legend.text = element_text(colour = "#FFFFFF"),
            axis.ticks = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(size = 0.3, colour = "#888888"),
            axis.title.y = element_blank(),
            axis.title.x = element_text())+
      labs(title = "2021 - Favourite albums", x = "Minutes listened")
    
    plotly::ggplotly(p, source = "2")
  })
  
  observeEvent(event_data("plotly_click", source = "2"), {
    if(input$id=="Plot3"){
      batData = event_data("plotly_click", source = "2")
      showModal(modalDialog(title = "elo"))
    }
    
  })
  
  
  
  
  observeEvent(event_data("plotly_click", source = "1"), {
    if(input$id == "Plot3"){
      
      barData = event_data("plotly_click", source = "1")
      
      df1 <- streaming_history_df %>% 
        group_by(artistName) %>% 
        summarise(Time = sum(msPlayed)/60000) %>% 
        arrange(-Time) %>% 
        head(input$n) %>% 
        mutate(artistName = fct_reorder(artistName, Time))
      
      artist <- df1[barData$pointNumber+1,1] %>% 
        mutate(artistName = as.character(artistName))
      
      artist <- as.character(artist)
      
      favSong <- streaming_history_df %>% 
        filter(artistName == artist) %>% 
        group_by(trackName) %>% 
        summarise(Sum = sum(msPlayed)/60000) %>%
        arrange(-Sum) %>% 
        head(5)
      colnames(favSong) <- c("Track Name", "Minutes Listened")
      
      
      
      showModal(modalDialog(easyClose = TRUE, title = tags$a(style = "color: white", icon('robot'), br(), as.character(artist)), renderPlot({
        
        endMonth <- as.character(c("01","02","03","04","05","06","07","08","09","10","11","12"))
        MonthSum <- c(0,0,0,0,0,0,0,0,0,0,0,0)
        
        df1 <- data.frame(endMonth, MonthSum)
       
        
        
        df2 <- dfDates %>% 
          filter(artistName == artist) %>% 
          group_by(endMonth) %>% 
          summarise(MonthSum = sum(msPlayed)/60000)
        
        df3 <- rbind(df2,df1)
        
        ggplot(df3,aes(x = endMonth, y = MonthSum))+
          geom_col(fill = "#1ED760")+
          theme(panel.background = element_rect(fill = "#444444"),
                plot.background = element_rect(fill = "#444444"),
                text = element_text(color = "#FFFFFF"),
                axis.text = element_text(color = "#FFFFFF"),
                legend.text = element_text(colour = "#FFFFFF"),
                axis.ticks = element_blank(),
                panel.grid.minor = element_blank(),
                panel.grid.major = element_line(size = 0.3, colour = "#888888"),
                axis.title.y = element_blank(),
                axis.title.x = element_text())
      }),
      
      renderTable(barData),
      
      br(),
      render_gt({
        gt(favSong) %>% 
          tab_header(title = md("Favourite tracks")) %>% 
          tab_style(
            style = list(
              cell_text(color = 'white'),
              cell_fill(color = "#444444")
            ),
            locations = list(
              cells_body(),
              cells_title(),
              cells_column_labels()
              
            )
          )
        
        
      })
      
      ))
    }
  })
  
}

