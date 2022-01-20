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
    
    if(input$personPlot3 == "Mikołaj"){
      df <- df_mikolaj
    }
    else if(input$personPlot3 == "Krzysiek"){
      df <- df_krzysiek
    }
    else{
      df <- df_daniel
    }
    
    
    
    df1 <- df %>% 
      group_by(master_metadata_album_artist_name) %>% 
      summarise(Time = sum(ms_played)/60000) %>% 
      arrange(-Time) %>% 
      head(input$n) %>% 
      mutate(master_metadata_album_artist_name = fct_reorder(master_metadata_album_artist_name, Time))
    
    
    p <- ggplot(df1,aes(x = Time, y = master_metadata_album_artist_name))+
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
      labs(title = "Favourite Artists", x = "Minutes listened")
    
    plotly::ggplotly(p, source = "1")
  })
  
  output$plot4 <- plotly::renderPlotly({
    
    
    if(input$personPlot3 == "Mikołaj"){
      df <- df_mikolaj
    }
    else if(input$personPlot3 == "Krzysiek"){
      df <- df_krzysiek
    }
    else {
      df <- df_daniel
    }
    
    
    
    df1 <- df %>% 
      group_by(master_metadata_album_album_name) %>% 
      summarise(Time = sum(ms_played)/60000) %>% 
      arrange(-Time) %>% 
      head(input$n) %>% 
      mutate(master_metadata_album_album_name = fct_reorder(master_metadata_album_album_name, Time))
    
    
    p <- ggplot(df1,aes(x = Time, y = master_metadata_album_album_name))+
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
      labs(title = "Favourite albums", x = "Minutes listened")
    
    plotly::ggplotly(p, source = "2")
  })
  
  output$plot5 <- plotly::renderPlotly({
    
    if(input$personPlot3 == "Mikołaj"){
      df <- df_mikolaj
    }
    else if(input$personPlot3 == "Krzysiek"){
      df <- df_krzysiek
    }
    else {
      df <- df_daniel
    }
    
    
    df1 <- df %>% 
      group_by(master_metadata_track_name) %>% 
      summarise(Time = sum(ms_played)/60000) %>% 
      arrange(-Time) %>% 
      head(input$n) %>% 
      mutate(master_metadata_album_album_name = fct_reorder(master_metadata_track_name, Time))
    
    
    p <- ggplot(df1,aes(x = Time, y = master_metadata_track_name))+
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
      labs(title = "Favourite tracks", x = "Minutes listened")
    
    plotly::ggplotly(p, source = "3")
    
  })
  
  
  observeEvent(event_data("plotly_click", source = "3"), {
    if(input$personPlot3 == "Mikolaj"){
      df <- df_mikolaj
    }
    else if(input$personPlot3 == "Krzysiek"){
      df <- df_krzysiek
    }
    else{
      df <- df_daniel
    }
    
    barData = event_data("plotly_click", source = "3")
    
    df1 <- df %>% 
      group_by(master_metadata_track_name) %>% 
      summarise(Time = sum(ms_played)/60000) %>% 
      arrange(-Time) %>% 
      head(input$n) %>% 
      mutate(master_metadata_track_name = fct_reorder(master_metadata_track_name, Time))
    
    song <- df1[barData$pointNumber+1,1] %>% 
      mutate(master_metadata_track_name = as.character(master_metadata_track_name))
    
    song <- as.character(song)
    
    wykon <- ez <- as.character(df_daniel %>%
                                  filter(master_metadata_track_name == song)%>%
                                  head(1)%>%
                                  select(master_metadata_album_artist_name))
    
    showModal(modalDialog(easyClose = TRUE, title = tags$a(style = "color: white", icon('robot'), as.character(song)),
                          renderText(paste("Wykonawca : ", wykon)),
                          br(),
                          renderPlot({
                            endMonth <- as.character(c("01","02","03","04","05","06","07","08","09","10","11","12"))
                            MonthSum <- c(0,0,0,0,0,0,0,0,0,0,0,0)
                            df1 <- data.frame(endMonth, MonthSum)
                            
                            df2 <- df %>% 
                              mutate(endTime = as.Date(ts)) %>% 
                              mutate(
                                endMonth = strftime(endTime, "%m")
                              ) %>% 
                              filter(master_metadata_track_name == song) %>% 
                              group_by(endMonth) %>% 
                              summarise(MonthSum = sum(ms_played)/60000)
                            
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
                                    axis.title.y = element_text(),
                                    axis.title.x = element_text())+
                              labs(x = "Month", y = "Minutes played")+
                              scale_x_discrete(labels = c("Jan","Feb","Mar", "Apr", "May", "June", "Jul","Aug","Sep","Oct","Nov","Dec"))
                          }
                        )
                      ))
  })
  
  
  
  
  
  
  
  
  observeEvent(event_data("plotly_click", source = "2"), {
      
      if(input$personPlot3 == "Mikolaj"){
        df <- df_mikolaj
      }
      else if(input$personPlot3 == "Krzysiek"){
        df <- df_krzysiek
      }
      else{
        df <- df_daniel
      }
      
      
      barData = event_data("plotly_click", source = "2")
      
      
      df1 <- df %>% 
        group_by(master_metadata_album_album_name) %>% 
        summarise(Time = sum(ms_played)/60000) %>% 
        arrange(-Time) %>% 
        head(input$n) %>% 
        mutate(master_metadata_album_album_name = fct_reorder(master_metadata_album_album_name, Time))
      
      artist <- df1[barData$pointNumber+1,1] %>% 
        mutate(master_metadata_album_album_name = as.character(master_metadata_album_album_name))
      
      artist <- as.character(artist)
      
      favSong <- df %>% 
        filter(master_metadata_album_album_name == artist) %>% 
        group_by(master_metadata_track_name) %>% 
        summarise(Sum = sum(ms_played)/60000) %>%
        arrange(-Sum) %>% 
        head(5)
      colnames(favSong) <- c("Track Name", "Minutes Listened")
      
      
      
      
      
      showModal(modalDialog(easyClose = TRUE, title = tags$a(style = "color: white", icon('robot'), as.character(artist)),
                            renderPlot({
                              endMonth <- as.character(c("01","02","03","04","05","06","07","08","09","10","11","12"))
                              MonthSum <- c(0,0,0,0,0,0,0,0,0,0,0,0)
                              
                              df1 <- data.frame(endMonth, MonthSum)
                              
                              
                              
                              df2 <- df %>% 
                                mutate(endTime = as.Date(ts)) %>% 
                                mutate(
                                  endMonth = strftime(endTime, "%m")
                                ) %>% 
                                filter(master_metadata_album_album_name == artist) %>% 
                                group_by(endMonth) %>% 
                                summarise(MonthSum = sum(ms_played)/60000)
                              
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
                                      axis.title.y = element_text(),
                                      axis.title.x = element_text())+
                                labs(x = "Month", y = "Minutes played")+
                                scale_x_discrete(labels = c("Jan","Feb","Mar", "Apr", "May", "June", "Jul","Aug","Sep","Oct","Nov","Dec"))
                            }),
                            br(),
        render_gt({
          gt(favSong) %>% 
            tab_header(title = md("Favourite tracks on selected album")) %>% 
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
    
    
  })
  
  
  
  
  observeEvent(event_data("plotly_click", source = "1"), {

      if(input$personPlot3 == "Mikolaj"){
        df <- df_mikolaj
      }
      else if(input$personPlot3 == "Krzysiek"){
        df <- df_krzysiek
      }
      else{
        df <- df_daniel
      }
      
      barData = event_data("plotly_click", source = "1")
      
      df1 <- df %>% 
        group_by(master_metadata_album_artist_name) %>% 
        summarise(Time = sum(ms_played)/60000) %>% 
        arrange(-Time) %>% 
        head(input$n) %>% 
        mutate(master_metadata_album_artist_name = fct_reorder(master_metadata_album_artist_name, Time))
      
      artist <- df1[barData$pointNumber+1,1] %>% 
        mutate(master_metadata_album_artist_name = as.character(master_metadata_album_artist_name))
      
      artist <- as.character(artist)
      
      favSong <- df %>% 
        filter(master_metadata_album_artist_name == artist) %>% 
        group_by(master_metadata_track_name) %>% 
        summarise(Sum = sum(ms_played)/60000) %>%
        arrange(-Sum) %>% 
        head(5)
      colnames(favSong) <- c("Track Name", "Minutes Listened")
      
      
      
      showModal(modalDialog(easyClose = TRUE, title = tags$a(style = "color: white", icon('robot'), as.character(artist)), renderPlot({
        
        endMonth <- as.character(c("01","02","03","04","05","06","07","08","09","10","11","12"))
        MonthSum <- c(0,0,0,0,0,0,0,0,0,0,0,0)
        
        df1 <- data.frame(endMonth, MonthSum)
       
        
        
        df2 <- df %>% 
          mutate(endTime = as.Date(ts)) %>% 
          mutate(
            endMonth = strftime(endTime, "%m")
          ) %>% 
          filter(master_metadata_album_artist_name == artist) %>% 
          group_by(endMonth) %>% 
          summarise(MonthSum = sum(ms_played)/60000)
        
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
                axis.title.y = element_text(),
                axis.title.x = element_text())+
          labs(x = "Month", y = "Minutes played")+
          scale_x_discrete(labels = c("Jan","Feb","Mar", "Apr", "May", "June", "Jul","Aug","Sep","Oct","Nov","Dec"))
      }),
  
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
  })
  
}

