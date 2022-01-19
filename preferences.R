library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(jsonlite)
library(tidyr)
library(stringr)
library(bslib)
library(wordcloud)
library(wordcloud2)
library(ggstream)
library(shinydashboard)


# data
streaming_history_df <- fromJSON("data/krzysiek/endsong.json")
features_df <- read.csv("data/krzysiek/tracks_features.csv")
df_krzysiek <- left_join(streaming_history_df, features_df, by = "spotify_track_uri", keep = FALSE)

streaming_history_df <- fromJSON("data/mikołaj/endsong.json")
features_df <- read.csv("data/mikołaj/tracks_features.csv")
df_mikolaj <- left_join(streaming_history_df, features_df, by = "spotify_track_uri", keep = FALSE)

streaming_history_df <- read.csv("data/daniel/endsong.csv")
features_df <- read.csv("data/daniel/tracks_features.csv")
df_daniel <- left_join(streaming_history_df, features_df, by = c("trackName" = "master_metadata_track_name"), keep = FALSE)




# utils to genre plot 
genre_seeds <- c(
  "acoustic",
  "afrobeat",
  "alt-rock",
  "alternative",
  "ambient",
  "anime",
  "black-metal",
  "bluegrass",
  "blues",
  "bossanova",
  "breakbeat",
  "cantopop",
  "chicago-house",
  "children",
  "chill",
  "classical",
  "club",
  "comedy",
  "country",
  "dance",
  "dancehall",
  "death-metal",
  "deep-house",
  "detroit-techno",
  "disco",
  "disney",
  "drum-and-bass",
  "dub",
  "dubstep",
  "edm",
  "electro",
  "electronic",
  "emo",
  "folk",
  "forro",
  "funk",
  "garage",
  "gospel",
  "goth",
  "grindcore",
  "groove",
  "grunge",
  "guitar",
  "happy",
  "hard-rock",
  "hardcore",
  "hardstyle",
  "heavy-metal",
  "hip-hop",
  "hip hop",
  "holidays",
  "honky-tonk",
  "house",
  "idm",
  "indie",
  "indie-pop",
  "industrial",
  "j-dance",
  "j-idol",
  "j-pop",
  "j-rock",
  "jazz",
  "k-pop",
  "kids",
  "latin",
  "latino",
  "malay",
  "mandopop",
  "metal",
  "metal-misc",
  "metalcore",
  "minimal-techno",
  "movies",
  "mpb",
  "new-age",
  "new-release",
  "opera",
  "pagode",
  "party",
  "piano",
  "pop",
  "pop-film",
  "post-dubstep",
  "power-pop",
  "progressive-house",
  "psych-rock",
  "punk",
  "punk-rock",
  "r-n-b",
  "rainy-day",
  "reggae",
  "reggaeton",
  "road-trip",
  "rock",
  "rock-n-roll",
  "rockabilly",
  "romance",
  "sad",
  "salsa",
  "samba",
  "sertanejo",
  "show-tunes",
  "singer-songwriter",
  "ska",
  "sleep",
  "songwriter",
  "soul",
  "soundtrack",
  "study",
  "summer",
  "synth-pop",
  "tango",
  "techno",
  "trance",
  "trap",
  "trip-hop",
  "work-out"
)



unify_genres <- function(x, genre_seeds) {
  res <- character(length(x))
  for (i in 1:length(x)) {
    idx <- which(do.call(str_detect, c(x[i], list(genre_seeds))))
    res[i] <- paste0(genre_seeds[idx], collapse = ", ")
  }
  res
}

my_theme <- bs_theme(
  bg = "#121212",
  fg = "#FFFFFF",
  primary = "#1DB954",
)
# ------------------------------------------------------------------------------

server <- function(input, output){
  
  output$tempo_histogram <- renderPlot({
    
    if (input$person == "Daniel") {
      df <- df_daniel
    } else if (input$person == "Krzysiek") {
      df <- df_krzysiek
    } else {
      df <- df_mikolaj
    }
    
    df %>% 
      ggplot(aes(tempo)) + 
      geom_histogram(binwidth = 4, fill = "#1DB954", alpha = 0.87) +
      theme_dark() + 
      xlab("") + 
      ylab("") + 
      theme(
        plot.background = element_rect(fill = "#121212", colour = "#121212"),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour = "#FFFFFF")
        
      ) 
  })
  
  output$genres <- renderPlot(bg = "#121212", {
    
    if (input$person == "Daniel") {
      df <- df_daniel
    } else if (input$person == "Krzysiek") {
      df <- df_krzysiek
    } else {
      df <- df_mikolaj
    }
    
    
    df <- df %>% 
      group_by(genres) %>% 
      summarise(count = n()) %>% 
      arrange(-count) %>% 
      mutate(genres = unify_genres(genres, genre_seeds)) %>% 
      group_by(genres) %>% 
      summarise(count = sum(count)) %>% 
      arrange(-count) %>% 
      separate_rows(genres, sep = ", ") %>% 
      group_by(genres) %>% 
      summarise(count = sum(count)) %>% 
      arrange(-count) %>% 
      filter(genres != "") %>% 
      head(30)
    
    
    wordcloud(df$genres, df$count, col = terrain.colors(length(df$genres)), bg = "#121212", scale = c(4, 1.5))
    
    # 
    # plot_ly(
    #   type='treemap',
    #   data=df,
    #   labels=~genres,
    #   parents=character(20),
    #   values=~count,
    #   domain=list(column=0)
    # ) %>% 
    # layout(
    #   showlegend = FALSE,
    #   plot_bgcolor  = "#121212",
    #   paper_bgcolor = "#121212",
    #   font = list(color="white")
    # ) %>% 
    # config(displayModeBar = FALSE)
    # 
    
  })
  
  output$density_plot <- renderPlotly({
    
    
    density_df_krzysiek <- df_krzysiek %>% 
      select(danceability, energy, acousticness, instrumentalness, speechiness, valence) %>% 
      lapply(., density, na.rm = TRUE)
    
    density_df_mikolaj <- df_mikolaj %>% 
      select(danceability, energy, acousticness, instrumentalness, speechiness, valence) %>% 
      lapply(., density, na.rm = TRUE)
    
    
    density_df_daniel <- df_daniel %>% 
      select(danceability, energy, acousticness, instrumentalness, speechiness, valence) %>% 
      lapply(., density, na.rm = TRUE)
      
    feature_names <- names(density_df_daniel)
    
    
    if (input$person == "Krzysiek") {
      alpha_krzysiek <- 1
      alpha_mikolaj <- 0.1
      alpha_daniel <- 0.1
    } else if (input$person == "Mikołaj") {
      alpha_krzysiek <- 0.1
      alpha_mikolaj <- 1
      alpha_daniel <- 0.1
    } else {
      alpha_krzysiek <- 0.1
      alpha_mikolaj <- 0.1
      alpha_daniel <- 1
    }
    
    
    plots <- lapply(feature_names, function(feature_n) {
        plot_ly(
          type = 'scatter',
          mode = 'lines' 
        ) %>% 
        add_trace(
          x = density_df_krzysiek[[feature_n]][['x']],
          y = density_df_krzysiek[[feature_n]][['y']],
          color = 'red', 
          opacity = alpha_krzysiek,
          name = 'Krzysiek'
        ) %>% 
        add_trace(
          x = density_df_mikolaj[[feature_n]][['x']],
          y = density_df_mikolaj[[feature_n]][['y']],
          color = 'green', 
          opacity = alpha_mikolaj,
          name = 'Mikołaj'
        ) %>% 
        add_trace(
          x = density_df_daniel[[feature_n]][['x']],
          y = density_df_daniel[[feature_n]][['y']],
          color = 'blue', 
          opacity = alpha_daniel,
          name = 'Daniel'
        ) %>% 
        layout(
          xaxis = list(
            tickvals = list(0, 0.5, 1),
            title = feature_n
          ),
          yaxis = list(showticklabels = FALSE),
          showlegend = FALSE,
          plot_bgcolor  = "#121212",
          paper_bgcolor = "#121212",
          font = list(color="white")
        ) %>% 
        config(displayModeBar = FALSE)
     
     
     
     
    })
    firstRow <- subplot(plots[1:3], nrows = 1, titleX = TRUE) 
    secondRow <- subplot(plots[4:6], shareX = TRUE, nrows = 1, titleX = TRUE) 
    subplot(firstRow, secondRow, nrows = 2, titleX = TRUE, margin = 0.05) %>%
    config(displayModeBar = FALSE)
    
  })
  
  output$radar_plot <- renderPlotly({
    radar_krzysiek <- df_krzysiek %>% 
      select(danceability, energy, speechiness, acousticness, instrumentalness, valence) %>% 
      pivot_longer(everything(), names_to = "feature", values_to = "value") %>% 
      group_by(feature) %>% 
      summarise(value = mean(value, na.rm = TRUE)) %>% 
      arrange(factor(feature, c("danceability", "energy",  "acousticness", "instrumentalness", "speechiness", "valence"))) 
    
    radar_mikolaj <- df_mikolaj %>% 
      select(danceability, energy, speechiness, acousticness, instrumentalness, valence) %>% 
      pivot_longer(everything(), names_to = "feature", values_to = "value") %>% 
      group_by(feature) %>% 
      summarise(value = mean(value, na.rm = TRUE)) %>% 
      arrange(factor(feature, c("danceability", "energy",  "acousticness", "instrumentalness", "speechiness", "valence"))) 
    
    radar_daniel <- df_daniel %>% 
      select(danceability, energy, speechiness, acousticness, instrumentalness, valence) %>% 
      pivot_longer(everything(), names_to = "feature", values_to = "value") %>% 
      group_by(feature) %>% 
      summarise(value = mean(value, na.rm = TRUE)) %>% 
      arrange(factor(feature, c("danceability", "energy",  "acousticness", "instrumentalness", "speechiness", "valence"))) 
    
    
    
    fig <- plot_ly(
        type = 'scatterpolar',
        mode = 'markers',
        fill = 'toself'
    )
    
    if (input$person == "Krzysiek") {
      alpha_krzysiek <- 0.7
      alpha_mikolaj <- 0.5
      alpha_daniel <- 0.5
    } else if (input$person == "Mikołaj") {
      alpha_krzysiek <- 0.5
      alpha_mikolaj <- 0.7
      alpha_daniel <- 0.5
    } else {
      alpha_krzysiek <- 0.5
      alpha_mikolaj <- 0.5
      alpha_daniel <- 0.7
    }
    
    
    fig <- fig %>% add_trace(
      data = radar_krzysiek,
      r = ~value,
      theta = ~feature,
      hovertemplate = paste("Feature: %{theta}<br>Value: %{r:.3f}<extra></extra>"),
      name = "Krzysiek",
      fillcolor = "red",
      opacity = alpha_krzysiek
    )
    
    fig <- fig %>% add_trace(
      data = radar_mikolaj,
      r = ~value,
      theta = ~feature,
      hovertemplate = paste("Feature: %{theta}<br>Value: %{r:.3f}<extra></extra>"),
      name = "Mikołaj",
      fillcolor = "green",
      opacity = alpha_mikolaj
    )
    
    fig <- fig %>% add_trace(
      data = radar_daniel,
      r = ~value,
      theta = ~feature,
      hovertemplate = paste("Feature: %{theta}<br>Value: %{r:.3f}<extra></extra>"),
      name = "Daniel",
      fillcolor = "blue",
      opacity = alpha_daniel
    )
    
    fig  %>%
      layout(
        font = list(color = "#1DB954"),
        plot_bgcolor  = "rgba(0, 0, 0, 0)",
        paper_bgcolor = "rgba(0, 0, 0, 0)",
        polar = list(
          bgcolor = "rgba(0, 0, 0, 0)",
          radialaxis = list(
            visible = T,
            range = c(0,1),
            gridcolor = "#FFFFFF",
            color = "#FFFFFF",
            tickfont = list(color = "#FFFFFF")
          ),
          angularaxis = list(
            linecolor = "#FFFFFF",
            tickfont = list(size = 14)
          )
        ),
        showlegend = F
      ) %>% 
      config(displayModeBar = FALSE)
    
  })
  
  # output$genres_stream <- renderPlot({
  #   
  #   
  #   df <- df_krzysiek
  #   
  #   top_genres <- df %>% 
  #     mutate(genres = unify_genres(genres, genre_seeds)) %>% 
  #     separate_rows(genres, sep = ", ") %>%
  #     filter(genres != "") %>%
  #     group_by(genres) %>% 
  #     summarise(count = n()) %>% 
  #     arrange(-count) %>% 
  #     head(5) %>% 
  #     pull(genres)
  #   
  #   
  #   p <- df %>% 
  #     mutate(date = as.Date(ts)) %>% 
  #     filter(date > as.Date("2021-01-01") & date < as.Date("2021-12-31")) %>% 
  #     mutate(genres = unify_genres(genres, genre_seeds)) %>% 
  #     separate_rows(genres, sep = ", ") %>%
  #     filter(genres != "") %>% 
  #     group_by(date, genres) %>% 
  #     summarise(ms_played = sum(ms_played), .groups = "drop") %>% 
  #     filter(genres %in% top_genres) %>% 
  #     ggplot(aes(date, ms_played, color = genres, fill = genres)) + 
  #     geom_stream() +
  #     scale_x_date(date_labels = "%b") + 
  #     theme(
  #       plot.background = element_rect(fill = "#121212", colour = "#121212"),
  #       panel.background = element_blank(),
  #       panel.border = element_rect(fill = NA, colour = NA),
  #       panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank(),
  #       axis.text.y = element_blank(),
  #       axis.text.x = element_text(colour = "#FFFFFF")
  #       
  #     ) 
  #   
  #   p
  # })
}
  



# ui1 <- fluidRow(
#   fluidRow(
#     column( 
#       width = 6,
#       plotOutput("tempo_histogram")
#     ),
#     column(
#       width = 6,
#       box(
#         plotOutput("genres")
#       )
#     )
#   ),
#   fluidRow(
#     column(
#       width = 4,
#       plotlyOutput("radar_plot")
#     ),
#     column(
#       width = 8,
#       plotlyOutput("density_plot")
#     )#,
#     # fluidRow(
#     #   plotOutput("genres_stream", width = "100%", height = "30%")
#     # )
#   )
# )



# 
# ui1 <- fluidPage(
#   radioButtons("person", "Select person", choices = c("Daniel", "Krzysiek", "Mikołaj")),
#   fluidRow(
#     column(
#       width = 6,
#       plotOutput("tempo_histogram")
#     ),
#     column(
#       width = 6,
#       plotOutput("genres")
#     )
#   ),
#   fluidRow(
#     column(
#       width = 4,
#       plotlyOutput("radar_plot")
#     ), 
#     column(
#       width = 8,
#       plotlyOutput("density_plot")
#     )#,
#     # fluidRow(
#     #   plotOutput("genres_stream", width = "100%", height = "30%")
#     # )
#   )
# )
# 
# ui2 <- fluidPage()
# 
# ui3 <- fluidPage()
# 
# app_ui <- navbarPage(
#   id = "id",
#   title = "Prototype",
#   tabPanel("Plot1", ui1),
#   tabPanel("Plot2", ui2),
#   tabPanel("Plot3", ui3),
#   theme = my_theme
# )
# 



app_ui <- dashboardPage(
  title = "SpotifyViz",
  dashboardHeader(title = tags$a(img(src='Spotify_Logo_RGB_Green.png', style = "width: 131px; background-color: #040404"))), 
  dashboardSidebar(
    width = "230px",
    sidebarMenu(
      menuItem("Home", tabName = "home"),
      menuItem("Activity", tabName = "activity"),
      menuItem("Preferences", tabName = "preferences")
    ),
    radioButtons("person", "Select person", choices = c("Daniel", "Krzysiek", "Mikołaj"))
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
      .main-sidebar .sidebar .sidebar-menu {
        width: 230px;
        height: 100%;
        background-color: #040404;
      }
      
      html {
        font-size: 16px;
      }
      
      .content-wrapper .content {
          background-color: #121212;
      }
      
      * {
          font-family: "Open Sans", sans-serif;
          letter-spacing: -0.35px;
      } 
      
      .skin-blue .main-header .navbar {
        background-color: #040404;
      }       
      
      skin-blue .main-sidebar {
        background-color: #040404;
      }
      
      .box .box-body {
        background-color: #121212;
        -webkit-box-shadow: none; 
        -moz-box-shadow: none;
        box-shadow: none;
      }
      
      .logo {
        background-color:  #040404 !important;
      }
      .navbar {
        background-color:  #040404 !important;
      }
      
      .radio {
        font-weight: normal;
        background-color: #040404;
      }
      
      .control-label {
        font-weight: normal;
      }
      .shiny-bound-input {
        background-color: #040404;

      }
      
      #sidebarCollapsed { 
        background-color: #040404;
      }
      
      
      .skin-blue .main-sidebar .sidebar .sidebar-menu  a{
        border-radius: 5px;
        border-color: transparent;
      }
      
      .skin-blue .main-header .navbar .sidebar-toggle:hover {
          background-color: #1DB954;
      }
      
    

    '))),
    tabItems(
      tabItem(tabName = "preferences",
              fluidRow(
                box(plotOutput("tempo_histogram"), background = "red"),
                box(plotOutput("genres"), background = "red")
              ), 
              fluidRow(
                box(plotlyOutput("radar_plot"), width = 4, background = "red"),
                box(plotlyOutput("density_plot"), width = 8, background = "red")  
              )
              
      ),
      tabItem(tabName = "home",
              h2("No tab content")
      ),
      tabItem(tabName = "activity",
              h2("No tab content")
      )
    )
  )
)

shinyApp(app_ui, server)

