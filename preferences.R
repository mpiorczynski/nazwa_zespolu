library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(jsonlite)
library(tidyr)
library(stringr)


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


# ------------------------------------------------------------------------------

server <- function(input, output){
  
  output$tempo_histogram <- renderPlot({
    df_daniel %>% 
      ggplot(aes(tempo)) + 
      geom_histogram(binwidth = 4)
  })
  
  output$genres <- renderPlotly({
    
    
    df <- df_mikolaj %>% 
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
      head(20)
    
    
    plot_ly(
      type='treemap',
      data=df,
      labels=~genres,
      parents=character(20),
      values=~count,
      domain=list(column=0))
    
  })
  
  output$density_plot <- renderPlotly({
    
    df <- df_krzysiek
    p1 <- df %>% 
      ggplot(aes(danceability)) + 
      geom_density() + 
      theme_bw()
    
    p2 <- df %>% 
      ggplot(aes(energy)) + 
      geom_density() + 
      theme_bw()
    
    p3 <- df %>% 
      ggplot(aes(acousticness)) + 
      geom_density() + 
      theme_bw()
    
    p4 <- df %>% 
      ggplot(aes(instrumentalness)) + 
      geom_density() + 
      theme_bw()
    
    p5 <- df %>% 
    ggplot(aes(speechiness)) + 
      geom_density() + 
      theme_bw()
    
    p6 <- df %>% 
      ggplot(aes(valence)) + 
      geom_density() + 
      theme_bw()
    
    
    subplot(
      ggplotly(p1), 
      ggplotly(p2), 
      ggplotly(p3), 
      ggplotly(p4), 
      ggplotly(p5), 
      ggplotly(p6),
      nrows = 2
    )
    
  })
  
  output$radar_plot <- renderPlotly({
    df_mikolaj %>% 
      select(danceability, energy, speechiness, acousticness, instrumentalness, valence) %>% 
      pivot_longer(everything(), names_to = "feature", values_to = "value") %>% 
      group_by(feature) %>% 
      summarise(value = mean(value, na.rm = TRUE)) %>% 
      arrange(factor(feature, c("danceability", "energy",  "acousticness", "instrumentalness", "speechiness", "valence"))) %>% 
      plot_ly(
        type = 'scatterpolar',
        mode = 'markers',
        fill = 'toself',
        r = ~value,
        theta = ~feature,
        hovertemplate = paste("Feature: %{theta}<br>Value: %{r}<extra></extra>"),
        name = ""
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0, 1)
          )
        ),
        showlegend = F
      ) %>% 
      config(displayModeBar = FALSE)
    
  })
  
}

ui1 <- fluidPage(
  fluidRow(
    column(
      width = 6,
      plotOutput("tempo_histogram")
    ),
    column(
      width = 6,
      plotlyOutput("genres")
    )
  ),
  fluidRow(
    column(
      width = 6,
      plotlyOutput("radar_plot")
    ), 
    column(
      width = 6,
      h4("O jakich cechach najchętniej słuchamy muzyki"),
      plotlyOutput("density_plot")
    )
  )
)

ui2 <- fluidPage()

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

