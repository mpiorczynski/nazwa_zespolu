library(spotifyr)
library(jsonlite)
library(stringr)

# data loading
path_to_data <- "data/krzysiek/endsong.json"
streaming_history_df <- fromJSON(path_to_data)

# access to API
Sys.setenv(SPOTIFY_CLIENT_ID = "b9d14492b8234f1fb44794448afe82a8")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "f705777d52954af8b73ea11089f62358")
access_token <- get_spotify_access_token()

unique_tracks <- unique(streaming_history_df$spotify_track_uri) 
# remove podcasts which have NA
unique_tracks <- unique_tracks[!is.na(unique_tracks)]
n <- length(unique_tracks)

features_df <- data.frame(
  spotify_track_uri = unique_tracks,
  genres = character(n),
  danceability = character(n),
  energy = character(n),
  key = character(n),
  loudness = character(n),
  mode = character(n),
  speechiness = character(n),
  acousticness = character(n),
  instrumentalness = character(n),
  liveness = character(n),
  valence = character(n),
  tempo = character(n),
  type = character(n),
  id = character(n),
  uri = character(n),
  track_href = character(n),
  analysis_url = character(n),
  duration_ms = character(n),
  time_signature = character(n)
)

for (i in 1:n) {
  cat(paste0("Processing...: ", i, " of ", n, sep = "\n"))
  track_uri <- features_df$spotify_track_uri[i]
  track_id <- str_sub(track_uri, -22)
  track_features <- get_track_audio_features(track_id)
  features_df[i, 3:20] <- track_features
  track <- get_track(track_id)
  artist_id <- track$artists$id[1]
  artist <- get_artist(artist_id)
  genres <- artist$genres
  features_df$genres[i] <- paste(genres, collapse = ", ")
}

features_df

# saving output
path_to_save <- "data/krzysiek/"
filepath <- file.path(path_to_save, "tracks_features.csv")
write.csv(features_df, filepath, row.names = FALSE)
