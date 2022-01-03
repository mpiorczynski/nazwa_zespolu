library(jsonlite)
library(ggplot2)
library(dplyr)
library(forcats)

streaming_history_df <- fromJSON("data/my_spotify_data/StreamingHistory0.json")
View(streaming_history_df)
streaming_history_df %>% slice_max(msPlayed, n = 10) %>% mutate(msPlayed = msPlayed / (1000 * 60)) %>% View()

ggplot(streaming_history_df) + 
  geom_histogram(aes(x = msPlayed / (1000 * 60)), bins = 50) + 
  scale_x_continuous(expand = c(0, 0), minor_breaks = 0:45) + 
  scale_y_continuous(expand = c(0, 0)) + 
  labs(
    title = "Distribution of time of listened tracks",
    x = "Track length [min]", 
    y = "Count"
  ) + 
  theme_bw() 

ggplot(streaming_history_df) + 
  geom_boxplot(aes(x = msPlayed / (1000 * 60))) + 
  labs(
    title = "Distribution of time of listened tracks",
    x = "Listening time [min]", 
  ) + 
  theme_bw() 


streaming_history_df %>% 
  group_by(artistName) %>% 
  summarise(count = n()) %>% 
  mutate(artistName = fct_reorder(artistName, count)) %>% 
  slice_max(count, n = 10) %>% 
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

streaming_history_df %>% 
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


