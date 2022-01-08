library(jsonlite)
library(stringr)
library(dplyr)
library(ggplot2)
library(shiny)
library(shinycssloaders)

wdays <- c("Niedziela", "Poniedziałek", "Wtorek", "Środa", "Czwartek", "Piątek", "Sobota")
hours <- sprintf("%02d:00-%02d:00", 0:23, 1:24)

df <- fromJSON("data/krzysiek/endsong.json") %>% 
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

# WeekDay / Hour heatmap
df_heatmap <- expand.grid(weekday = 1:7, hour = 0:23)

df_heatmap <- df_heatmap %>% 
  merge(
  df %>%
  group_by(weekday, hour) %>% 
  summarise(n = n()),
  by = c("weekday", "hour"),
  all.x=TRUE
  ) %>% 
  mutate(n = ifelse(is.na(n), 0, n))

ggplot(df_heatmap) + 
  geom_tile(aes(x=weekday, y=hour, fill=n)) +
  scale_x_continuous(breaks=1:7, labels=wdays) +
  scale_y_continuous(breaks=0:23, labels=hours) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  scale_fill_gradient(high = "green", low = "black")

# listening density plot
ggplot(df, aes(x=hour*60*60 + min*60 + s, y=..count../2000, color=platform)) +
  geom_density(alpha=0.1) +
  scale_y_continuous(name = "", breaks=NULL) + 
  scale_x_continuous(name = "Hour", breaks = (0:24)*60*60, labels=0:24)

  
  