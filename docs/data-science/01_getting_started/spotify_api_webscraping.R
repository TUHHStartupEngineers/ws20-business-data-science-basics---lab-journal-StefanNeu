Sys.setenv(SPOTIFY_CLIENT_ID = "63644fbe2933479081c9c22044b42941")
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'a00169314253484f9a1761ca73d4dfe0')

# load libraries
library(spotifyr)
library(tidyverse)

# get authorization token, even though most of methods call get_spotify_access_token() by themselves
access_token <- get_spotify_access_token()

# raw album data from "amo" by Bring Me The Horizon, by using the spotify id of that album
raw_data <- get_album("04mkS7FooK8fRbB626T9NR")

# select important columns
tracks_tbl <- raw_data$tracks$items %>% select(track_number, name, explicit, id, duration_ms)

tracks_tbl

# acquire the tempo (in BPM) from all tracks by accessing with their respective track id
tracks_bpm <- vector()

for(i in 1:dim(tracks_tbl)[1]) {
  tracks_bpm <- c(tracks_bpm, get_track_audio_analysis(tracks_tbl[i, 4])$track$tempo)
}

# calculate the mean bpm of all tracks
# result for that album: 132 BPM
bpm_mean = mean(tracks_bpm) 

# same for tempo confidence
tracks_bpm_conf <- vector()

for(i in 1:dim(tracks_tbl)[1]) {
  tracks_bpm_conf <- c(tracks_bpm_conf, get_track_audio_analysis(tracks_tbl[i, 4])$track$tempo_confidence)
}

tempo_tbl <- data.frame(tracks_bpm, tracks_bpm_conf)

# we could state the hypothesis that a high tempo could result in a low tempo confidence, 
# so we test the correlation of tempo and tempo confidence

# result is close to zero, so there is no strong correlation
correlation = cor(tracks_bpm, tracks_bpm_conf)


# we also try to confirm these results from a scatter plot
ggplot(tempo_tbl, aes(x =tracks_bpm, y = tracks_bpm_conf)) +
         
         geom_point(size=4, shape=4) +
  
        geom_text(label=tracks_tbl$name) + 
  
  labs(
    title = "Tempo and Tempo Confidence of albumtracks from \"amo\" by Bring Me The Horizon",
    x = "Tempo in BPM (Beats Per Minute)",
    y = "Tempo Confidence in percent"
  )






