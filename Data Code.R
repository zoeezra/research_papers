# IMPORTING DATA
tiktok_data <- read.csv(file.choose(), header = TRUE)
billboard_data <- read.csv(file.choose(), header = TRUE)

# IMPORTING SPOTIFY API AND OTHER LIBRARIES
library(spotifyr)
Sys.setenv(SPOTIFY_CLIENT_ID = '7c55d9cd2542482e896b6c7ce689f39e')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '5a334737836f48ed971886acba40937b')
library(dplyr)
library(zoo)
library(stringr)
library(data.table)

tiktok_data <- tiktok_data[-535,]

# CREATING VARIABLE LISTS
danceability <- c()
energy <- c()
loudness <- c()
speechiness <- c()
acousticness <- c()
instrumentalness <- c()
liveness <- c()
valence <- c()
tempo <- c()
key <- c()
song_mode <- c()
duration_ms <- c()
track_popularity <- c()
release_date <- c()
song_age <- c()

# PULLING DATA FROM SPOTIFY
for (i in tiktok_data$Spotify.ID){
  song_data_1 <- get_track_audio_features(as.character(i))
  
  danceability <- c(danceability, song_data_1$danceability)
  energy <- c(energy, song_data_1$energy)
  loudness <- c(loudness, song_data_1$loudness)
  speechiness <- c(speechiness, song_data_1$speechiness)
  acousticness <- c(acousticness, song_data_1$acousticness)
  instrumentalness <- c(instrumentalness, song_data_1$instrumentalness)
  liveness <- c(liveness, song_data_1$liveness)
  valence <- c(valence, song_data_1$valence)
  tempo <- c(tempo, song_data_1$tempo)
  key <- c(key, song_data_1$key)
  song_mode <- c(song_mode, song_data_1$mode)
  duration_ms <- c(duration_ms, song_data_1$duration_ms)
  
  song_data_2 <- get_track(as.character(i))
  track_popularity <- c(track_popularity, song_data_2$popularity)
  
  if (nchar(song_data_2$album$release_date) > 4){
    release_date <- c(release_date, substring(song_data_2$album$release_date, 1, 7))
  }else{
    release_date <- c(release_date, song_data_2$album$release_date)
  }
}


# ADDING DATA TO MAIN DATAFRAME
tiktok_data$danceability <- danceability
tiktok_data$energy <- energy
tiktok_data$loudness <- loudness
tiktok_data$speechiness <- speechiness
tiktok_data$acousticness <- acousticness
tiktok_data$instrumentalness <- danceability
tiktok_data$liveness <- liveness
tiktok_data$valence <- valence
tiktok_data$tempo <- tempo
tiktok_data$key <- key
tiktok_data$song_mode <- song_mode
tiktok_data$duration_ms <- duration_ms
tiktok_data$track_popularity <- track_popularity
tiktok_data$release_date <- release_date

# ADDING SONG AGE
for (i in 1:length(tiktok_data$Song)){
  if (nchar(tiktok_data$release_date[i]) == 4){
    y <- substring(tiktok_data$Date_Peaked[i], 1, 4)
    song_age <- c(song_age, as.numeric(y)-as.numeric(tiktok_data$release_date[i]))
  }else{
    song_age <- c(song_age, as.yearmon(tiktok_data$Date_Peaked[i])-as.yearmon(tiktok_data$release_date[i]))
  }
}

tiktok_data$song_age <- song_age*12

# ADDING DATA TO SONG LIST DATA
tiktok_songs <- read.csv(file.choose(), header = TRUE)
tiktok_songs <- tiktok_songs[-321, ]

artist <- c()
title <- c()

for (i in tiktok_songs$Spotify.ID){
  Song_Data <- get_track(as.character(i))
  artist <- c(artist, Song_Data$artists$name[1])
  title <- c(title, str_to_title(Song_Data$name))
}

tiktok_songs$artists <- artist
tiktok_songs$song <- title

write.csv(tiktok_songs, "C:\\Users\\zoeez\\Documents\\School\\UP Diliman\\AY 2022-2023\\2nd Sem\\Stat 136\\Tiktok_Songs.csv", row.names = TRUE)

# DETERMINING ARTIST APPEARANCE IN BILLBOARD CHART
artist_popularity <- c()

for (i in tiktok_songs$artists){
  if (any(billboard_data$artist %like% as.character(i)) == TRUE){
    artist_popularity <- c(artist_popularity, i)
  }
}
## Inputted popularity manually based on list ##

artist_pop_data <- read.csv(file.choose(), header = TRUE)
artist_pop_data <- artist_pop_data[, -c(2, 3)]
tiktok_data_1 <- left_join(tiktok_data, artist_pop_data, by = "Spotify.ID")

write.csv(tiktok_data_1, "C:\\Users\\zoeez\\Documents\\School\\UP Diliman\\AY 2022-2023\\2nd Sem\\Stat 136\\Final_Tiktok_Data.csv", row.names = TRUE)

# EARLIEST AND LATEST
tiktok_data_2 <- read.csv(file.choose(), header = TRUE)
tiktok_data_2$Date <- as.yearmon(tiktok_data_2$Date)

tiktok_data_3 <- tiktok_data_2 %>%
  group_by(Spotify.ID) %>%
  filter(Date == max(Date)) %>%
  distinct

tiktok_data_3 <- tiktok_data_3[!duplicated(tiktok_data_3$Spotify.ID),]
tiktok_data_3 <- tiktok_data_3[-594,]

tiktok_data_4 <- tiktok_data_2 %>%
  group_by(Spotify.ID) %>%
  filter(Date == min(Date)) %>%
  distinct

tiktok_data_4 <- tiktok_data_4[!duplicated(tiktok_data_4$Spotify.ID),]
tiktok_data_4 <- tiktok_data_4[-593,]

write.csv(tiktok_data_3, "C:\\Users\\zoeez\\Documents\\School\\UP Diliman\\AY 2022-2023\\2nd Sem\\Stat 136\\Final_Tiktok_Data_Latest.csv", row.names = TRUE)
write.csv(tiktok_data_4, "C:\\Users\\zoeez\\Documents\\School\\UP Diliman\\AY 2022-2023\\2nd Sem\\Stat 136\\Final_Tiktok_Data_Earliest.csv", row.names = TRUE)
