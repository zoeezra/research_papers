# IMPORTING LIBRARIES AND DATASETS
library(olsrr)
library(car)
library(ggplot2)
library(reshape2)

original_data <- read.csv(file.choose(), header = TRUE)

### WITH DUPLICATES ###

# TURNING DUMMY VARIABLES INTO FACTORS AND CONVERTING COLUMNS
original_data$artist_popularity <- as.factor(original_data$artist_popularity)
original_data$Trend <- as.factor(original_data$Trend)
original_data$key <- as.factor(original_data$key)
original_data$song_mode <- as.factor(original_data$song_mode)
original_data$duration_s <- original_data$duration_ms/1000

# CREATING LINEAR MODEL
original_lm <- lm(track_popularity~.-Date-Year-Month-Song-Date_Peaked-Year.Peaked-Month.Peaked-Spotify.Link-Spotify.ID-release_date-duration_ms-instrumentalness, data = original_data)
summary(original_lm)

# DIAGNOSTIC ANALYSIS
par(mfrow = c(2,2))
plot(tiktok_s_lm)
hist(tiktok_s_lm$residuals)
ols_test_normality(tiktok_s_lm)
ols_test_breusch_pagan(tiktok_s_lm, rhs = TRUE)
ols_plot_added_variable(tiktok_s_lm)
ols_plot_comp_plus_resid(tiktok_s_lm)

# REMOVING VARIABLES
ols_step_all_possible(tiktok_s_lm)
ols_step_both_p(tiktok_s_lm, details = TRUE)
ols_step_forward_p(tiktok_s_lm, details = TRUE)
ols_step_backward_p(tiktok_s_lm, details = TRUE)

# OUTLIERS
ols_plot_resid_stud(tiktok_s_lm)

# TESTING MULTICOLLINEARITY
tiktok_s_lin <- tiktok_s_data[, c(7:9, 13:21, 24, 27)]
round(cor(tiktok_s_lin), 3)
pairs(tiktok_s_lin, lower.panel = NULL)

### LATEST DATA ###

latest_data <- read.csv(file.choose(), header = TRUE)

# TURNING DUMMY VARIABLES INTO FACTORS
latest_data$artist_popularity <- as.factor(latest_data$artist_popularity)
latest_data$Trend <- as.factor(latest_data$Trend)
latest_data$key <- as.factor(latest_data$key)
latest_data$song_mode <- as.factor(latest_data$song_mode)

# LATEST MODEL
latest_lm <- lm(track_popularity~.-ID-Year-Month-Song-Date.Peaked-Year.Peaked-Month.Peaked-Spotify.Link-Spotify.ID-release_date-instrumentalness, data = latest_data)
summary(latest_lm)
ols_regress(latest_lm)

# DIAGNOSTIC ANALYSIS
par(mfrow = c(2, 2))
plot(latest_lm)
par(mfrow = c(1,1))
hist(latest_lm$residuals, main = "Histogram of Model Residuals", xlab = "Residuals")
ols_test_normality(tiktok_latest_lm)
ols_test_breusch_pagan(tiktok_latest_lm, rhs = TRUE)
ols_plot_added_variable(tiktok_latest_lm)
ols_plot_comp_plus_resid(tiktok_latest_lm)

# REMOVING VARIABLES
ols_step_both_p(latest_lm, details = TRUE)
ols_step_forward_p(latest_lm, details = TRUE)
ols_step_backward_p(latest_lm, details = TRUE)
ols_step_all_possible(latest_lm)

# CORRELATION MATRIX
latest_data_red <- latest_data[, c(9:11, 15:16, 18, 21:23, 29)]
corr_matrix <- round(cor(latest_data_red), 3)

get_upper_tri <- function(corr){
  corr[lower.tri(corr)] <- NA
  return(corr)
}

upper_tri <- get_upper_tri(corr_matrix)
melted_corr_matrix <- melt(upper_tri, na.rm = TRUE)

ggplot(data = melted_corr_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#69C9D0", high = "#EE1D52", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation\nCoefficient") +
  geom_text(aes(Var1, Var2, label = value), color = "black", size = 5) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(angle = 90, size = 14),
    axis.text.y = element_text(size = 14)) +
  ggtitle("Correlation Matrix of Continuous Variables")

# REDUCED MODEL
latest_lm_red <- lm(track_popularity ~ Play_Count+Popular_Videos+Trend+danceability+energy+speechiness+liveness+valence+tempo+song_age+artist_popularity, data = latest_data)
ols_regress(latest_lm_red)
summary(latest_lm_red)

# DIAGNOSTIC ANALYSIS
par(mfrow = c(2, 2))
plot(latest_lm_red)
par(mfrow = c(1,1))
hist(latest_lm_red$residuals, main = "Histogram of Reduced Model Residuals", xlab = "Residuals")
ols_test_normality(latest_lm_red)
ols_test_breusch_pagan(latest_lm_red, rhs = TRUE)
ols_plot_added_variable(latest_lm_red)
ols_plot_comp_plus_resid(latest_lm_red)

## Partial Residual Plots
play_count_lm <- lm(track_popularity ~ Play_Count, data = latest_data)
popular_videos_lm <- lm(track_popularity ~ Popular_Videos, data = latest_data)
trend_lm <- lm(track_popularity ~ Trend, data = latest_data)
danceability_lm <- lm(track_popularity ~ danceability, data = latest_data)
energy_lm <- lm(track_popularity ~ energy, data = latest_data)
speechiness_lm <- lm(track_popularity ~ speechiness, data = latest_data)
liveness_lm <- lm(track_popularity ~ liveness, data = latest_data)
valence_lm <- lm(track_popularity ~ valence, data = latest_data)
tempo_lm <- lm(track_popularity ~ tempo, data = latest_data)
song_age_lm <- lm(track_popularity ~ song_age, data = latest_data)
artist_popularity_lm <- lm(track_popularity ~ artist_popularity, data = latest_data)

par(mfrow = c(1, 3), cex.lab = 1.5, cex.axis = 1)
plot(play_count_lm$residuals~latest_data$Play_Count, xlab = 'Play Count', ylab = "Residuals", col = "black", )
plot(popular_videos_lm$residuals~latest_data$Popular_Videos, xlab = 'Popular Videos', ylab = "Residuals", col = "black")
plot(song_age_lm$residuals~latest_data$song_age, xlab = 'Song Age', ylab = "Residuals", col = "black")

# LINEARIZATION (LADDER TRANSFORMATION)
## 1
latest_lm_line <- lm(track_popularity^1 ~ log(Play_Count)+log(Popular_Videos)+Trend+danceability+energy+speechiness+liveness+valence+tempo+log(song_age+39)+artist_popularity, data = latest_data)
summary(latest_lm_line)
par(mfrow = c(2, 2))
plot(latest_lm_line)
ols_test_normality(latest_lm_line)
ols_test_breusch_pagan(latest_lm_line, rhs = TRUE)
## 1.5
latest_lm_line <- lm(track_popularity^1.5 ~ log(Play_Count)+log(Popular_Videos)+Trend+danceability+energy+speechiness+liveness+valence+tempo+log(song_age+39)+artist_popularity, data = latest_data)
summary(latest_lm_line)
par(mfrow = c(2, 2))
plot(latest_lm_line)
ols_test_normality(latest_lm_line)
ols_test_breusch_pagan(latest_lm_line, rhs = TRUE)
## 2
latest_lm_line <- lm(track_popularity^2 ~ log(Play_Count)+log(Popular_Videos)+Trend+danceability+energy+speechiness+liveness+valence+tempo+log(song_age+39)+artist_popularity, data = latest_data)
summary(latest_lm_line)
par(mfrow = c(2, 2), cex.label = 1, cex.axis = 1)
plot(latest_lm_line)
ols_test_normality(latest_lm_line)
ols_test_breusch_pagan(latest_lm_line, rhs = TRUE)
## 2.5
latest_lm_line <- lm(track_popularity^2.5 ~ log(Play_Count)+log(Popular_Videos)+Trend+danceability+energy+speechiness+liveness+valence+tempo+log(song_age+39)+artist_popularity, data = latest_data)
summary(latest_lm_line)
par(mfrow = c(2, 2))
plot(latest_lm_line)
ols_test_normality(latest_lm_line)
ols_test_breusch_pagan(latest_lm_line, rhs = TRUE)
## 3
latest_lm_line <- lm(track_popularity^3 ~ log(Play_Count)+log(Popular_Videos)+Trend+danceability+energy+speechiness+liveness+valence+tempo+log(song_age+39)+artist_popularity, data = latest_data)
summary(latest_lm_line)
par(mfrow = c(2, 2))
plot(latest_lm_line)
ols_test_normality(latest_lm_line)
ols_test_breusch_pagan(latest_lm_line, rhs = TRUE)

par(mfrow = c(1,1))
hist(latest_lm_line$residuals, main = "Histogram of Linearized Model Residuals", xlab = "Residuals")
ols_regress(latest_lm_line)
ols_plot_comp_plus_resid(latest_lm_line)

# WEIGHTED LEAST SQUARES ESTIMATION
par(mfrow = c(1, 2), cex.lab = 1.5, cex.axis = 1)
plot(speechiness_lm$residuals~latest_data$speechiness, xlab = 'Speechiness', ylab = "Residuals", col = "black")
plot(liveness_lm$residuals~latest_data$Popular_Videos, xlab = 'Liveness', ylab = "Residuals", col = "black")

sd <- lm(abs(latest_lm_line$residuals)~speechiness+liveness, data = latest_data)$fitted.values
w <- (sd)^-2
latest_lm_w <- lm(track_popularity^2 ~ log(Play_Count)+log(Popular_Videos)+Trend+danceability+energy+speechiness+liveness+valence+tempo+log(song_age+39)+artist_popularity, data = latest_data, weights = w)
summary(latest_lm_w)

par(mfrow = c(1,1))
plot(latest_lm_w)
par(mfrow = c(1,1))
hist(latest_lm_w$residuals, main = "Histogram of Weighted Model Residuals", xlab = "Residuals")
ols_test_normality(latest_lm_w)
ols_test_breusch_pagan(latest_lm_w, rhs = TRUE)
bptest(latest_lm_w, studentize = FALSE)
ols_plot_comp_plus_resid(latest_lm_w)

# AUTOCORRELATION
durbinWatsonTest(latest_lm_w)

# MULTICOLLINEARITY
## VIF
vif(latest_lm_w)