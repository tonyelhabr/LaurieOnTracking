
library(tidyverse)
dir_data <- 'data'
game_id <- 2

events <- read_event_data(dir_data, game_id, postprocess = TRUE)
tracking_home <- read_tracking_data(dir_data, game_id, side = 'home', postprocess = TRUE)
tracking_away <- read_tracking_data(dir_data, game_id, side = 'away', postprocess = TRUE)
events
tracking_home
tracking_away
tracking_home <- tracking_home %>% add_player_velocities()
tracking_away <- tracking_away %>% add_player_velocities()

params <- get_default_model_params()
params
gk_numbers <- map_chr(list(tracking_home, tracking_away), find_goalkeeper)
gk_numbers
home_attack_direction <- tracking_home %>% find_playing_direction(str_subset(gk_numbers, '^home'))
home_attack_direction
epv_grid <- read_epv_grid()
epv_grid
viz_epv <- epv_grid %>% plot_epv()
viz_epv

tracking <- full_join(tracking_home, tracking_away)
tracking
tracking_long <- tracking %>% pivot_longer(-c(period, frame, time, ball_x, ball_y))