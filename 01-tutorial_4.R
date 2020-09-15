
library(tidyverse)
game_id <- 2

events <- import_event_data(game_id = game_id, postprocess = TRUE)
tracking_home <- import_tracking_data_timed(game_id = game_id, side = 'home', overwrite = T)
tracking_away <- import_tracking_data_timed(game_id = game_id, side = 'away', overwrite = T)
tracking <- bind_rows(tracking_home, tracking_away)
params <- get_default_model_params()
params
tracking %>% skimr::skim()

gk_numbers <- tracking %>% pull_gk_numbers()
home_attack_direction <- tracking %>% pull_home_attack_direction(gk_numbers = gk_numbers)

epv_grid <- import_epv_grid()
epv_grid
viz_epv <- epv_grid %>% plot_epv()
viz_epv
# event_id <- 822
# event <-
#   events %>% 
#   filter(event_id == !!event_id)
# event
# start_frame <- event$start_frame
# end_frame <- event$end_frame
# pass_team <- event$team
# 
# players <- tracking %>% filter(frame == !!start_frame)
# players_att <- players %>% filter(side == !!pass_team)
# players_def <- players %>% filter(side != !!pass_team)
# ball_pos <- players %>% slice(1) %>% select(ball_x, ball_y) %>% deframe()
# ball_pos

# calculate_pitch_contral_at_target <-
#   function(target_x, target_y, ball_x, ball_y, attacking_players, defending_players, params = get_default_model_params()) {
#     # ball_travel_time <- sqrt(target_position - ball_start_pos)
#     ball_dist <- sqrt((target_x - ball_x) ^ 2 + (target_y - ball_y)^2)
#     ball_speed <- ball_dist / params[['average_ball_speed']]
#     tau_min_att <- 
#   }

