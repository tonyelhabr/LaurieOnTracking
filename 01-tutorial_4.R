
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
# viz_epv <- epv_grid %>% plot_epv()
# viz_epv
