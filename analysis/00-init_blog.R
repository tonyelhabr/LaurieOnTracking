
# extrafont::font_import(prompt = FALSE, pattern = 'Fira')
# extrafont::loadfonts(device = 'win', quiet = TRUE)
library(tidyverse)
game_id <- 2
events <- import_event_data(game_id = game_id, postprocess = TRUE)
tracking_home <- import_tracking_data_timed(game_id = game_id, side = 'home', overwrite = F)
tracking_away <- import_tracking_data_timed(game_id = game_id, side = 'away', overwrite = F)
tracking <- bind_rows(tracking_home, tracking_away)
tracking

.event_id <- 823L
events_filt <-
  events %>%
  filter(event_id == .event_id)
events_filt

.clip_tracking <- function(tracking) {
  tracking %>% 
    select(-period, -time, -team) %>% 
    mutate(across(where(is.double), ~round(.x, 3))) %>% 
    clipr::write_clip()
}

tracking_start <-
  tracking %>%
  inner_join(events_filt %>% select(frame = start_frame))
tracking_start

tracking_end <-
  tracking %>%
  inner_join(events_filt %>% select(frame = end_frame))
tracking_end

events_filt %>% 
  select(event_id, side, type, start_frame, end_frame, matches('[xy]$')) %>% 
  clipr::write_clip()
tracking_start %>% .clip_tracking()
tracking_end %>% .clip_tracking()