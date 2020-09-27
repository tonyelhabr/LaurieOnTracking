
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
  clipr::write_clip()%>% datapasta::tribble_paste()
tracking_start %>% .clip_tracking() %>% datapasta::tribble_paste()
tracking_end %>% .clip_tracking() %>% datapasta::tribble_paste()

# for 2nd post ----
tracking_filt <-
  tracking %>% 
  filter(between(frame, events_filt$start_frame, events_filt$end_frame + 1L))
tracking_filt
feather::write_feather(tracking_filt, fs::path(.get_dir_data(), 'tracking_filt.fst'))

.add_lead_cols <- function(tracking) {
  tracking %>% 
    group_by(player_id) %>% 
    mutate(
      across(c(x, y, time), ~dplyr::lead(.x, 1L), .names = 'next_{col}')
    ) %>% 
    ungroup()
}

.clip_tracking <- function(tracking) {
  tracking %>% 
    select(-period, -team) %>% 
    mutate(across(where(is.double), ~round(.x, 3))) %>% 
    clipr::write_clip()
}

tracking_start <-
  tracking_filt %>%
  .add_lead_cols() %>% 
  inner_join(events_filt %>% select(frame = start_frame))
tracking_start

tracking_end <-
  tracking_filt %>%
  .add_lead_cols() %>% 
  inner_join(events_filt %>% select(frame = end_frame))
tracking_end

tracking_start %>% .clip_tracking() # %>% datapasta::tribble_paste()
tracking_end %>% .clip_tracking() # %>% datapasta::tribble_paste()

