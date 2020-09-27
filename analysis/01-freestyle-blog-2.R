
event_ids <- c(823, 1754, 1664)

library(tidyverse)
game_id <- 2
events <- import_event_data(game_id = game_id, postprocess = TRUE)
tracking_home <- import_tracking_data_timed(game_id = game_id, side = 'home', overwrite = F)
tracking_away <- import_tracking_data_timed(game_id = game_id, side = 'away', overwrite = F)
tracking <- bind_rows(tracking_home, tracking_away)
tracking


.add_lead_cols <- function(tracking) {
  res <-
    tracking %>% 
    group_by(player_id) %>% 
    mutate(
      across(c(x, y, time), ~dplyr::lead(.x, 1L), .names = 'next_{col}')
    ) %>% 
    ungroup()
  res
}

do_compare <- 
  function(event_id, 
           events,
           tracking,
           pitch_grid_fb = .get_pitch_grid(n_cell_x = 100L, n_cell_y = 100L),
           pitch_grid_spearman = import_epv_grid() %>% select(x, y)) {
    
    events_filt <-
      events %>%
      filter(event_id == !!event_id)
    
    tracking_start <-
      tracking_filt %>%
      .add_lead_cols() %>% 
      inner_join(events_filt %>% select(frame = start_frame))
    
    # TODO: Put this in a function.
    pc_fb <-
      tracking_start %>% 
      # Drop the players who aren't on the field.
      drop_na() %>% 
      mutate(
        speed_x = .get_speed(x, next_x, time, next_time),
        speed_y = .get_speed(y, next_y, time, next_time),
        srat = .get_srat(speed_x, speed_y),
        theta = .get_theta(speed_x, speed_y),
        mu_x = .get_mu(x, speed_x),
        mu_y = .get_mu(y, speed_y),
        ri = .get_ri(x, y, ball_x, ball_y),
        R = map(theta, .get_R),
        S = map2(ri, srat, .get_S),
        Sigma = map2(R, S, .get_Sigma),
        I = pmap(list(x, y, mu_x, mu_y, Sigma), .calculate_I, pitch_grid = !!pitch_grid_fb)
      ) %>% 
      select(any_of(names(tracking_start)), I)
    pc_fb
    
    pc_agg_fb <-
      pc_fb %>% 
      select(frame, time, player_id, side, player_x = x, player_y = y, I) %>% 
      mutate(pitch_grid = list(!!pitch_grid_fb)) %>% 
      unnest(cols = c(I, pitch_grid)) %>% 
      group_by(frame, time, side, x, y) %>%
      summarise(side_sum = sum(I, na.rm = TRUE)) %>%
      ungroup() %>%
      pivot_wider(names_from = side, values_from = side_sum) %>% 
      # No real reason. Just makes the numbers easer to look at.
      mutate(
        value = (away / (home + away)) %>% round(3)
      )
    pc_agg_fb
    
    pc_agg_spearman <-
      do_calculate_pc_for_event(
        tracking = tracking_start,
        events = events_filt %>% mutate(frame = start_frame),
        event_id = .event_id,
        epv_grid = pitch_grid_spearman
      ) %>% 
      select(x, y, value = ppcf_att)
    pc_agg_spearman
    
    df <- pc_grid_epv_start
    rec <- 
      df %>% 
      recipes::recipe(formula(value ~ x + y), data = .)
    rec
    
    spec <-
      parsnip::nearest_neighbor(neighbors = 8) %>% 
      # parsnip::svm_rbf(mode = 'regression') %>% 
      parsnip::set_engine('kknn')
    spec
    
    wf <-
      workflows::workflow() %>%
      workflows::add_recipe(rec) %>%
      workflows::add_model(spec)
    wf
    
    fit <-
      parsnip::fit(wf, df) %>% 
      workflows::pull_workflow_fit()
    fit
    
    pred_spearman <- 
      fit %>% 
      predict(new_data = pitch_grid_fb) %>%
      rename(value = .pred) %>% 
      bind_cols(pitch_grid_fb)
    pred_spearman
    
    res <-
      full_join(
        pc_agg_fb %>% rename(value_fb = value), 
        pred_spearman %>% rename(value_spearman = value)
      )
    res
  }