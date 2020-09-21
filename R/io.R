
.get_dir_data <- function() {
  'data'
}

.get_dir_output <- function() {
  'output'
}

.to_opta_coordinates <- function(data, dims = .get_dims_opta()) {
  res <-
    data %>%
    mutate(
      across(matches('x$'), ~{.x * dims[1]}),
      # across(matches('y$'), ~{.x * dims[2]})
      across(matches('y$'), ~{-1 * (.x * dims[2] - dims[2])})
    )
  res
}

.to_single_player_direction <- function(data) {
  res <- data %>% mutate(across(matches('^[x|y]$'), ~if_else(period == 2, -1 * .x, .x)))
  res
}

import_event_data <- function(game_id, dir = .get_dir_data(), postprocess = FALSE) {
  path <- fs::path(dir, glue::glue('Sample_Game_{game_id}'), glue::glue('Sample_Game_{game_id}_RawEventsData.csv'))
  res <- path %>% read_csv() %>% janitor::clean_names()
  if(!postprocess) {
    return(res)
  }
  res <- 
    res %>% 
    .to_opta_coordinates() %>% 
    .to_single_player_direction() %>% 
    mutate(across(matches('^(start|end)_[xy]$'), ~if_else(is.nan(.x), NA_real_, .x))) %>% 
    fill(start_x, start_y, .direction = 'downup') %>% 
    mutate(
      across(end_x, ~coalesce(.x, start_x)),
      across(end_y, ~coalesce(.x, start_y)),
      across(c(team, type, subtype), tolower),
      across(c(period, matches('(frame|_s)$')), as.integer),
      across(c(from, to), ~str_remove(.x, 'Player') %>% as.integer())
    ) %>% 
    rename(side = team, from_player_id = from, to_player_id = to) %>% 
    mutate(event_id = row_number()) %>% 
    relocate(event_id)
  res
}

.toupper1 <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

# Reference: https://www.robert-hickman.eu/post/fall_back_in_to_space/
.fix_tracking_names <- function(data, side) {
  rgx <- '^X[0-9]*$'
  nms <- names(data)
  names(data)[grep(rgx, nms)-1] <- paste0(nms[grep(rgx, nms)-1], '_x')
  nms <- names(data)
  names(data)[grep(rgx, nms)] <- gsub('_x$', '_y', nms[grep(rgx, nms)-1])
  res <-
    data %>% 
    rename_with(tolower) %>% 
    rename_with(~str_replace(.x, 'player', sprintf('%s_', side)), matches('^player')) %>% 
    rename(time = 3)
  res
}

.add_velocity_cols <- 
  function(data, 
           max_speed = 12,
           smoothing = TRUE,
           smoothing_filter = c('movavg', 'savgol'),
           ...,
           .verbose = TRUE,
           .fl = 7,
           .forder = 1,
           .dorder = 0,
           .n = .fl,
           .type = 's') {
    smoothing_filter <- match.arg(smoothing_filter)
    res <- 
      data %>%
      # select(time, period, x := !!col_x_sym, y := !!col_y_sym) %>% 
      mutate(
        across(
          matches('^[xy]$'), 
          list(v = ~{(.x - dplyr::lag(.x)) / (time - dplyr::lag(time))})
        )
      )
    
    if(max_speed > 0) {
      res <-
        res %>% 
        mutate(raw_speed = sqrt(x_v^2 + y_v^2)) %>% 
        mutate(across(matches('^[xy]_v$'), ~if_else(raw_speed > !!max_speed, NA_real_, .x))) %>% 
        select(-raw_speed)
    }
    
    if(smoothing) {
      res <- res %>% group_by(period, side, player_id) 
      # TODO: Figure out why this isn't working.
      if(smoothing_filter == 'savgol') {
        
        res <-
          res %>% 
          mutate(across(matches('^[xy]_v$'), ~pracma::savgol(.x, fl = .fl, forder = .forder, dorder = .dorder)))
      } else if (smoothing_filter == 'movavg') {
        res <-
          res %>% 
          mutate(across(matches('^[xy]_v$'), ~pracma::movavg(.x, n = .n, type = .type)))
      }
      res <- res %>% ungroup()
    }
    res
  }

import_tracking_data <-
  function(game_id,
           side = .get_valid_sides(),
           dir = .get_dir_data(),
           postprocess = TRUE,
           ...,
           overwrite = TRUE,
           dir_export = .get_dir_output(),
           basename_export = glue::glue('{side}_{game_id}_tracking.fst'),
           path_export = fs::path(dir_export, basename_export)) {

    path_exists <- fs::file_exists(path_export)
    # browser()
    if(path_exists & !overwrite) {
      # return(rio::import(path_export))
      return(feather::read_feather(path_export))
    }
    
    side <- .validate_side(side)
    path <- fs::path(dir, glue::glue('Sample_Game_{game_id}'), glue::glue('Sample_Game_{game_id}_RawTrackingData_{.toupper1(side)}_Team.csv'))
    
    suppressWarnings(data <- read_csv(path, skip = 2))
    res_init <- data %>% .fix_tracking_names(side = side)

    if(!postprocess) {
      return(res_init)
    }
    
    res <-
      res_init %>%
      # head(10000) %>% 
      mutate(across(c(ball_x, ball_y), ~if_else(is.nan(.x), NA_real_, .x))) %>% 
      fill(ball_x, ball_y, .direction = 'downup') %>% 
      pivot_longer(cols = matches('^(home|away)')) %>%
      separate(name, into = c('side', 'player_id', 'coord'), sep = '_') %>%
      pivot_wider(names_from = coord, values_from = value) %>%
      mutate(
        across(c(x, y), ~if_else(is.nan(.x), NA_real_, .x)),
        across(c(period, frame, player_id), as.integer)
      ) %>% 
      mutate(team = !!side) %>% 
      arrange(player_id, frame) %>% 
      .to_opta_coordinates() %>% 
      .add_velocity_cols()
    feather::write_feather(res, path = path_export)
    res
  }

import_tracking_data_timed <- .time_it(import_tracking_data)

# TODO: Need to check how `pull_gk_numbers()` and  `pull_home_attack_direction()` are "needed".
# `pull_home_attack_direction()` is used in tutorial 4, but not by any function.
# `pull_gk_numbers()` is also used in tutorial 4, and it is used in the `player()` class function and `pull_home_attack_direction()` functions. It is certainly useful because it is needed for the player class.
pull_gk_ids <- memoise::memoise({
  function(tracking) {
    res <-
      tracking %>% 
      group_by(side) %>% 
      filter(frame == min(frame)) %>% 
      filter(abs(x) == max(abs(x), na.rm = TRUE)) %>% 
      ungroup() %>% 
      # This returns the player numbers as characters! Don't want that.
      pull(player_id, side)
    nms <- res %>% names()
    vals <- res %>% as.integer()
    res <- setNames(vals, nms)
    res
  }
})

pull_home_attack_direction <- function(tracking, gk_numbers = pull_gk_numbers(tracking)) {
  res <-
    tracking %>% 
    filter(side == 'home' & player == gk_numbers['home']) %>% 
    slice(1) %>% 
    pull(x) %>% 
    sign() %>% 
    {. * -1}
  res
}

