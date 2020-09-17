
.calculate_player_velocity <-
  function(data,
           player_prefix,
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
    col_x <- glue::glue('{player_prefix}_x')
    col_y <- glue::glue('{player_prefix}_y')
    nms <- data %>% names()
    cols_require <- c(col_x, col_y, 'time', 'period')
    assertthat::assert_that(all(cols_require %in% nms))
    
    smoothing_filter <- match.arg(smoothing_filter)
    
    .display_info('Calculating velocity for {player_prefix}', .verbose = .verbose)
    
    col_x_sym <- sym(col_x)
    col_y_sym <- sym(col_y)
    
    # res <- data %>% mutate(idx = row_number()) %>% relocate(idx)
    # idx_2h <- res %>% filter(period == 2) %>% pull(idx) %>% min()
    
    
    res <- 
      data %>%
      select(time, period, x := !!col_x_sym, y := !!col_y_sym) %>% 
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
        mutate(across(matches('^[xy]_v$'), ~if_else(raw_speed > max_speed, NA_real_, .x)))
    }
    
    if(smoothing) {
      res <- res %>% group_by(period) 
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
    
    res <- res %>% mutate(speed = sqrt(x_v^2 + y_v^2)) 
    
    col_vx_res <- glue::glue('{player_prefix}_vx')
    col_vy_res <- glue::glue('{player_prefix}_vy')
    col_speed_res <- glue::glue('{player_prefix}_speed')
    col_vx_res_sym <- sym(col_vx_res)
    col_vy_res_sym <- sym(col_vy_res)
    col_speed_res_sym <- sym(col_speed_res)
    res <-
      res %>% 
      select(!!col_vx_res_sym := x_v, !!col_vy_res_sym := y_v, !!col_speed_res_sym := speed)
    
    res
  }

add_player_velocities <- function(data, ...) {
  
  rgx <- .get_col_rgx_metrica()
  player_prefixes <-
    data %>% 
    names() %>% 
    str_subset(rgx) %>% 
    str_replace(rgx, '\\1_\\2') %>% 
    unique()
  
  res <-
    player_prefixes %>% 
    map_dfc(
      ~.calculate_player_velocity(
        data = data, 
        player_prefix = .x,
        ...
      )
    ) %>% 
    bind_cols(data, .)
  res
}


