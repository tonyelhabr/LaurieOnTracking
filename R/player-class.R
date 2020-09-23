
# library(vctrs)

new_player <-
  function(player_id = integer(),
           is_gk = logical(),
           side = character(),
           is_attack = logical(),
           tti = double(),
           in_frame = logical(),
           x = double(),
           y = double(),
           x_v = double(),
           y_v = double(),
           vmax = double(),
           reaction_time = double(),
           tti_sigma = double(),
           lambda_att = double(),
           lambda_def = double()) {
    
    # I like using quotes for the names of list elements because they technically don't exist in the list before hand. Quotes implicitly imply that we are creating something new.
    player <-
      vctrs::new_rcrd(
        list(
          'player_id' = player_id,
          'is_gk' = is_gk,
          'side' = side,
          'is_attack' = is_attack,
          'tti' = tti,
          'in_frame' = in_frame,
          'name' = sprintf('%s_%s', side, player_id),
          'x' = x,
          'y' = y,
          'x_v' = x_v,
          'y_v' = y_v,
          'vmax' = vmax,
          'reaction_time' = reaction_time,
          'tti_sigma' = tti_sigma,
          'lambda_att' = lambda_att,
          'lambda_def' = lambda_def,
          'ppcf' = 0
        ),
        class = 'player'
      )
    player
  }

validate_player <- function(player) {
  
  vctrs::vec_assert(vctrs::field(player, 'player_id'), integer())
  side <- vctrs::field(player, 'side')
  vctrs::vec_assert(side, character())
  .validate_side(side)
  vctrs::vec_assert(vctrs::field(player, 'is_attack'), logical())
  vctrs::vec_assert(vctrs::field(player, 'tti'), double())
  vctrs::vec_assert(vctrs::field(player, 'in_frame'), logical())
  vctrs::vec_assert(vctrs::field(player, 'x'), double())
  vctrs::vec_assert(vctrs::field(player, 'y'), double())
  vctrs::vec_assert(vctrs::field(player, 'x_v'), double())
  vctrs::vec_assert(vctrs::field(player, 'y_v'), double())
  vctrs::vec_assert(vctrs::field(player, 'vmax'), double())
  vctrs::vec_assert(vctrs::field(player, 'reaction_time'), double())
  vctrs::vec_assert(vctrs::field(player, 'tti_sigma'), double())
  vctrs::vec_assert(vctrs::field(player, 'lambda_att'), double())
  vctrs::vec_assert(vctrs::field(player, 'lambda_def'), double())
  player
}

player <- 
  function(player_id,
           frame,
           events,
           tracking,
           params = .get_default_pc_params(),
           gk_ids = pull_gk_ids(tracking)) {
    
    
    player_id <- as.integer(player_id)
    frame <- as.integer(frame)
    
    vctrs::vec_assert(params, list())
    nms_req <- c('max_player_speed', 'reaction_time', 'tti_sigma', 'lambda_att', 'lambda_def')
    assertthat::assert_that(all(nms_req %in% names(params)))
    # TODO: Implement checks for the data types of the columns. (Even though stronger checking is done in `new_player()`, the user may pass in a "malformed" params that has the required names but whose key-value pairs don't have the correct types.
    
    assertthat::assert_that(is.data.frame(events))
    nms_req <- c('start_frame', 'side')
    assertthat::assert_that(all(nms_req %in% names(events)))
    
    event_filt <- events %>% filter(start_frame == !!frame)
    assertthat::assert_that(nrow(event_filt) == 1L)
    
    assertthat::assert_that(is.data.frame(tracking))
    nms_req <- c('frame', 'player_id', 'side', 'x', 'y', 'x_v', 'y_v')
    assertthat::assert_that(all(nms_req %in% names(tracking)))
    
    tracking_filt <- tracking %>% filter(frame == !!frame, player_id == !!player_id)
    assertthat::assert_that(nrow(tracking_filt) == 1L)
    
    side <- tracking_filt[['side']]
    
    assertthat::assert_that(
      length(gk_ids) == 2, 
      identical(sort(names(gk_ids)), c('away', 'home'))
    )
    is_gk <- any(player_id %in% gk_ids)
    
    x <- tracking_filt[['x']]
    y <- tracking_filt[['y']]
    player <-
      new_player(
        player_id = player_id,
        is_gk = is_gk,
        is_attack = side == event_filt[['side']],
        tti = -1,
        in_frame = !is.na(x) & !is.na(y),
        side = side,
        x = x,
        y = y,
        x_v = tracking_filt[['x_v']],
        y_v = tracking_filt[['y_v']],
        vmax = params[['max_player_speed']],
        reaction_time = params[['reaction_time']],
        tti_sigma = params[['tti_sigma']],
        lambda_att = params[['lambda_att']],
        lambda_def = params[['lambda_def']]
      )
    player <- validate_player(player)
    player
  }

# print ----
format.player <- function(player, ...) {
  if(vctrs::field(player, 'in_frame')) {
    suffix <- sprintf('with `position = (%.2f, %.2f)` and `velocity = <%.1f, %.1f>`', vctrs::field(player, 'player_id'), vctrs::field(player, 'y'), vctrs::field(player, 'x_v'), vctrs::field(player, 'y_v'))
  } else {
    suffix <- 'is not on the pitch'
  }
  prefix <- sprintf('`player_id = %s` ', vctrs::field(player, 'player_id'))
  msg <- sprintf('%s%s', prefix, suffix)
  paste(msg, sep = '\n')
}

obj_print_data.player <- function(player) {
  cat(format(player), sep = '\n')
}

# tti ----
# .norm <- function(x1, x2, y1, y2) {
#   res <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
#   res <- matrix(c(x1, y1, x2, y2), nrow = 2, byrow = TRUE)
#   res
# }
.norm <- function(x1, x2, y1, y2) {
  m <- matrix(c(x1, y1)) - matrix(c(x2, y2))
  res <- sqrt(sum(m^2))
  res
}

.get_tti.player <- function(player, x2, y2, ...) {
  ri <- vctrs::field(player, 'reaction_time')
  x1 <- vctrs::field(player, 'x') + vctrs::field(player, 'x_v') * ri
  y1 <- vctrs::field(player, 'y') + vctrs::field(player, 'y_v') * ri
  res <- ri + .norm(x1, x2, y1, y2) / vctrs::field(player, 'vmax')
  res
}

.msg_cls_err <- function(player, f) {
  cls <- class(player)[1]
  sprintf('`%s()` doesn\'t know how to handle class `%s`!', f, cls) 
}

.get_tti.default <- function(player, ...) {
  stop(.msg_cls_err(player, '.get_tti'), call. = FALSE)
}

.get_tti <- function(player, ...) {
  UseMethod('.get_tti')
}

`.set_tti<-.player` <- function(player, value) {
  vctrs::field(player, 'tti') <- value
  player
}

`.set_tti<-.default` <- function(player, ...) {
  stop(.msg_cls_err(player, '.set_tti'), call. = FALSE)
}

`.set_tti<-` <- function(player, ...) {
  UseMethod('.set_tti<-')
}

# p_intercept ----
.get_p_intercept.player <- function(player, t, ...) {
  den <- 1 + exp(-base::pi / sqrt(3) / vctrs::field(player, 'tti_sigma') * (t - vctrs::field(player, 'tti')))
  res <- 1 / den
  # assertthat::assert_that(res > 0, msg = sprintf('Probability to intercept (`%.2f`) cannot be < 0.', res))
  res
}

.get_p_intercept.default <- function(player, ...) {
  stop(.msg_cls_err(player, '.get_p_intercept'), call. = FALSE)
}

.get_p_intercept <- function(player, ...) {
  UseMethod('.get_p_intercept')
}

`.set_ppcf<-.player` <- function(player, value) {
  vctrs::field(player, 'p_intercept') <- value
  player
}

`.set_p_intercept<-.default` <- function(player, ...) {
  stop(.msg_cls_err(player, '.set_p_intercept'), call. = FALSE)
}

`.set_p_intercept<-` <- function(player, ...) {
  UseMethod('.set_p_intercept<-')
}

# ppcf ----
`.set_ppcf<-.player` <- function(player, value) {
  vctrs::field(player, 'ppcf') <- value
  player
}

`.set_ppcf<-.default` <- function(player, ...) {
  stop(.msg_cls_err(player, '.set_ppcf'), call. = FALSE)
}

`.set_ppcf<-` <- function(player, ...) {
  UseMethod('.set_ppcf<-')
}

# pc-assert ----
.assert_dppcf_dt <- function(dppcf_dt, i, is_attack = TRUE) {
  cnd <- dppcf_dt >= 0
  if(cnd) {
    return(invisible())
  }
  prefix <- ifelse(is_attack, 'attack', 'defend')
  msg <- sprintf('Incremental %sing player probability (`dppcf_dt = %.3f` at `i = %d`) must be >= 0', prefix, dppcf_dt, i)
  warning(msg, call. = FALSE)
}

.assert_ppcf <- function(ppcf, i, is_attack = TRUE) {
  cnd <- ppcf >= 0 & ppcf <= 1
  if(cnd) {
    return(invisible())
  }
  prefix <- ifelse(is_attack, 'Attack', 'Defend')
  msg <- sprintf('%sing player probability (`ppcf = %.3f` at `i = %d`) must be >= 0 and <= 1', prefix, ppcf, i)
  warning(msg, call. = FALSE)
}

.truncate01 <- function(x) {
  case_when(
    x >= 1 ~ 1,
    x <= 0 ~ 0,
    TRUE ~ x
  )
}

# `target_[xy]` will be equal to `ball_[xy]` when just calculating pitch control only at the ball.
# We will end up calculating pitch control for the entire pitch for a single frame.
calculate_pc_at_target <-
  function(players,
           ball_x,
           ball_y,
           target_x = ball_x,
           target_y = ball_y,
           params = .get_default_pc_params()) {
    
    ball_dist <- .norm(target_x, ball_x, target_y, ball_y)
    ball_time <- ball_dist / params[['average_ball_speed']]
    
    ps_att <- players %>% keep(~{vctrs::field(.x, 'is_attack')})
    ps_def <- players %>% keep(~{!vctrs::field(.x, 'is_attack')})
    
    f_update_tti <- function(v) {
      # Don't think `map` works for updating a value in place, so need to use a `for` loop (yikes!)
      for(i in seq_along(v)) {
        # browser()
        value <- .get_tti(v[[i]], x2 = target_x, y2 = target_y)
        .set_tti(v[[i]]) <- value
      }
      invisible(v)
    }
    
    ps_att <- ps_att %>% f_update_tti()
    ps_def <- ps_def %>% f_update_tti()
    
    f_tau_min <- function(v) {
      v %>% map_dbl(~vctrs::field(..1, 'tti')) %>% min(na.rm = TRUE)
    }
    
    tau_min_att <- ps_att %>% f_tau_min()
    tau_min_def <- ps_def %>% f_tau_min()
    
    t_def <- params[['time_to_control_def']]
    is_gt <- ifelse(tau_min_att - max(ball_time, tau_min_def) >= t_def, TRUE, FALSE)
    if(is_gt) {
      # message('`ppcf_def = 1` automatically because no defenders are sufficiently close.')
      res <- list('ppcf_att' = 0, 'ppcf_def' = 1)
      return(res)
    }
    
    t_att <- params[['time_to_control_att']]
    is_gt <- ifelse(tau_min_def - max(ball_time, tau_min_att) >= t_att, TRUE, FALSE)
    if(is_gt) {
      # message('`ppcf_att = 1` automatically because no attackers are sufficiently close.')
      res <- list('ppcf_att' = 1, 'ppcf_def' = 0)
      return(res)
    }
    
    f_discard <- function(v, tau_min, t) {
      res <-
        v %>% 
        discard(~is.na(vctrs::field(..1, 'x'))) %>% 
        discard(~(vctrs::field(., 'tti') - tau_min) >= t)
      res
    }
    ps_att_filt <- ps_att %>% f_discard(tau_min = tau_min_att, t = t_att)
    ps_def_filt <- ps_def %>% f_discard(tau_min = tau_min_def, t = t_def)
    int_dt <- params[['int_dt']]
    
    dt_seq <- seq(ball_time - int_dt, ball_time + params[['max_int_time']], by = int_dt)
    n_seq <- dt_seq %>% length()
    ppcf_att <- rep(0, n_seq)
    ppcf_def <- rep(0, n_seq)
    p_tot <- 0
    i <- 2
    limit_hi <- 1 
    limit_lo <- 0
    
    while (((1 - p_tot) > params[['model_converge_tol']]) & (i < n_seq) & i < params[['iter_min']]) {
      t <- dt_seq[i]
      ii <- i - 1
      for(ip in seq_along(ps_att_filt)) {
        
        lhs <- 1 - ppcf_att[ii] - ppcf_def[ii]
        p <- ps_att_filt[[ip]]
        dppcf_dt <- lhs * .get_p_intercept(p, t) * vctrs::field(p, 'lambda_att')
        
        value_ip <- dppcf_dt * int_dt
        
        .assert_dppcf_dt(dppcf_dt, i = i, is_attack = TRUE)
        .set_ppcf(ps_att_filt[[ip]]) <- value_ip
        
        value_i <- ppcf_att[i] + vctrs::field(ps_att_filt[[ip]], 'ppcf')
        
        if(value_i >= limit_hi) {
          value_i <- limit_hi
        } else if(value_i <= limit_lo) {
          value_i <- limit_lo
        }
        
        ppcf_att[i] <- value_i
      }
      
      for(ip in seq_along(ps_def_filt)) {
        
        lhs <- 1 - ppcf_att[ii] - ppcf_def[ii]
        p <- ps_def_filt[[ip]]
        dppcf_dt <- lhs * .get_p_intercept(p, t) * vctrs::field(p, 'lambda_def')
        
        value_ip <- dppcf_dt * int_dt
        
        .assert_dppcf_dt(dppcf_dt, i = i, is_attack = FALSE)
        .set_ppcf(ps_def_filt[[ip]]) <- value_ip
        
        value_i <- ppcf_def[i] + vctrs::field(ps_def_filt[[ip]], 'ppcf')
        
        if(value_i >= limit_hi) {
          value_i <- limit_hi
        } else if(value_i <= limit_lo) {
          value_i <- limit_lo
        }
        
        ppcf_def[i] <- value_i
      }
      
      # "Normalize" to make sure the two sum up to 1.
      ppcf_i <- ppcf_att[i] + ppcf_def[i]
      ppcf_att[i] <- ppcf_att[i] / ppcf_i
      ppcf_def[i] <- ppcf_def[i] / ppcf_i
      
      p_tot <- ppcf_att[i] + ppcf_def[i]
      i <- i + 1
      
    }
    
    if(i >= n_seq) {
      warning(sprintf('Integration failed to converge: `p_tot = %.3f`', p_tot), call. = FALSE)
    }

    
    i_last <- i - 1
    ppcf_att[i_last] <- .truncate01(ppcf_att[i_last])
    ppcf_def[i_last] <- .truncate01(ppcf_def[i_last])
    .assert_ppcf(ppcf_att[i_last], i, is_attack = TRUE)
    .assert_ppcf(ppcf_def[i_last], i, is_attack = FALSE)
    
    i_seq <- 1:i_last
    res <-
      list(
        'ppcf_att' = ppcf_att[i_last], 
        'ppcf_def' = ppcf_def[i_last]
      )
    res
  }

do_calculate_pc_for_event <-
  function(tracking,
           events,
           event_id,
           params = .get_default_pc_params(),
           epv_grid = import_epv_grid()) {
    
    events_filt <- events %>% filter(event_id == !!event_id)
    start_frame <- events_filt[['start_frame']]
    tracking_filt <- tracking %>% filter(frame == start_frame)
    
    players <-
      tracking_filt %>%
      pull(player_id) %>%
      map(
        ~ player(
          player_id = .x,
          events = events_filt,
          tracking = tracking_filt,
          frame = start_frame,
          params = params
        )
      )
    players
    
    # dx <- dims[1] / n_cell_x
    # dy <- dims[2] / n_cell_y
    # 
    # if(geom == 'tile') {
    #   init_x <- 0 + dx / 2
    #   init_y <- 0 + dy / 2
    # } else if (geom == 'contour') {
    #   init_x <- 0
    #   init_y <- 0
    # }
    # # browser()
    # grid_pc <- 
    #   crossing(
    #     x = seq(init_x, dims[1], length.out = n_cell_x),
    #     y = seq(init_y, dims[2], length.out = n_cell_y)
    #   )
    grid <- epv_grid %>% distinct(x, y)
    ball_x <- tracking_filt[1, ][['ball_x']]
    ball_y <- tracking_filt[1, ][['ball_y']]

    f <- function() {
      res <-
        grid %>% 
        mutate(
          res = 
            map2(x, y, 
                 ~calculate_pc_at_target(
                   players = players,
                   ball_x = ball_x,
                   ball_y = ball_y,
                   target_x = ..1,
                   target_y = ..2
                 )
            )
        )
      res
    }
    f_timed <- .time_it(f, .name = 'calculating pitch control')
    pc_grid <-
      f_timed() %>% 
      mutate(
        ppcf_att = map_dbl(res, ~pluck(.x, 'ppcf_att')),
        ppcf_def = map_dbl(res, ~pluck(.x, 'ppcf_def'))
      ) %>% 
      select(-res) %>% 
      arrange(x, y)
    pc_grid
  }

# epv ----
.rescale <- function(x, rng1, rng2) {
  rng2[1] + ((x - rng1[1]) * (rng2[2] - rng2[1])) / (rng1[2] - rng1[1])
}

.tidy_epv_grid <- function(epv_grid) {
  res <-
    epv_grid %>% 
    mutate(row = row_number()) %>% 
    relocate(row) %>% 
    pivot_longer(-row, names_to = 'col')
  res
}

.get_epv_rng_orig <- function(dir = c('x', 'y')) {
  dir <- match.arg(dir)
  switch(dir, x = c(1L, 50L), y = c(1L, 32L))
}

.rescale_tidy_epv_grid <- function(tidy_epv_grid, dims = .get_dims_opta()) {
  x_rng_orig <- .get_epv_rng_orig('x')
  y_rng_orig <- .get_epv_rng_orig('y')
  res <-
    tidy_epv_grid %>% 
    mutate(
      # across(col, ~str_remove(.x, '^X') %>% as.integer() %>% .rescale(x_rng_orig, c(0L + dims[1] / x_rng_orig[2], dims[1]))),
      # across(row, ~.rescale(.x, y_rng_orig, c(0L + dims[2] / y_rng_orig[2], dims[2])))
      # across(col, ~str_remove(.x, '^X') %>% as.integer() %>% .rescale(x_rng_orig, c(0, dims[1] - dims[1] / x_rng_orig[2]))),
      # across(row, ~.rescale(.x, y_rng_orig, c(0, dims[2] - dims[2] / y_rng_orig[2])))
      across(col, ~str_remove(.x, '^X') %>% as.integer() %>% .rescale(x_rng_orig, c(0 + dims[1] / x_rng_orig[2] / 2, dims[1] - dims[1] / x_rng_orig[2] / 2))),
      across(row, ~.rescale(.x, y_rng_orig, c(0 + dims[2] / y_rng_orig[2] / 2, dims[2] - dims[2] / y_rng_orig[2] / 2)))
    ) %>% 
    rename(x = col, y = row)
  res
}

.add_grid_id_cols <- function(grid) {
  res <-
    grid %>% 
    mutate(idx = dense_rank(x), idy = dense_rank(y))
  res
}

import_epv_grid <- memoise::memoise({
  function(path = file.path('data', 'EPV_grid.csv')) {
    # res <- read_delim(path, delim = ',')
    # res <- read.table(path, sep = ',') %>% as.matrix()
    if(!fs::file_exists(path)) {
      path <- 'https://raw.githubusercontent.com/Friends-of-Tracking-Data-FoTD/LaurieOnTracking/master/EPV_grid.csv'
    }
    res <- 
      read_csv(path, col_names = FALSE) %>% 
      .tidy_epv_grid() %>% 
      .rescale_tidy_epv_grid() %>% 
      .add_grid_id_cols()
    res
  }
})

import_xt_grid <- memoise::memoise({
  function(path = file.path('data', 'xT.csv')) {
    if(!fs::file_exists(path)) {
      path <- 'https://raw.githubusercontent.com/anenglishgoat/InteractivePitchControl/master/xT.csv'
    }
    import_epv_grid(path = path)
  }
})

plot_epv_grid <- function(epv_grid = import_epv_grid(), attack_direction = 1, ...) {
  if(attack_direction == -1) {
    epv_grid <- epv_grid %>% mutate(across(value, ~.x * -1))
  }
  viz <-
    epv_grid %>% 
    aes(x = x, y = y) +
    .pitch_gg(...) +
    geom_raster(
      aes(fill = value), 
      interpolate = TRUE,
      hjust = 0.5,
      vjust = 0.5,
      alpha = 0.5
    ) +
    scale_fill_distiller(palette = 'Blues', direction = 1)
  viz
}


.filter_epv_grid <- function(epv_grid, x, y) {
  res <-
    epv_grid %>% 
    mutate(
      # dx = sqrt(x^2 + start_x^2),
      # dy = sqrt(y^2 + start_y^2)
      dx = abs(x - !!x),
      dy = abs(y - !!y)
    ) %>% 
    mutate(dz = sqrt(dx^2 + dy^2)) %>% 
    filter(dz == min(dz)) %>% 
    select(-dx, -dy, -dz)
  res
}

.compute_epv_added <- function(epv_grid, start_x, start_y, end_x, end_y) {
  epv_start <- epv_grid %>% .filter_epv_grid(start_x, start_y)
  epv_end <- epv_grid %>% .filter_epv_grid(end_x, end_y)
  value_start <- epv_start[['value']]
  value_end <- epv_end[['value']]
  list(
    'start' = value_start,
    'end' = value_end,
    'change' = value_end - value_start
  )
}

.do_compute_epv_added <- function(events, event_id, epv_grid) {
  events_filt <- events %>% filter(event_id == !!event_id)
  assertthat::assert_that(nrow(events_filt) == 1L)
  start_x <- events_filt[['start_x']]
  start_y <- events_filt[['start_y']]
  end_x <- events_filt[['end_x']]
  end_y <- events_filt[['end_y']]
  res <-
    .compute_epv_added(
      epv_grid = epv_grid,
      start_x = start_x,
      start_y = start_y,
      end_x = end_x,
      end_y = end_y
    )
  res
}

do_compute_epv_added <- function(..., epv_grid = import_epv_grid()) {
  do_compute_epv_added(..., epv_grid = epv_grid)
}

do_compute_xt_added <- function(..., epv_grid = import_xt_grid()) {
  do_compute_epv_added(..., epv_grid = epv_grid)
}

do_compute_eepv_added <- function(tracking, events, event_id, epv_grid = import_epv_grid(), params = .get_default_pc_params(), ...) {

  events_filt <- events %>% filter(event_id == !!event_id)
  start_frame <- events_filt[['start_frame']]
  end_frame <- events_filt[['end_frame']]
  tracking_start <- tracking %>% filter(frame == start_frame)
  tracking_end <- tracking %>% filter(frame == end_frame)
  
  players_start <-
    tracking_start %>%
    pull(player_id) %>%
    map(
      ~ player(
        player_id = .x,
        events = events_filt,
        tracking = tracking_start,
        frame = start_frame,
        params = params
      )
    )
  players_start
  
  ball_x_start <- tracking_start[1, ][['ball_x']]
  ball_y_start <- tracking_start[1, ][['ball_y']]
  
  pc_start <-
    calculate_pc_at_target(
      players = players_start,
      ball_x = ball_x_start,
      ball_y = ball_y_start,
      target_x = ball_x_start,
      target_y = ball_y_start
    )
  
  ball_x_end <- tracking_end[1, ][['ball_x']]
  ball_y_end <- tracking_end[1, ][['ball_y']]
  
  pc_end <-
    calculate_pc_at_target(
      players = players_start,
      ball_x = ball_x_start,
      ball_y = ball_y_start,
      target_x = ball_x_end,
      target_y = ball_y_end
    )

  # # Use the `tracking` ball position as start and end instead of the `event` columns since it is more precise.
  events_filt <- events %>% filter(event_id == !!event_id)
  assertthat::assert_that(nrow(events_filt) == 1L)
  # start_x <- events_filt[['start_x']]
  # start_y <- events_filt[['start_y']]
  # end_x <- events_filt[['end_x']]
  # end_y <- events_filt[['end_y']]
  start_x <- ball_x_start
  start_y <- ball_y_start
  end_x <- ball_x_end
  end_y <- ball_y_end
  
  epv_start <- epv_grid %>% .filter_epv_grid(start_x, start_y) %>% pluck('value')
  epv_end <- epv_grid %>% .filter_epv_grid(end_x, end_y) %>% pluck('value')
  ppcf_att_start <- pc_start %>% pluck('ppcf_att')
  ppcf_att_end <- pc_end %>% pluck('ppcf_att')
  eepv_start <- epv_start * ppcf_att_start
  eepv_end <- epv_end * ppcf_att_end
  res <-
    list(
      'epv_start' = epv_start,
      'epv_end' = epv_end,
      'ppcf_att_start' = ppcf_att_start,
      'ppcf_att_end' = ppcf_att_end,
      'eepv_start' = eepv_start,
      'eepv_end' = eepv_end,
      'eepv_added' = eepv_end - eepv_start
    )
  res
}

