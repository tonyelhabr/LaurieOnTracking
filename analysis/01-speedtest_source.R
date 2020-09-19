
.time_it <- function(f, ..., .name = NULL, .verbose = .get_verbose()) {
  
  if(is.null(.name)) {
    .name <- as.character(sys.call())
  }
  
  function(...) {
    time_1 <- Sys.time()
    .display_info('Starting {cli::bg_black(.name)} at {cli::bg_black(time_1)}.', .verbose = .verbose)
    res <- f(...)
    time_2 <- Sys.time()
    dur <- (time_2 - time_1) %>% lubridate::as.duration()
    dur_s <- dur %>% as.numeric('seconds') %>% scales::comma(accuracy = 0.1)
    dur_m <- dur %>% as.numeric('minutes') %>% scales::comma(accuracy = 0.1)
    parenth <- 
      ifelse(
        as.numeric(dur, 'seconds') >= 31L, 
        glue::glue(' (~{cli::bg_black(dur_m)} minutes)') %>% as.character(), 
        ''
      )
    .display_info('Finished {cli::bg_black(.name)} at {cli::bg_black(time_2)}. It took {cli::bg_black(dur_s)} seconds{parenth} to complete.', .verbose = .verbose)
    invisible(res)
  }
}

library(vctrs)

# TODO: Make a validation function!
new_player <-
  function(player_id = integer(),
           is_gk = logical(),
           side = character(),
           is_attack = logical(),
           tti = double(),
           in_frame = logical(),
           start_frame = integer(),
           x = double(),
           y = double(),
           x_v = double(),
           y_v = double(),
           vmax = double(),
           reaction_time = double(),
           tti_sigma = double(),
           lambda_att = double(),
           lambda_def = double()) {
    
    vctrs::vec_assert(player_id, integer())
    vctrs::vec_assert(side, character())
    .validate_side(side)
    vctrs::vec_assert(start_frame, integer())
    # TODO: More assertion.
    
    # browser()
    res <-
      vctrs::new_rcrd(
        # structure(
        list(
          'player_id' = player_id,
          'is_gk' = is_gk,
          'side' = side,
          'is_attack' = is_attack,
          'tti' = tti,
          'in_frame' = in_frame,
          'name' = sprintf('%s_%s', side, player_id),
          'start_frame' = start_frame,
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
    res
  }

player <- 
  function(player_id = 1L,
           # side = 'home',
           start_frame = 1L,
           events,
           tracking,
           params = get_default_pc_params()) {
    
    player_id <- as.integer(player_id)
    start_frame <- as.integer(start_frame)
    
    vctrs::vec_assert(params, list())
    nms_req <- c('max_player_speed', 'reaction_time', 'tti_sigma', 'lambda_att', 'lambda_def')
    assertthat::assert_that(all(nms_req %in% names(params)))
    # TODO: Implement checks for the data types. (Even though stronger checking is done in `new_player()`, the user may pass in a "malformed" params that has the required names but whose key-value pairs don't have the correct types.
    
    assertthat::assert_that(is.data.frame(events))
    nms_req <- c('start_frame', 'team')
    assertthat::assert_that(all(nms_req %in% names(events)))
    
    event <- events %>% filter(start_frame == !!start_frame)
    assertthat::assert_that(nrow(event) == 1L)
    
    assertthat::assert_that(is.data.frame(tracking))
    
    nms_req <- c('frame', 'player', 'side', 'x', 'y', 'x_v', 'y_v')
    assertthat::assert_that(all(nms_req %in% names(tracking)))
    
    frame <- tracking %>% filter(frame == !!start_frame, player == player_id)
    # browser()
    assertthat::assert_that(nrow(frame) == 1L)
    
    
    side <- frame[['side']]
    # .validate_side(side)
    
    gk_numbers <- tracking %>% pull_gk_numbers()
    # vctrs::vec_assert(gk_numbers, character()) # Probably overkill
    assertthat::assert_that(
      length(gk_numbers) == 2, 
      identical(sort(names(gk_numbers)), c('away', 'home'))
    )
    is_gk <- any(player_id %in% gk_numbers)
    
    res <-
      new_player(
        player_id = player_id,
        is_gk = is_gk,
        is_attack = side == tolower(event[['team']]),
        tti = -1,
        in_frame = TRUE,
        side = side,
        start_frame = start_frame,
        x = frame[['x']],
        y = frame[['y']],
        # pos = list('x' = frame[['x']], 'y' = frame[['y']]),
        x_v = frame[['x_v']],
        y_v = frame[['y_v']],
        # v = list('x' = frame[['x_v']], 'y' = frame[['y_v']]),
        vmax = params[['max_player_speed']],
        reaction_time = params[['reaction_time']],
        tti_sigma = params[['tti_sigma']],
        lambda_att = params[['lambda_att']],
        lambda_def = params[['lambda_def']]
      )
    res
  }

format.player <- function(x, ...) {
  info <- sprintf('`player_id = %s`%s on %s team (%s) is located at `(%.2f, %.2f)` with velocity = `<%.1f, %.1f>` and has `tti = %.2f` and `ppcf = %.3f`', field(x, 'player_id'), ifelse(field(x, 'is_gk'), ' (goalkeeper)', ''), field(x, 'side'), ifelse(field(x, 'is_attack'), 'attacking', 'defending'), field(x, 'x'), field(x, 'y'), field(x, 'x_v'), field(x, 'y_v'), field(x, 'tti'), field(x, 'ppcf'))
  # if(field(x, 'is_gk')) {
  #   info <- sprintf('%s. Player is the goalkeeper', info)
  # }
  # info <- sprintf('<player> %s located at (%s) with velocity <%s>', field(x, 'player_id'), field(x, 'pos'), field(x, 'v'))
  paste(info, sep = '\n')
  # print(format(info))
  # cat(info, sep = '\n')
  # invisible(x)
}

obj_print_data.player <- function(x) {
  cat(format(x), sep = '\n')
}

.norm_vec <- function(x) sqrt(sum(x^2))

# norm(matrix(c(1, 2, 4, 2.5), nrow = 2, byrow = TRUE))
.norm <- function(x1, x2, y1, y2) {
  # res <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  # res <- matrix(c(x1, y1, x2, y2), nrow = 2, byrow = TRUE)
  m <- matrix(c(x1, y1)) - matrix(c(x2, y2))
  res <- .norm_vec(m)
  res
}

.tti.player <- function(x, rx2, ry2, ...) {
  ri <- field(x, 'reaction_time')
  rx1 <- field(x, 'x') + field(x, 'x_v') * ri
  ry1 <- field(x, 'y') + field(x, 'y_v') * ri
  # rxd <- rx2 - rx1
  # ryd <- ry2 - ry1
  res <- ri + .norm(rx1, rx2, ry1, ry2) / field(x, 'vmax')
  res
}

.p_intercept.player <- function(x, t, ...) {
  den_term <- (-pi / sqrt(3) / field(x, 'tti_sigma')) * (t - field(x, 'tti'))
  den <- 1 + exp(den_term)
  res <- 1 / den
  # assertthat::assert_that(res > 0, msg = sprintf('Probability to intercept (`%.2f`) cannot be < 0.', res))
  # if(res < 0 | res > 1) {
  #   browser()
  # }
  res
}

`.update_tti<-.player` <- function(x, value) {
  field(x, 'tti') <- value
  x
}

# TODO!
# `.update_in_frame<-.player` <- function(x, value) {
#   field(x, 'in_frame') <- value
#   x
# }

`.update_ppcf<-.player` <- function(x, value) {
  field(x, 'ppcf') <- value
  x
}

.tti.default <- function(x, ...) {
  cls <- class(x)[1]
  err <- sprintf('`.tti()` doesn\'t know how to handle class `%s`!', cls) 
  stop(err, call. = FALSE)
}

.p_intercept.default <- function(x, ...) {
  cls <- class(x)[1]
  err <- sprintf('`.p_intercept()` doesn\'t know how to handle class `%s`!', cls) 
  stop(err, call. = FALSE)
}

`.update_tti<-default` <- function(x, ...) {
  cls <- class(x)[1]
  err <- sprintf('`.update_tti()` doesn\'t know how to handle class `%s`!', cls) 
  stop(err, call. = FALSE)
}

`.update_ppcf<-default` <- function(x, ...) {
  cls <- class(x)[1]
  err <- sprintf('`.update_ppcf()` doesn\'t know how to handle class `%s`!', cls) 
  stop(err, call. = FALSE)
}


.tti <- function(x, ...) {
  UseMethod('.tti')
}

.p_intercept <- function(x, ...) {
  UseMethod('.p_intercept')
}

`.update_tti<-` <- function(x, ...) {
  UseMethod('.update_tti<-')
}

`.update_ppcf<-` <- function(x, ...) {
  UseMethod('.update_ppcf<-')
}

f_assert_dppcf_dt <- function(dppcf_dt, i, is_attack = TRUE) {
  cnd <- dppcf_dt >= 0
  if(cnd) {
    return(invisible())
  }
  prefix <- ifelse(is_attack, 'attack', 'defend')
  msg <- sprintf('Incremental %sing player probability (`dppcf_dt = %.3f` at `i = %d`) must be >= 0', prefix, dppcf_dt, i)
  # assertthat::assert_that(dppcf_dt >= 0, msg = msg)
  warning(msg, call. = FALSE)
}
f_assert_ppcf <- function(ppcf, i, is_attack = TRUE) {
  cnd <- ppcf >= 0 & ppcf <= 1
  if(cnd) {
    return(invisible())
  }
  prefix <- ifelse(is_attack, 'Attack', 'Defend')
  msg <- sprintf('%sing player probability (`ppcf = %.3f` at `i = %d`) must be >= 0 and <= 1', prefix, ppcf, i)
  # assertthat::assert_that(cnd, msg = msg)
  warning(msg, call. = FALSE)
  # message(sprintf('Setting the %sing player probability to 1', prefix))
}

calculate_pc_at_target <-
  function(players,
           ball_x,
           ball_y,
           target_x = ball_x,
           target_y = ball_y,
           params = get_default_pc_params()) {

    ball_dist <- .norm(target_x, ball_x, target_y, ball_y)
    ball_time <- ball_dist / params[['average_ball_speed']]
    
    ps_att <- players %>% keep(~{field(.x, 'is_attack')})
    ps_def <- players %>% keep(~{!field(.x, 'is_attack')})

    f_update_tti <- function(v) {
      for(i in seq_along(v)) {
        value <- .tti(v[[i]], rx2 = target_x, ry2 = target_y)
        .update_tti(v[[i]]) <- value
      }
      invisible(v)
    }
    
    ps_att <- ps_att %>% f_update_tti()
    ps_att
    ps_def <- ps_def %>% f_update_tti()
    ps_def
    
    f_tau_min <- function(v) {
      res <- v %>% map_dbl(~field(..1, 'tti')) %>% min(na.rm = TRUE)
      res
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
        discard(~is.na(field(..1, 'x'))) %>% 
        discard(~(field(., 'tti') - tau_min) >= t)
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
    # i <- 2
    i <- 2
    limit_hi <- 1 
    limit_lo <- 0
    
    while (((1 - p_tot) > params[['model_converge_tol']]) & (i < n_seq) & i < 5) {
      t <- dt_seq[i]
      ii <- i - 1
      for(ip in seq_along(ps_att_filt)) {
        
        lhs <- 1 - ppcf_att[ii] - ppcf_def[ii]
        p <- ps_att_filt[[ip]]
        dppcf_dt <- lhs * .p_intercept(p, t) * field(p, 'lambda_att')
        
        value_ip <- dppcf_dt * int_dt
        
        f_assert_dppcf_dt(dppcf_dt, i = i, is_attack = TRUE)
        .update_ppcf(ps_att_filt[[ip]]) <- value_ip
        
        value_i <- ppcf_att[i] + field(ps_att_filt[[ip]], 'ppcf')
        
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
        dppcf_dt <- lhs * .p_intercept(p, t) * field(p, 'lambda_def')
        
        value_ip <- dppcf_dt * int_dt
        
        f_assert_dppcf_dt(dppcf_dt, i = i, is_attack = FALSE)
        .update_ppcf(ps_def_filt[[ip]]) <- value_ip
        
        value_i <- ppcf_def[i] + field(ps_def_filt[[ip]], 'ppcf')
        
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
    f_assert_ppcf(ppcf_att[i_last], i, is_attack = TRUE)
    f_assert_ppcf(ppcf_def[i_last], i, is_attack = FALSE)
    
    i_seq <- 1:i_last
    # browser()
    if(i > 2) {
      browser()
    }
    res <-
      list(
        'ppcf_att' = ppcf_att[i_last], 
        'ppcf_def' = ppcf_def[i_last],
        'i_last' = i_last,
        'p_tot' = p_tot,
        'ppcf_att_seq' = ppcf_att[i_seq],
        'ppcf_def_seq' = ppcf_def[i_seq]
      )
    res
  }

do_calculate_pc_for_event <-
  function(tracking, events, event_id, params = get_default_pc_params(), epv_grid = import_epv_grid(), n_cell_x = 50L, n_cell_y = 50L) {
    events1 <- events %>% filter(event_id == !!event_id)
    start_frame <- events1[['start_frame']]
    tracking1 <- tracking %>% filter(frame == start_frame)
    
    players <-
      tracking1 %>%
      pull(player) %>%
      map(
        ~ player(
          player_id = .x,
          events = events1,
          tracking = tracking1,
          start_frame = start_frame,
          params = params
        )
      )
    players
    
    dims <- .get_dims_opta()
    dx <- dims[1] / n_cell_x
    dy <- dims[2] / n_cell_y
    
    grid_pc <-
      crossing(
        x = seq(0 + dx / 2, dims[1], length.out = n_cell_x),
        y = seq(0 + dy / 2, dims[2], length.out = n_cell_y)
      )
    
    ball_x <- tracking1[1, ][['ball_x']]
    ball_y <- tracking1[1, ][['ball_y']]

    do <- function() {
      res <-
        grid_pc %>% 
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
        ) %>% 
        mutate(
          ppcf_att = map_dbl(res, ~pluck(.x, 'ppcf_att')),
          ppcf_def = map_dbl(res, ~pluck(.x, 'ppcf_def'))
        ) %>% 
        # select(-pc, -res) %>% 
        arrange(x, y)
      res
    }
    
    pc <- do()
    pc
  }


f <- function() {
  require(tidyverse)
  event_id <- 823L
  path_events1 <- fs::path('output', 'events1.csv')
  path_tracking1 <- fs::path('output', 'tracking1.csv')
  events1 <- path_events1 %>% read_csv() %>% mutate(across(c(event_id, matches('frame$')), as.integer))
  tracking1 <- path_tracking1 %>% read_csv() %>% mutate(across(c(period, frame, player), as.integer))
  pc <-
    do_calculate_pc_for_event(
      tracking = tracking1,
      events = events1,
      event_id = event_id
    )
  pc
}
