
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
           params = get_default_model_params()) {
    
    if(FALSE) {
      ball_x <- tracking[1, ][['ball_x']]
      ball_y <- tracking[1, ][['ball_y']]
      target_x <- ball_x
      target_y <- ball_y
      params = get_default_model_params()
      players <-
        tracking %>%
        pull(player) %>%
        map(~player(player_id = .x, start_frame = .frame, events = events, tracking = tracking))
    }
    
    ball_dist <- .norm(target_x, ball_x, target_y, ball_y)
    ball_time <- ball_dist / params[['average_ball_speed']]
    
    # pass_team <- event[['team']] %>% tolower()
    ps_att <- players %>% keep(~{field(.x, 'is_attack')})
    ps_def <- players %>% keep(~{!field(.x, 'is_attack')})
    
    if(FALSE) {
      ip <- 6
      x <- ps_att[[ip]]
      ri <- field(x, 'reaction_time')
      rx1 <- field(x, 'x') + field(x, 'x_v') * ri
      ry1 <- field(x, 'y') + field(x, 'y_v') * ri
      rx2 <- target_x
      ry2 <- target_y
      num <- .norm(rx1, rx2, ry1, ry2)
      num
      tti <- ri + .norm(rx1, rx2, ry1, ry2) / field(x, 'vmax')
      tti
      .update_tti(ps_att[[ip]]) <- tti
      x <- ps_att[[ip]]
      t <- 5
      den_term <- -pi / sqrt(3) / field(x, 'tti_sigma') * (t - field(x, 'tti'))
      den_term
      den <- 1 + exp(den_term)
      p_intercept <- 1 / den
      p_intercept
    }
    
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
      # res <- v %>% map_dbl(~.tti(..1, rx2 = target_x, ry2 = target_y)) %>% min(na.rm = TRUE)
      res <- v %>% map_dbl(~field(..1, 'tti')) %>% min(na.rm = TRUE)
      res
    }
    
    tau_min_att <- ps_att %>% f_tau_min()
    tau_min_def <- ps_def %>% f_tau_min()
    
    t_def <- params[['time_to_control_def']]
    is_gt <- ifelse(tau_min_att - max(ball_time, tau_min_def) >= t_def, TRUE, FALSE)
    if(is_gt) {
      # browser()
      message('`ppcf_def = 1` automatically because no defenders are sufficiently close.')
      res <- list('ppcf_att' = 0, 'ppcf_def' = 1)
      # res <- list('ppcf_att' = 0, 'ppcf_def' = 0)
      return(res)
    }
    
    t_att <- params[['time_to_control_att']]
    is_gt <- ifelse(tau_min_def - max(ball_time, tau_min_att) >= t_att, TRUE, FALSE)
    if(is_gt) {
      # browser()
      message('`ppcf_att = 1` automatically because no attackers are sufficiently close.')
      res <- list('ppcf_att' = 1, 'ppcf_def' = 0)
      # res <- list('ppcf_att' = 0, 'ppcf_def' = 0)
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
    
    # # debug
    # t <- dt_seq[i]
    # ip <- 1
    # p <- ps_att_filt[[ip]]
    # dppcf_dt <- (1 - ppcf_att[i - 1] - ppcf_def[i - 1]) * .p_intercept(p, t) * field(p, 'lambda_att')
    # .update_ppcf(p) <- dppcf_dt * int_dt
    # ppcf_att[i] <- ppcf_att[i] + field(p, 'ppcf')
    
    dt_seq <- seq(ball_time - int_dt, ball_time + params[['max_int_time']], by = int_dt)
    n_seq <- dt_seq %>% length()
    ppcf_att <- rep(0, n_seq)
    ppcf_def <- rep(0, n_seq)
    p_tot <- 0
    # i <- 2
    i <- 2
    limit_hi <- 1 
    limit_lo <- 0
    
    while (((1 - p_tot) > params[['model_converge_tol']]) & (i < n_seq)) {
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
      if(i >= 5) {
        ppcf_i <- ppcf_att[i] + ppcf_def[i]
        ppcf_att[i] <- ppcf_att[i] / ppcf_i
        ppcf_def[i] <- ppcf_def[i] / ppcf_i
      }
      
      p_tot <- ppcf_att[i] + ppcf_def[i]
      i <- i + 1
      
    }
    # cat(sprintf('`i = %d`, `ppcf_def[i - 1] = %.3f`, `ppcf_att[i - 1] = %.3f`, `p_tot = %.3f`', i, ppcf_def[i - 1], ppcf_att[i - 1], p_tot), sep = '\n')
    # ppcf_att[1:i] %>% round(3) %>% print()
    # ppcf_def[1:i] %>% round(3) %>% print()
    if(i >= n_seq) {
      warning(sprintf('Integration failed to converge: `p_tot = %.3f`', p_tot), call. = FALSE)
    }
    
    i_last <- i - 1
    f_assert_ppcf(ppcf_att[i_last], i, is_attack = TRUE)
    f_assert_ppcf(ppcf_def[i_last], i, is_attack = FALSE)

    i_seq <- 1:i_last
    # browser()
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
  function(tracking, events, event_id, params = get_default_model_params(), epv_grid = import_epv_grid(), n_cell_x = 50L, n_cell_y = 50L) {
    
    if(FALSE) {

      params = get_default_model_params()
      epv_grid = import_epv_grid()
      event_id = 823L
      n_cell_x = 50L
      n_cell_y = 32L
    }
    
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
    # n_cell_y <- n_cell_x * dims[2] / dims[1]
    # n_cell_y
    dx <- dims[1] / n_cell_x
    dy <- dims[2] / n_cell_y
    
    grid_pc <-
      crossing(
        x = seq(0 + dx / 2, dims[1], length.out = n_cell_x),
        y = seq(0 + dy / 2, dims[2], length.out = n_cell_y)
    )
    
    ball_x <- tracking1[1, ][['ball_x']]
    ball_y <- tracking1[1, ][['ball_y']]
    # target_x <- events1[1, ][['start_x']]
    # target_y <- events1[1, ][['start_y']]
    # target_x <- ball_x
    # target_y <- ball_y
    f <- quietly(calculate_pc_at_target)
    # f <- safely(calculate_pc_at_target, otherwise = list(), quiet = FALSE)
    # ff <- .time_it(f)
    do <- function() {
      res <-
        grid_pc %>% 
        # sample_n(10) %>% 
        # sample_frac(0.1) %>% 
        mutate(
          pc = 
            map2(x, y, 
                 ~f(
                   players = players,
                   ball_x = ball_x,
                   ball_y = ball_y,
                   target_x = ..1,
                   target_y = ..2
                 )
            )
        ) %>% 
        mutate(
          res = map(pc, ~pluck(.x, 'result'))
        ) %>% 
        mutate(
          ppcf_att = map_dbl(res, ~pluck(.x, 'ppcf_att')),
          ppcf_def = map_dbl(res, ~pluck(.x, 'ppcf_def'))
        ) %>% 
        # select(-pc, -res) %>% 
        arrange(x, y)
      res
    }
    
    do_timed <- .time_it(do, .name = 'calculate pc for event')
    pc <- do_timed()
    pc
  }

