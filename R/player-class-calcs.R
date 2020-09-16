
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

f_msg_ppcf_p <- function(p, dppcf_dt, i, is_attack = TRUE) {
  if(i >= 1) {
    return(invisible())
  }
  prefix <- ifelse(is_attack, 'Attack', 'Defend')
  msg <- sprintf('%sing `player_id = %d` ppcf change at `i = %d`: %.3f -> %.3f', prefix, field(p, 'player_id'), i, field(p, 'ppcf'), dppcf_dt * int_dt)
  cat(msg, sep = '\n')
}

f_msg_ppcf <- function(ppcf, p, i, is_attack = TRUE) {
  if(i >= 1) {
    return(invisible())
  }
  prefix <- ifelse(is_attack, 'Attack', 'Defend')
  msg <- sprintf('%sing `ppcf` change at `i = %d` due to `player_id = %d` (with `ppcf = %.3f`): %.3f -> %.3f', prefix, i, field(p, 'player_id'), field(p, 'ppcf'), ppcf[[i]], ppcf[[i]] + field(p, 'ppcf'))
  cat(msg, sep = '\n')
}

calculate_pitch_contral_at_target <-
  function(players, ball_x, ball_y, target_x = ball_x, target_y = ball_y, params = get_default_model_params()) {
    
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
      message('`ppcf_att = 1` automatically because no defenders are sufficiently close.')
      res <- list('ppcf_att' = 1, 'ppcf_def' = 0)
      return(res)
    }
    
    t_att <- params[['time_to_control_att']]
    is_gt <- ifelse(tau_min_def - max(ball_time, tau_min_att) >= t_att, TRUE, FALSE)
    if(is_gt) {
      message('`ppcf_def = 1` automatically because no attackers are sufficiently close.')
      res <- list('ppcf_att' = 0, 'ppcf_def' = 1)
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
    
    while (((1 - p_tot) > params[['model_converge_tol']]) & (i < n_seq)) {
      t <- dt_seq[i]
      for(ip in seq_along(ps_att_filt)) {
        lhs <- 1 - ppcf_att[i - 1] - ppcf_def[i - 1]
        p <- ps_att_filt[[ip]]
        dppcf_dt <- lhs * .p_intercept(p, t) * field(p, 'lambda_att')
        # dppcf_dt <- lhs * .p_intercept(ps_att_filt[[ip]], t) * field(ps_att_filt[[ip]], 'lambda_att')

        value <- dppcf_dt * int_dt
        if(value >= 0.9) {
          browser()
          value <- 0.9
        }
        f_assert_dppcf_dt(dppcf_dt, i = i, is_attack = TRUE)
        # f_msg_ppcf_p(p = p, dppcf_dt = dppcf_dt, i = i, is_attack = TRUE)
        .update_ppcf(ps_att_filt[[ip]]) <- value
        p <- ps_att_filt[[ip]]
        # f_msg_ppcf(ppcf = ppcf_att, p = p, i = i, is_attack = TRUE)
        value <- ppcf_att[i] + field(p, 'ppcf')
        if(value >= 0.9) {
          browser()
          value <- 0.9
        }
        ppcf_att[i] <- value
      }
      for(ip in seq_along(ps_def_filt)) {
        lhs <- 1 - ppcf_att[i - 1] - ppcf_def[i - 1]
        p <- ps_def_filt[[ip]]
        dppcf_dt <- lhs * .p_intercept(p, t) * field(p, 'lambda_def')
        
        value <- dppcf_dt * int_dt
        if(value >= 0.999) {
          browser()
          value <- 0.999
        }
        f_assert_dppcf_dt(dppcf_dt, i = i, is_attack = FALSE)
        # f_msg_ppcf_p(p = p, dppcf_dt = dppcf_dt, i = i, is_attack = FALSE)
        .update_ppcf(ps_def_filt[[ip]]) <- value
        p <- ps_def_filt[[ip]]
        # f_msg_ppcf(ppcf = ppcf_def, p = p, i = i, is_attack = FALSE)
        value <- ppcf_def[i] + field(p, 'ppcf')
        if(value >= 0.999) {
          browser()
          value <- 0.999
        }
        ppcf_def[i] <- value
      }
      p_tot <- ppcf_def[i] + ppcf_att[i]
      i <- i + 1
    }
    # cat(sprintf('`i = %d`, `ppcf_def[i - 1] = %.3f`, `ppcf_att[i - 1] = %.3f`, `p_tot = %.3f`', i, ppcf_def[i - 1], ppcf_att[i - 1], p_tot), sep = '\n')
    # ppcf_att[1:i] %>% round(3) %>% print()
    # ppcf_def[1:i] %>% round(3) %>% print()
    if(i >= n_seq) {
      warning(sprintf('Integration failed to converge: %.3f', p_tot), call. = FALSE)
    }
    
    i_last <- i - 1
    f_assert_ppcf(ppcf_att[i_last], i, is_attack = TRUE)
    f_assert_ppcf(ppcf_def[i_last], i, is_attack = FALSE)

    i_seq <- 1:i_last
    # browser()
    res <-
      list(
        'i_last' = i_last,
        'p_tot' = p_tot,
        'ppcf_att' = ppcf_att[i_last], 
        'ppcf_def' = ppcf_def[i_last],
        'ppcf_att_seq' = ppcf_att[i_seq],
        'ppcf_def_seq' = ppcf_def[i_seq]
      )
    res
  }



