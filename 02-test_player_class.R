
.frame <- 52941L
event <- events %>% filter(start_frame == .frame)
# tracking %>% filter(frame == 52941L) %>% mutate(across(where(is.double), ~round(.x, 3))) %>% clipr::write_clip()
tracking <-
tibble::tribble(
  ~period, ~frame,   ~time, ~ball_x, ~ball_y,  ~side, ~player,     ~x,     ~y,  ~team,  ~x_v,  ~y_v,
       1L, 52941L, 2117.64,  83.228,  60.898, "home",      1L,  75.86, 19.098, "home", 2.726, 2.047,
       1L, 52941L, 2117.64,  83.228,  60.898, "home",      2L, 83.965, 32.694, "home", 3.184, 2.402,
       1L, 52941L, 2117.64,  83.228,  60.898, "home",      3L, 83.456, 43.271, "home", 3.619, 4.447,
       1L, 52941L, 2117.64,  83.228,  60.898, "home",      4L, 85.264, 57.351, "home", 2.318, 4.367,
       1L, 52941L, 2117.64,  83.228,  60.898, "home",      5L, 68.906, 41.463, "home", 3.791, 2.618,
       1L, 52941L, 2117.64,  83.228,  60.898, "home",      6L, 69.767, 27.799, "home", 3.199, 2.008,
       1L, 52941L, 2117.64,  83.228,  60.898, "home",      7L, 73.456, 49.783, "home", 4.444, 2.727,
       1L, 52941L, 2117.64,  83.228,  60.898, "home",      8L,  67.19, 15.758, "home", 2.936, 1.355,
       1L, 52941L, 2117.64,  83.228,  60.898, "home",      9L, 58.126, 38.864, "home", 1.151, 1.241,
       1L, 52941L, 2117.64,  83.228,  60.898, "home",     10L, 56.188, 30.429, "home", 1.204, 0.617,
       1L, 52941L, 2117.64,  83.228,  60.898, "home",     11L, 99.909, 34.432, "home", 2.254, 0.872,
       1L, 52941L, 2117.64,  83.228,  60.898, "home",     12L,     NA,     NA, "home",    NA,    NA,
       1L, 52941L, 2117.64,  83.228,  60.898, "home",     13L,     NA,     NA, "home",    NA,    NA,
       1L, 52941L, 2117.64,  83.228,  60.898, "home",     14L,     NA,     NA, "home",    NA,    NA,
       1L, 52941L, 2117.64,  83.228,  60.898, "away",     15L, 56.943, 56.847, "away", 4.346, 1.783,
       1L, 52941L, 2117.64,  83.228,  60.898, "away",     16L, 48.255, 41.246, "away", 2.366, 1.353,
       1L, 52941L, 2117.64,  83.228,  60.898, "away",     17L, 48.679, 24.164, "away", 2.156, 2.397,
       1L, 52941L, 2117.64,  83.228,  60.898, "away",     18L, 53.589, 10.579, "away", 1.009, 1.253,
       1L, 52941L, 2117.64,  83.228,  60.898, "away",     19L, 83.327, 60.943, "away", 2.794,  1.46,
       1L, 52941L, 2117.64,  83.228,  60.898, "away",     20L, 61.849, 39.555, "away", 3.056, 0.954,
       1L, 52941L, 2117.64,  83.228,  60.898, "away",     21L, 54.458, 24.013, "away", 1.504, 2.161,
       1L, 52941L, 2117.64,  83.228,  60.898, "away",     22L, 71.509, 22.307, "away", 1.354, 0.265,
       1L, 52941L, 2117.64,  83.228,  60.898, "away",     23L, 84.925, 47.717, "away", 2.543, 3.844,
       1L, 52941L, 2117.64,  83.228,  60.898, "away",     24L, 79.165, 34.443, "away", 3.941, 0.449,
       1L, 52941L, 2117.64,  83.228,  60.898, "away",     25L, 19.325, 30.903, "away", 0.814, 0.644,
       1L, 52941L, 2117.64,  83.228,  60.898, "away",     26L,     NA,     NA, "away",    NA,    NA
  )

# players <-
#   tracking %>% 
#   pull(player) %>% 
#   map(~player(player_id = .x, start_frame = 52941L, events = events, tracking = tracking))
# players

# p <- player(player_id = 1L, start_frame = 52941L, tracking = tracking)
# p
# 
# r <- .tti(p, rx2 = 3, ry2 = 7)
# p_intercept <- .p_intercept(p, t = 2, rx2 = 3, ry2 = 7)
# p_intercept

f_assert_dppcf_dt <- function(dppcf_dt , i, is_attack = TRUE) {
  assertthat::assert_that(dppcf_dt >= 0, msg = sprintf('Incremental %sing player probability (`dppcf_dt = %.3f` at `i = %d`) must be >= 0', ifelse(is_attack, 'attack', 'defend'), dppcf_dt , i))
}
f_assert_ppcf <- function(ppcf, i, is_attack = TRUE) {
  assertthat::assert_that(ppcf >= 0 & ppcf <= 0, msg = sprintf('%sing player probability (`ppcf = %.3f` at `i = %d`) must be >= 0 and <= 1', ifelse(is_attack, 'Attack', 'Defend'), ppcf, i))
}

f_msg_ppcf_p <- function(p, dppcf_dt, i, is_attack = TRUE) {
  if(i >= 4 & i <= 159) {
    return(invisible())
  }
  cat(sprintf('%sing `player_id = %d` ppcf change at `i = %d`: %.3f -> %.3f', ifelse(is_attack, 'Attack', 'Defend'), field(p, 'player_id'), i, field(p, 'ppcf'), dppcf_dt * int_dt), sep = '\n')
}

f_msg_ppcf <- function(ppcf, p, i, is_attack = TRUE) {
  if(i >= 4 & i <= 159) {
    return(invisible())
  }
  cat(sprintf('%sing `ppcf` change at `i = %d` due to `player_id = %d` (with `ppcf = %.3f`): %.3f -> %.3f', ifelse(is_attack, 'Attack', 'Defend'), i, field(p, 'player_id'), field(p, 'ppcf'), ppcf[[i]], ppcf[[i]] + field(p, 'ppcf')), sep = '\n')
}

calculate_pitch_contral_at_target <-
  function(target_x, target_y, ball_x, ball_y, players, params = get_default_model_params()) {
    # ball_travel_time <- sqrt(target_position - ball_start_pos)
    ball_x <- 83.228
    ball_y <- 60.898
    # target_x <- 95
    # target_y <- 50
    target_x <- ball_x
    target_y <- ball_y
    ball_dist <- .norm(target_x, ball_x, target_y, ball_y)
    ball_time <- ball_dist / params[['average_ball_speed']]
    
    players <-
      tracking %>% 
      pull(player) %>% 
      map(~player(player_id = .x, start_frame = 52941L, events = events, tracking = tracking))
    players
    
    ps_att <- players %>% keep(~{field(.x, 'side') == 'home'})
    ps_def <- players %>% keep(~{field(.x, 'side') == 'away'})

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
    ps_deff_tau_min <- function(v) {
      # res <- v %>% map_dbl(~.tti(..1, rx2 = target_x, ry2 = target_y)) %>% min(na.rm = TRUE)
      res <- v %>% map_dbl(~field(..1, 'tti')) %>% min(na.rm = TRUE)
      res
    }
    field(ps_att[[1]], 'tti')
    field(ps_att[[1]], 'player_id')
    ps_def[[2]]
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
        discard(~(field(., 'tti') - tau_min) < t)
      res
    }
    ps_att_filt <- ps_att %>% f_discard(tau_min = tau_min_att, t = t_att)
    ps_def_filt <- ps_def %>% f_discard(tau_min = tau_min_def, t = t_def)
    ps_def_filt
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
    ppcf_def <- ppcf_att
    p_tot <- 0
    i <- 2
    while (((1 - p_tot) > params[['model_converge_tol']]) & (i < n_seq)) {
      t <- dt_seq[i]
      for(ip in seq_along(ps_att_filt)) {
        p <- ps_att_filt[[ip]]
        dppcf_dt <- (1 - ppcf_att[i - 1] - ppcf_def[i - 1]) * .p_intercept(p, t) * field(p, 'lambda_att')
        # f_assert_dppcf_dt(dppcf_dt, i = i, is_attack = TRUE)
        f_msg_ppcf_p(p = ps_def_filt[[ip]], dppcf_dt = dppcf_dt, i = i, is_attack = TRUE)
        .update_ppcf(ps_def_filt[[ip]]) <- dppcf_dt * int_dt

        f_msg_ppcf(ppcf = ppcf_att, p = ps_att_filt[[ip]], i = i, is_attack = TRUE)
        ppcf_att[i] <- ppcf_att[i] + field(ps_att_filt[[ip]], 'ppcf')
      }
      for(ip in seq_along(ps_def_filt)) {
        p <- ps_def_filt[[ip]]
        dppcf_dt <- (1 - ppcf_att[i - 1] - ppcf_def[i - 1]) * .p_intercept(p, t) * field(p, 'lambda_def')
        # f_assert_dppcf_dt(dppcf_dt, i = i, is_attack = FALSE)
        f_msg_ppcf_p(p = ps_def_filt[[ip]], dppcf_dt = dppcf_dt, i = i, is_attack = FALSE)
        .update_ppcf(ps_def_filt[[ip]]) <- dppcf_dt * int_dt
        
        f_msg_ppcf(ppcf = ppcf_def, p = ps_def_filt[[ip]], i = i, is_attack = FALSE)
        ppcf_def[i] <- ppcf_def[i] + field(ps_def_filt[[ip]], 'ppcf')
      }
      p_tot <- ppcf_def[i] + ppcf_att[i]
      i <- i + 1
    }
    cat(sprintf('`i = %d`, `ppcf_def[i] = %.3f`, `ppcf_att[i] = %.3f`, `p_tot = %.3f`', i, ppcf_def[i], ppcf_att[i], p_tot), sep = '\n')
    if(i >= n_seq) {
      warning(sprintf('Integration failed to converge: %.3f', p_tot), call. = FALSE)
    }
    f_assert_ppcf(ppcf_att[i - 1], i, is_attack = TRUE)
    f_assert_ppcf(ppcf_def[i - 1], i, is_attack = FALSE)
    res <- list('ppcf_att' = ppcf_att[i - 1], 'ppcf_def' = ppcf_def[i - 1])
    res
  }
calculate_pitch_contral_at_target()


