

..seq_pitch_dim <- function(dim = c('x', 'y'), n_cell = 100L) {
  dim <- match.arg(dim)
  name_origin <- sprintf('origin_%s', dim)
  name_length <- switch(dim, x = 'length', y = 'width')
  start <- .pitch_international[[name_origin]]
  end <- .pitch_international[[name_length]]
  d <- (end - start) / n_cell
  seq.int(start + d / 2, end - d / 2, length.out = n_cell)
}

.get_pitch_grid <- memoise::memoise({
  function(n_cell_x = 100L, n_cell_y = n_cell_x) {
  crossing(
    x = ..seq_pitch_dim('x'),
    y = ..seq_pitch_dim('y')
  )
}
})

.get_speed <- function(coord, next_coord, time, next_time) {
  (next_coord - coord) / (next_time - time)
}

.get_theta <- function(x_speed, y_speed) {
  hypotenuse_speed <- sqrt(x_speed^2 + y_speed^2)
  acos(x_speed / hypotenuse_speed)
}

.get_mu <- function(location, speed) {
  location + speed / 2
}

.get_srat <- function(speed_x, speed_y) {
  speed <- sqrt(speed_x^2 + abs(speed_y)^2)
  (speed / 13)^2
}

.get_ri <- function(x, y, ball_x, ball_y) {
  ball_diff <- sqrt((x - ball_x) ^ 2 + (y - ball_y)^2)
  ri <- 4 + ((ball_diff^3) / ((18^3) / 6))
  min(ri, 10)
}

.get_R <- function(theta) {
  matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), nrow = 2)
}

.get_S <- function(ri, srat) {
  top_left <- ri * (1 + srat) / 2
  bottom_right <- ri * (1-srat) / 2
  matrix(c(top_left, 0, 0, bottom_right), nrow = 2)
}

.get_Sigma <- function(R, S) {
  # I believe this `solve()` is what is leading to some bloated pitch control contours. (It "blows up".)
  inv_R <- solve(R)
  R %*% S %*% S %*% inv_R
}

.calculate_I <- function(pitch_grid, x, y, mu_x, mu_y, Sigma) {
  mu <- c(mu_x, mu_y)
  player_loc <- c(x, y)
  pitch_grid <- as.matrix(pitch_grid)
  num <- mvtnorm::dmvnorm(as.matrix(pitch_grid), mu, Sigma)
  den <- mvtnorm::dmvnorm(t(matrix(player_loc)), mu, Sigma)
  num / den
}

calculate_pc <- 
  function(time, next_time, ball_x, ball_y, x, y, next_x, next_y, side, player_id, 
           pitch_grid = .get_pitch_grid(), 
           ..., 
           debug = FALSE,
           export = TRUE,
           import = export,
           f_export = feather::write_feather,
           f_import = feather::read_feather,
           dir = .get_dir_data(), 
           basename = glue::glue('pc_{sprintf("%.02f", time)}_{sprintf("%04d", player_id)}.fst'), 
           path = fs::path(dir_data, basename), 
           overwrite = FALSE) {
    
    # browser()
    if(import) {
      path_exists <- fs::file_exists(path)
      if(path_exists & !overwrite & import) {
        return(f_import(path))
      }
    }
    speed_x <- .get_speed(x, next_x, time, next_time)
    speed_y <- .get_speed(y, next_y, time, next_time)
    srat <- .get_srat(speed_x, speed_y)
    theta <- .get_theta(speed_x, speed_y)
    
    mu_x <- .get_mu(x, speed_x)
    mu_y <- .get_mu(y, speed_y)
    
    ri <- .get_ri(x, y, ball_x, ball_y)
    
    R <- .get_R(theta)
    S <- .get_S(ri, srat)
    
    Sigma <- .get_Sigma(R, S)
    pitch_grid$I <- .calculate_I(pitch_grid, x, y, mu_x, mu_y, Sigma)
    if(export) {
      f_export(pitch_grid, path)
    }
    pitch_grid
  }
do_calculate_pc <- partial(calculate_pc, ... = )

do_aggregate_pc <- 
  function(pc,
           adjust = FALSE,
           method = c('original', 'modified'),
           sep = '_',
           suffix = NULL,
           export = TRUE,
           import = export,
           f_export = feather::write_feather,
           f_import = feather::read_feather,
           dir = .get_dir_data(), 
           basename = glue::glue('pc_agg_{method}{ifelse(!is.null(suffix), paste0(sep, suffix), "")}.fst'), 
           path = fs::path(dir_data, basename), 
           overwrite = FALSE) {
    
    method <- match.arg(method)
    if(import) {
      path_exists <- fs::file_exists(path)
      if(path_exists & !overwrite) {
        return(f_import(path))
      }
    }

    
    if(adjust) {
      # Another way to adjust is to look at large deviances in `player_x - x` and `player_y - y` with respect to `I`.
      # pc_bad <-
      #   pc %>% 
      #   # filter(frame == 110) %>% # , player_id != 6717) %>% 
      #   rename(player_x = x, player_y = y) %>% 
      #   unnest(data) %>% 
      #   mutate(dx = player_x - x, dy = player_y - y) %>% 
      #   mutate(s = sqrt(dx^2 + dy^2))
      
      pc_bad <-
        pc %>% 
        rename(player_x = x, player_y = y) %>% 
        unnest(data)
      
      bad_players <-
        pc_bad %>% 
        group_by(frame, player_id) %>% 
        # Somewhat analogous to 3 standard deviations.
        summarize(n = sum(I > 3)) %>% 
        ungroup() %>% 
        # Identify "bad" players as those who have more than 3 frames with >3 stdev. `I`.
        filter(n > 3)
      
      if(nrow(bad_players) > 0) {
        
        suppressMessages(
          pc_bad_adj <-
            pc_bad %>% 
            inner_join(bad_players) %>% 
            group_by(player_id, frame) %>% 
            # Sort of normalize back to where the max is 2 standard deviations.
            mutate(across(I, ~(2 * ((.x - 0) / (max(.x) - 0))^1)))%>% 
            ungroup() %>% 
            # select(-dx, -dy, -s) %>% 
            nest(data = c(x, y, I)) %>% 
            rename(x = player_x, y = player_y)
        )
        
        suppressMessages(
          pc <-
            bind_rows(
              pc %>% anti_join(bad_players),
              pc_bad_adj
            )
        )
      }
    }
    # browser()
    res <-
      pc %>%
      select(frame, time, player_id, side, x, y, I) %>% 
      unnest(I) %>% 
      group_by(frame, time, side, x, y) %>%
      summarise(side_sum = sum(I, na.rm = TRUE)) %>%
      ungroup() %>%
      pivot_wider(names_from = side, values_from = side_sum)
    
    res <-
      if(method == 'original') {
        res %>% mutate(pc = 1 / (1 + exp(home - away)))
      } else if (method == 'modified') {
        res %>% mutate(pc = home)
      }
    if(export) {
      f_export(res, path)
    }
    res
  }


