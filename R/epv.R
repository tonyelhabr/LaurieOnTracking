
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

.rescale_tidy_epv_grid <- function(tidy_epv_grid, dims = .get_dims_opta()) {
  res <-
    tidy_epv_grid %>% 
    # I believe I double checked how this flips and it should be correct. The `X` prefixed columns are `x`. 
    mutate(
      across(col, ~str_remove(.x, '^X') %>% as.integer() %>% .rescale(c(1L, 50L), c(0L, dims[1]))),
      across(row, ~.rescale(.x, c(1L, 32L), c(0L, dims[2])))
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
    res <- 
      read_csv(path, col_names = FALSE) %>% 
      .tidy_epv_grid() %>% 
      .rescale_tidy_epv_grid() %>% 
      .add_grid_id_cols()
    res
  }
})

# Reference: https://raw.githubusercontent.com/anenglishgoat/InteractivePitchControl/master/xT.csv
import_xt_grid <- memoise::memoise({
  function(path = file.path('data', 'xT.csv')) {
    import_epv_grid(path = path)
  }
})

.get_pitch <- function(pitch_fill = 'white', pitch_color = 'black', limits = FALSE) {
  ggsoccer::annotate_pitch(
    fill = pitch_fill, 
    colour = pitch_color,
    limits = limits,
    dimension = ggsoccer::pitch_opta
  )
}

.pitch_gg <- function(pitch = .get_pitch(), ...) {
  res <-
    list(
      ...,
      pitch,
      # coord_flip(
      #   xlim = xlim,
      #   ylim = ylim
      # ),
      # coord_flip(),
      ggsoccer::theme_pitch(), # changing the default aspect ratio cuz it looks weird
      theme(legend.position = 'none')
    )
  res
}

plot_epv_grid <- function(epv_grid = import_epv_grid(), attack_direction = 1, ...) {
  if(attack_direction == -1) {
    epv_grid <- epv_grid %>% mutate(across(value, ~.x * -1))
  }
  viz <-
    epv_grid %>% 
    ggplot() +
    aes(x = x, y = y) %>% 
    .pitch_gg(...) +
    geom_tile(aes(fill = value))
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
    epv_start = value_start,
    epv_end = value_end,
    epv_change = value_end - value_start
  )
}

do_compute_epv_added <- function(events, frame, epv_grid = import_epv_grid()) {
  event <- events %>% filter(start_frame == !!frame)
  assertthat::assert_that(nrow(event) == 1L)
  start_x <- events1[['start_x']]
  start_y <- events1[['start_y']]
  end_x <- events1[['end_x']]
  end_y <- events1[['end_y']]
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

do_compute_xt_added <- function(..., epv_grid = import_xt_grid()) {
  do_compute_epv_added(..., epv_grid = epv_grid)
}


