
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

.rescale_tidy_epv_grid <- function(tidy_epv_grid) {
  res <-
    tidy_epv_grid %>% 
    mutate(
      across(col, ~str_remove(.x, '^X') %>% as.integer() %>% .rescale(c(1L, 50L), c(0L, 106L))),
      across(row, ~.rescale(.x, c(1L, 32L), c(0L, 68L)))
    ) %>% 
    rename(x = row, y = col)
  res
}


read_epv_grid <- memoise::memoise({
  function(path = file.path('data', 'EPV_grid.csv')) {
    # res <- read_delim(path, delim = ',')
    # res <- read.table(path, sep = ',') %>% as.matrix()
    res <- 
      read_csv(path, col_names = FALSE) %>% 
      .tidy_epv_grid() %>% 
      .rescale_tidy_epv_grid()
    res
  }
})

.get_pitch <- function(pitch_fill = 'white', pitch_color = 'black', limits = FALSE) {
  ggsoccer::annotate_pitch(
    fill = pitch_fill, 
    colour = pitch_color,
    # limits = limits,
    dimension = ggsoccer::pitch_international
  )
  xlim
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
      coord_flip(),
      ggsoccer::theme_pitch(), # changing the default aspect ratio cuz it looks weird
      theme(legend.position = 'none')
    )
  res
}


plot_epv <- function(epv_grid = read_epv_grid(), attack_direction = 1, ...) {
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
