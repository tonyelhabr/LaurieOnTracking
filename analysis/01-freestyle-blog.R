
# extrafont::font_import(prompt = FALSE, pattern = 'Fira')
extrafont::loadfonts(device = 'win', quiet = TRUE)
library(tidyverse)
game_id <- 2
events <- import_event_data(game_id = game_id, postprocess = TRUE)
tracking_home <- import_tracking_data_timed(game_id = game_id, side = 'home', overwrite = F)
tracking_away <- import_tracking_data_timed(game_id = game_id, side = 'away', overwrite = F)
tracking <- bind_rows(tracking_home, tracking_away)
tracking

.event_id <- 823L
events_filt <-
  events %>%
  filter(event_id == .event_id)
events_filt

.clip_tracking <- function(tracking) {
  tracking %>% 
    select(-period, -time, -team) %>% 
    mutate(across(where(is.double), ~round(.x, 3))) %>% 
    clipr::write_clip()
}

tracking_start <-
  tracking %>%
  inner_join(events_filt %>% select(frame = start_frame))
tracking_start

tracking_end <-
  tracking %>%
  inner_join(events_filt %>% select(frame = end_frame))
tracking_end

tracking_start %>% .clip_tracking()
tracking_end %>% .clip_tracking()

tracking_start <-
  tibble::tribble(
    ~frame, ~ball_x, ~ball_y,  ~side, ~player_id,     ~x,     ~y,   ~x_v,   ~y_v,
    53027L,  89.251,  36.112, "home",         1L, 86.403, 57.897,  5.625, -5.861,
    53027L,  89.251,  36.112, "home",         2L, 90.569, 39.919,  1.429, -2.975,
    53027L,  89.251,  36.112, "home",         3L, 91.436, 34.291,   1.35,  3.521,
    53027L,  89.251,  36.112, "home",         4L, 87.993, 22.997,  0.957,  5.107,
    53027L,  89.251,  36.112, "home",         5L,  79.96, 36.311,  4.036,  1.764,
    53027L,  89.251,  36.112, "home",         6L,  78.28, 52.395,  3.707, -0.911,
    53027L,  89.251,  36.112, "home",         7L, 81.707, 25.492,  1.621,  2.239,
    53027L,  89.251,  36.112, "home",         8L, 72.437, 73.768,  1.921, -0.725,
    53027L,  89.251,  36.112, "home",         9L, 58.304, 37.277,  0.821, -1.132,
    53027L,  89.251,  36.112, "home",        10L, 56.848, 51.611,  0.857, -0.843,
    53027L,  89.251,  36.112, "home",        11L,  97.66, 47.477, -0.293,  0.918,
    53027L,  89.251,  36.112, "home",        12L,     NA,     NA,     NA,     NA,
    53027L,  89.251,  36.112, "home",        13L,     NA,     NA,     NA,     NA,
    53027L,  89.251,  36.112, "home",        14L,     NA,     NA,     NA,     NA,
    53027L,  89.251,  36.112, "away",        15L, 65.766, 15.929,  2.039,  1.254,
    53027L,  89.251,  36.112, "away",        16L, 54.231,  34.43,  2.125, -0.729,
    53027L,  89.251,  36.112, "away",        17L, 52.172, 57.135,  1.232,   -1.1,
    53027L,  89.251,  36.112, "away",        18L, 53.468, 78.232,  0.461,  -2.15,
    53027L,  89.251,  36.112, "away",        19L, 83.557, 23.396,  1.368,  4.664,
    53027L,  89.251,  36.112, "away",        20L, 68.541,  41.98,  2.793,  1.207,
    53027L,  89.251,  36.112, "away",        21L, 58.819, 58.089,  1.221, -1.139,
    53027L,  89.251,  36.112, "away",        22L,  75.67, 66.388,  3.482, -0.968,
    53027L,  89.251,  36.112, "away",        23L, 89.311, 36.268,  1.589,  4.239,
    53027L,  89.251,  36.112, "away",        24L, 90.368, 54.044,  2.754,  1.468,
    53027L,  89.251,  36.112, "away",        25L, 20.222, 53.147,  0.125, -0.314,
    53027L,  89.251,  36.112, "away",        26L,     NA,     NA,     NA,     NA
  )

tracking_end <-
  tibble::tribble(
    ~frame, ~ball_x, ~ball_y,  ~side, ~player_id,     ~x,     ~y,   ~x_v,   ~y_v,
    53045L,  91.866,  54.338, "home",         1L, 90.206,  53.76,  5.046, -5.186,
    53045L,  91.866,  54.338, "home",         2L,  91.37, 39.101,  0.829, -0.639,
    53045L,  91.866,  54.338, "home",         3L,  92.54,  37.71,  1.743,  5.371,
    53045L,  91.866,  54.338, "home",         4L, 88.743, 26.465,  1.161,  4.896,
    53045L,  91.866,  54.338, "home",         5L, 82.659, 37.861,  3.611,  2.382,
    53045L,  91.866,  54.338, "home",         6L, 80.937, 51.901,  3.725, -0.421,
    53045L,  91.866,  54.338, "home",         7L,  82.47, 27.153,  0.829,  2.232,
    53045L,  91.866,  54.338, "home",         8L, 73.762, 73.402,  1.796, -0.482,
    53045L,  91.866,  54.338, "home",         9L, 58.898, 36.508,  0.829, -0.975,
    53045L,  91.866,  54.338, "home",        10L,  57.43, 50.998,    0.8, -0.832,
    53045L,  91.866,  54.338, "home",        11L, 97.577, 48.748, -0.096,  2.054,
    53045L,  91.866,  54.338, "home",        12L,     NA,     NA,     NA,     NA,
    53045L,  91.866,  54.338, "home",        13L,     NA,     NA,     NA,     NA,
    53045L,  91.866,  54.338, "home",        14L,     NA,     NA,     NA,     NA,
    53045L,  91.866,  54.338, "away",        15L, 66.976, 16.847,  1.536,  1.311,
    53045L,  91.866,  54.338, "away",        16L, 55.677, 34.073,  1.961, -0.361,
    53045L,  91.866,  54.338, "away",        17L, 52.971, 56.375,  1.089,  -0.95,
    53045L,  91.866,  54.338, "away",        18L, 53.816, 76.916,  0.489, -1.796,
    53045L,  91.866,  54.338, "away",        19L, 84.534,  26.64,  1.257,  4.218,
    53045L,  91.866,  54.338, "away",        20L, 70.519, 42.628,  2.696,  0.832,
    53045L,  91.866,  54.338, "away",        21L, 59.563, 57.312,  0.964, -1.032,
    53045L,  91.866,  54.338, "away",        22L, 78.192, 65.947,  3.461, -0.482,
    53045L,  91.866,  54.338, "away",        23L, 89.896, 38.217,  0.286,  2.061,
    53045L,  91.866,  54.338, "away",        24L, 91.884, 54.468,  1.804,  0.214,
    53045L,  91.866,  54.338, "away",        25L, 20.257,   53.1,  0.039,      0,
    53045L,  91.866,  54.338, "away",        26L,     NA,     NA,     NA,     NA
  )

epv_grid <- import_epv_grid()
epv_grid
# epv_grid %>% 
#   ggplot() +
#   aes(x = x, y = y) +
#   .pitch_gg() +
#   geom_raster(
#     aes(fill = value), 
#     hjust = 0,
#     vjust = 0,
#     alpha = 0.5
#   ) +
#   scale_fill_distiller(palette = 'Blues', direction = 1)
  
# xt_grid <- import_xt_grid()
# xt_grid

# Data comes from post-processed Metrica sample game 2 data. I'm using a single frame here. Coordinates have been transformed to be on Opta's 100x100 unit grid.
.event_id <- 823L
.start_frame <- 53027L
.end_frame <- 53045L
.ball_x_start <- 89.251
.ball_y_start <- 36.112
.ball_x_end <- 91.866
.ball_y_end <- 54.338
# The events data frame has an `end_frame` in the actual data set as well, hence the `start_` prefix for `frame`.
events_filt <-
  tibble(
    event_id = .event_id,
    start_frame = .start_frame,
    end_frame = .end_frame,
    start_x = .ball_x_start,
    start_y = .ball_y_start,
    end_x = .ball_x_end,
    end_y = .ball_y_end,
    side = 'away'
  )

pc_grid_epv_start <-
  do_calculate_pc_for_event(
    tracking = tracking_start,
    events = events_filt %>% mutate(frame = start_frame),
    event_id = .event_id,
    epv_grid = epv_grid
  )
pc_grid_epv_start

# pc_grid_xt_start <-
#   do_calculate_pc_for_event(
#     tracking = tracking_filt,
#     events = events_filt %>% mutate(frame = start_frame),
#     event_id = .event_id,
#     epv_grid = xt_grid
#   )
# pc_grid_xt_start

# n_rdbl <- 10L
# pal1 <- colorRampPalette(c('red', 'white', 'blue'))(n_rdbl)
pal2 <- c('home' = 'red', 'away' = 'blue')
arw <- arrow(length = unit(3, 'pt'), type = 'closed')
# See https://stackoverflow.com/a/17313561/120898
pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

.gg_constants <- function(...) {
  list(
    # ...,
    scale_color_manual(values = pal2),
    geom_segment(
      data = tracking_filt %>% filter(!is.na(x)),
      size = 0.5,
      arrow = arw,
      aes(x = x, y = y, xend = x + x_v, yend = y + y_v, color = side)
    ),
    ggrepel::geom_text_repel(
      data = tracking_filt %>% filter(!is.na(x)),
      aes(x = x, y = y, color = side, label = player_id),
      force = 2,
      size = pts(8)
    ),
    geom_point(
      data = tracking_filt %>% filter(!is.na(x)),
      aes(x = x, y = y, color = side),
      size = 4
    ),
    geom_point(
      data = events_filt,
      aes(x = start_x, y = start_y),
      size = 3,
      fill = 'yellow',
      color = 'black',
      shape = 21
    ),
    geom_curve(
      data = events_filt,
      aes(x = start_x, y = start_y, xend = end_x, yend = end_y),
      size = 1,
      arrow = arw,
      color = 'black',
      curvature = -0.2
    ),
    theme(
      plot.title = ggtext::element_markdown('Karla', face = 'bold', size = 18, color = 'gray20'),
      plot.title.position = 'plot',
      plot.subtitle = ggtext::element_markdown('Karla', face = 'bold', size = 14, color = 'gray50'),
      plot.margin = margin(10, 10, 10, 10),
      plot.caption = ggtext::element_markdown('Karla', size = 14, color = 'gray20', hjust = 0),
      plot.caption.position = 'plot'
    ),
    labs(
      caption = '**Viz:** @TonyElHabr | **Data:** Metrica Sports'
    )
  )
}

viz_pc_grid_epv_start <-
  pc_grid_epv_start %>% 
  ggplot() +
  aes(x = x, y = y) +
  .pitch_gg() +
  geom_raster(
    aes(fill = ppcf_att),
    interpolate = TRUE,
    hjust = 0,
    vjust = 0,
    alpha = 0.4
  ) +
  scale_fill_gradient2(low = pal2[['home']], high = pal2[['away']], midpoint = 0.5) +
  scale_color_gradient2(low = pal2[['home']], high = pal2[['away']], midpoint = 0.5) +
  guides(fill = FALSE) +
  ggnewscale::new_scale_color() +
  labs(title = glue::glue('Metrica Sample Game 2, Event {.event_id}, Pitch Control')) +
  .gg_constants()
viz_pc_grid_epv_start
save_plot(viz_pc_grid_epv_start, file = sprintf('pc_%s_r', .event_id))

epvxppcf_grid_start <-
  inner_join(pc_grid_epv_start, epv_grid) %>%
  mutate(epv = ppcf_att * value)
epvxppcf_grid_start

eepv_added <-
  do_compute_eepv_added(
    bind_rows(tracking_start, tracking_end), 
    events_filt, 
    event_id = .event_id
  )
eepv_added

viz_epvxppcf_grid_start <-
  epvxppcf_grid_start %>%
  arrange(desc(epv)) %>% 
  ggplot() +
  aes(x = x, y = y) +
  .pitch_gg() +
  geom_raster(
    aes(fill = epv),
    interpolate = TRUE,
    hjust = 0,
    vjust = 0,
    alpha = 0.5
  ) +
  scale_fill_distiller(palette = 'Blues', direction = 1) +
  labs(
    title = glue::glue('Metrica Sample Game 2, Event {.event_id}, EPV'),
    subtitle = glue::glue('EPV Added: {scales::number(eepv_added[["eepv_added"]], accuracy = 0.001)}')
  ) +
  .gg_constants()
viz_epvxppcf_grid_start
save_plot(viz_epvxppcf_grid_start, file = sprintf('epv_%s_r', .event_id))
