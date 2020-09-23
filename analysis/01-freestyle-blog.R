
library(tidyverse)
# Data comes from post-processed Metrica sample game 2 data. I'm using a single frame here. Coordinates have been transformed to be on Opta's 100x100 unit grid.
.event_id <- 823L
events_filt <-
  tibble::tribble(
    ~event_id,  ~side,  ~type, ~start_frame, ~end_frame, ~start_x, ~start_y, ~end_x, ~end_y,
         823L, "away", "pass",       53027L,     53045L,      89L,      36L,    92L,    54L
    )

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

pal2 <- c('home' = 'red', 'away' = 'blue')
arw <- arrow(length = unit(3, 'pt'), type = 'closed')
# See https://stackoverflow.com/a/17313561/120898
pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

.gg_constants <- function(..., tracking = tracking_start, events = events_filt) {
  list(
    scale_color_manual(values = pal2),
    geom_segment(
      data = tracking %>% filter(!is.na(x)),
      size = 0.5,
      arrow = arw,
      aes(x = x, y = y, xend = x + x_v, yend = y + y_v, color = side)
    ),
    ggrepel::geom_text_repel(
      data = tracking %>% filter(!is.na(x)),
      aes(x = x, y = y, color = side, label = player_id),
      force = 2,
      size = pts(8)
    ),
    geom_point(
      data = tracking %>% filter(!is.na(x)),
      aes(x = x, y = y, color = side),
      size = 4
    ),
    geom_point(
      data = events,
      aes(x = start_x, y = start_y),
      size = 2,
      fill = 'black',
      color = 'black',
      shape = 21
    ),
    geom_segment(
      data = events,
      aes(x = start_x, y = start_y, xend = end_x, yend = end_y),
      # curvature = -0.2
      size = 1,
      arrow = arw,
      color = 'black'
    ),
    # labs(
    #   caption = '**Viz:** @TonyElHabr | **Data:** Metrica Sports'
    # ),
    theme(
      # plot.title = ggtext::element_markdown('Karla', face = 'bold', size = 18, color = 'gray20'),
      # plot.title.position = 'plot',
      # plot.subtitle = ggtext::element_markdown('Karla', face = 'bold', size = 14, color = 'gray50'),
      plot.subtitle = element_text(size = 16, hjust = 0.5),
      plot.margin = margin(10, 10, 10, 10) # ,
      # plot.caption = ggtext::element_markdown('Karla', size = 14, color = 'gray20', hjust = 0),
      # plot.caption.position = 'plot'
    )
  )
}

# tti and p_intercept ----
start_frame <- events_filt[['start_frame']]
tracking_filt <- tracking_start
params <- .get_default_pc_params()

ball_x <- tracking_filt[1, ][['ball_x']]
ball_y <- tracking_filt[1, ][['ball_y']]
target_x <- ball_x + 5
target_y <- target_x
players <-
  tracking_filt %>%
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
players
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

ps <-
  c(ps_att, ps_def) %>% 
  map_dfr(unlist) %>% 
  mutate(
    across(c(is_gk, is_attack, in_frame), as.logical),
    across(c(player_id), as.integer),
    across(c(tti, x, y, x_v, y_v, vmax, reaction_time, tti_sigma, lambda_att, lambda_def, ppcf), as.double)
  )

f_get_p_intercept <- function(v, t) {
  res <- vector(mode = 'double', length(v))
  for(i in seq_along(v)) {
    # browser()
    res[[i]] <- .get_p_intercept(v[[i]], t)
  }
  res
}

f_get_p_intercepts <- function(t) {
  
  # list('att' = ps_att, 'def' = ps_def)
  p_intercept_att <- ps_att %>% f_get_p_intercept(t = t)
  p_intercept_def <- ps_def %>% f_get_p_intercept(t = t)
  res <-
    # Would need to reverse this direction if attackers were the home team.
    # Also, should be more robust about `player_id`s.
    c(p_intercept_def, p_intercept_att) %>% 
    tibble(p_intercept = ., t = as.integer(!!t)) %>% 
    mutate(player_id = row_number()) %>% 
    mutate(p_intercept_norm = p_intercept / sum(p_intercept, na.rm = TRUE)) %>% 
    relocate(player_id, t)
  res
}
pis <- c(6L, 8L) %>% map_dfr(f_get_p_intercepts)

ps_filt <-
  ps %>% 
  arrange(tti) %>% 
  slice(c(1:2)) %>% 
  mutate(across(tti, ~scales::number(.x, accuracy = 0.1))) %>% 
  mutate(lab = glue::glue('tti = {tti} s'))

pis_filt <-
  pis %>% 
  inner_join(ps_filt %>% select(player_id, x, y)) %>% 
  mutate(
    across(matches('p_intercept'), ~scales::percent(.x, accuracy = 1))
  ) %>% 
  pivot_wider(names_from = t, values_from = c(p_intercept, p_intercept_norm)) %>% 
  mutate(
    lab = glue::glue('t = 6 s, p_intercept = {p_intercept_6}
                         t = 8 s, p_intercept = {p_intercept_8}'),
    lab_norm = glue::glue('t = 6 s, p_intercept = {p_intercept_norm_6}
                              t = 8 s, p_intercept = {p_intercept_norm_8}')
  )
pis_filt
target <- tibble(x = target_x, y = target_y)

gg_constants <- 
  .gg_constants(
    tracking = tracking_start %>% inner_join(ps_filt), 
    events = events_filt
  )
# Remove the event layers with the ball and pass segment.
gg_constants[[6]] <- NULL
gg_constants[[5]] <- NULL

viz_tti_ex <-
  ps %>% 
  ggplot() +
  aes(x = x, y = y) +
  .pitch_gg() +
  gg_constants +
  geom_point(data = target, shape = 18, size = 4, color = 'magenta', fill = 'green') +
  geom_segment(
    data = ps_filt,
    aes(xend = target$x, yend = target$y), linetype = 2
  ) +
  ggforce::geom_mark_circle(
    data = ps_filt,
    color = NA,
    fill = NA,
    label.buffer = unit(1, 'mm'),
    con.cap = unit(1, 'mm'),
    aes(label = lab, group = player_id)
  ) +
  theme(
    plot.title = element_text(size = 16, hjust = 0, face = 'bold'),
    plot.caption = element_text(size = 12, hjust = 0)
  ) +
  labs(
    title = 'Time to Intercept Example',
    caption = 'How long would it take the player to reach the marked position?'
  )
viz_tti_ex
save_plot(viz = viz_tti_ex)

viz_p_intercept_ex_1 <-
  pis %>% 
  ggplot() +
  aes(x = x, y = y) +
  .pitch_gg() +
  gg_constants +
  geom_point(data = target, shape = 18, size = 4, color = 'magenta', fill = 'green') +
  geom_segment(
    data = ps_filt,
    aes(xend = target$x, yend = target$y), linetype = 2
  ) +
  ggforce::geom_mark_circle(
    data = pis_filt,
    label.buffer = unit(1, 'mm'),
    con.cap = unit(1, 'mm'),
    aes(label = lab, group = player_id)
  ) +
  theme(
    plot.title = element_text(size = 16, hjust = 0, face = 'bold'),
    plot.caption = element_text(size = 12, hjust = 0)
  ) +
  labs(
    title = 'Probability of Intercepting Example 1',
    caption = 'What is the probability that the player reaches the marked position within t seconds?'
  )
viz_p_intercept_ex_1
save_plot(viz = viz_p_intercept_ex_1)

gg_constants_norm <- 
  .gg_constants(
    tracking = tracking_start, 
    events = events_filt
  )
gg_constants_norm[[6]] <- NULL
gg_constants_norm[[5]] <- NULL

viz_p_intercept_ex_2 <-
  pis %>% 
  ggplot() +
  aes(x = x, y = y) +
  .pitch_gg() +
  # .gg_constants(tracking = tracking_start, events = events_filt) +
  gg_constants_norm +
  geom_point(data = target, shape = 18, size = 4, color = 'magenta', fill = 'green') +
  geom_segment(
    data = ps_filt,
    aes(xend = target$x, yend = target$y), linetype = 2
  ) +
  ggforce::geom_mark_circle(
    # geom_label(
    data = pis_filt,
    label.buffer = unit(1, 'mm'),
    con.cap = unit(1, 'mm'),
    aes(label = lab_norm, group = player_id)
  ) +
  theme(
    plot.title = element_text(size = 16, hjust = 0, face = 'bold'),
    plot.caption = ggtext::element_markdown(size = 12, hjust = 0)
  ) +
  labs(
    title = 'Probability of Intercepting Example 2',
    caption = glue::glue('What is the probability that the player reaches the marked position within t seconds **before any other player**?')
  )
viz_p_intercept_ex_2
save_plot(viz = viz_p_intercept_ex_2)

# pc ----
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
pc_grid_epv_start <-
  do_calculate_pc_for_event(
    tracking = tracking_start,
    events = events_filt %>% mutate(frame = start_frame),
    event_id = .event_id,
    epv_grid = epv_grid
  )
pc_grid_epv_start

viz_pc_grid_epv_start <-
  pc_grid_epv_start %>% 
  ggplot() +
  aes(x = x, y = y) +
  .pitch_gg() +
  geom_raster(
    aes(fill = ppcf_att),
    interpolate = TRUE,
    hjust = 0.5,
    vjust = 0.5,
    alpha = 0.4
  ) +
  scale_fill_gradient2(low = pal2[['home']], high = pal2[['away']], midpoint = 0.5) +
  scale_color_gradient2(low = pal2[['home']], high = pal2[['away']], midpoint = 0.5) +
  guides(fill = FALSE) +
  ggnewscale::new_scale_color() +
  # labs(title = glue::glue('Metrica Sample Game 2, Event {.event_id}, Pitch Control')) +
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
    hjust = 0.5,
    vjust = 0.5,
    alpha = 0.5
  ) +
  scale_fill_distiller(palette = 'Blues', direction = 1) +
  # labs(
  #   title = glue::glue('Metrica Sample Game 2, Event {.event_id}, EPV'),
  #   subtitle = glue::glue('EPV Added: {scales::number(eepv_added[["eepv_added"]], accuracy = 0.001)}')
  # ) +
  labs(
    subtitle = glue::glue('EPV Added: {scales::number(eepv_added[["eepv_added"]], accuracy = 0.001)}')
  ) +
  .gg_constants()
viz_epvxppcf_grid_start
save_plot(viz_epvxppcf_grid_start, file = sprintf('epv_%s_r', .event_id))

.dir_plots <- fs::path('output', 'figs')
.generate_and_export_header <- function() {
  viz_header <- 
    tibble(
      x = c(-2, -1.1, 0, 1.1, 2),
      lab = c('', 'python', '', 'R', '')
    ) %>% 
    ggplot() +
    aes(x = x, y = 0) +
    geom_text(aes(label = lab), size = pts(18), fontface = 'bold', hjust = 0.5) +
    theme_void()
  save_plot(viz_header, file = 'header', height = 0.5, width = 16)
  viz_header
}

.import_png <- function(event_id, type, lang = c('python', 'r')) {
  lang <- match.arg(lang)
  event_id <- ifelse(lang == 'python', event_id - 1L, event_id)
  path <- fs::path(.dir_plots, sprintf('%s_%s_%s.png', type, event_id, lang))
  magick::image_read(path)
}

.import_png_header <- function() {
  path <- fs::path(.dir_plots, sprintf('header.png'))
  magick::image_read(path)
}

.png_scale <- function(img, dpi = 96, width = 8, height = 10, geometry = sprintf('%dx%d', width * dpi, height * dpi)) {
  magick::image_scale(img, geometry = geometry)
}

append_plots <- function(event_id, type = c('pc', 'epv'))  {
  # event_id = .event_id
  # type = 'pc'
  type <- match.arg(type)
  viz_header <- .import_png_header()
  viz_py <- .import_png(event_id, type = type, lang = 'python')
  viz_r <- .import_png(event_id, type = type, lang = 'r')
  res <- magick::image_append(c(.png_scale(viz_py), .png_scale(viz_r)))
  # res <- magick::image_append(c(.png_scale(viz_header, height = 0.5, width = 16), res), stack = TRUE)
  img_info <- magick::image_info(res)
  # magick::image_info(viz_header)
  # h <- img_info$height
  w <- img_info$width
  res <- magick::image_append(c(.png_scale(viz_header, geometry = sprintf('%dx%d', w, w)), res), stack = TRUE)
  path <- fs::path(.dir_plots, sprintf('viz_%s_%s_combined.png', type, event_id))
  magick::image_write(res, path = path)
  res
}

.generate_and_export_header()
viz_pc_append <- append_plots(event_id = .event_id, type = 'pc')
viz_epv_append <- append_plots(event_id = .event_id, type = 'epv')
