
library(tidyverse)

.event_id <- 823L
events_filt <-
  tibble::tribble(
    ~event_id,  ~side,  ~type, ~start_frame, ~end_frame, ~start_x, ~start_y, ~end_x, ~end_y,
    823L, "away", "pass",       53027L,     53045L,    93.45,    24.48,   96.6,  36.72
  )

tracking_start <-
  tibble::tribble(
    ~frame, ~ball_x, ~ball_y,  ~side, ~player_id,      ~x,     ~y,   ~x_v,   ~y_v,
    53027L,  93.714,  24.556, "home",         1L,  90.723,  39.37,  5.906, -3.985,
    53027L,  93.714,  24.556, "home",         2L,  95.097, 27.145,    1.5, -2.023,
    53027L,  93.714,  24.556, "home",         3L,  96.008, 23.318,  1.418,  2.395,
    53027L,  93.714,  24.556, "home",         4L,  92.393, 15.638,  1.005,  3.473,
    53027L,  93.714,  24.556, "home",         5L,  83.958, 24.691,  4.238,    1.2,
    53027L,  93.714,  24.556, "home",         6L,  82.194, 35.629,  3.893, -0.619,
    53027L,  93.714,  24.556, "home",         7L,  85.792, 17.335,  1.703,  1.523,
    53027L,  93.714,  24.556, "home",         8L,  76.059, 50.162,  2.018, -0.493,
    53027L,  93.714,  24.556, "home",         9L,  61.219, 25.348,  0.863,  -0.77,
    53027L,  93.714,  24.556, "home",        10L,   59.69, 35.095,    0.9, -0.573,
    53027L,  93.714,  24.556, "home",        11L, 102.543, 32.284, -0.308,  0.624,
    53027L,  93.714,  24.556, "home",        12L,      NA,     NA,     NA,     NA,
    53027L,  93.714,  24.556, "home",        13L,      NA,     NA,     NA,     NA,
    53027L,  93.714,  24.556, "home",        14L,      NA,     NA,     NA,     NA,
    53027L,  93.714,  24.556, "away",        15L,  69.054, 10.832,  2.141,  0.852,
    53027L,  93.714,  24.556, "away",        16L,  56.943, 23.412,  2.231, -0.495,
    53027L,  93.714,  24.556, "away",        17L,  54.781, 38.852,  1.294, -0.748,
    53027L,  93.714,  24.556, "away",        18L,  56.141, 53.198,  0.484, -1.462,
    53027L,  93.714,  24.556, "away",        19L,  87.735, 15.909,  1.436,  3.172,
    53027L,  93.714,  24.556, "away",        20L,  71.968, 28.546,  2.933,  0.821,
    53027L,  93.714,  24.556, "away",        21L,   61.76, 39.501,  1.283, -0.775,
    53027L,  93.714,  24.556, "away",        22L,  79.454, 45.144,  3.656, -0.658,
    53027L,  93.714,  24.556, "away",        23L,  93.777, 24.662,  1.669,  2.883,
    53027L,  93.714,  24.556, "away",        24L,  94.886,  36.75,  2.891,  0.998,
    53027L,  93.714,  24.556, "away",        25L,  21.233,  36.14,  0.131, -0.214,
    53027L,  93.714,  24.556, "away",        26L,      NA,     NA,     NA,     NA
  )


tracking_end <-
  tibble::tribble(
    ~frame, ~ball_x, ~ball_y,  ~side, ~player_id,      ~x,     ~y,   ~x_v,   ~y_v,
    53045L,  96.459,   36.95, "home",         1L,  94.716, 36.557,  5.299, -3.526,
    53045L,  96.459,   36.95, "home",         2L,  95.938, 26.589,   0.87, -0.435,
    53045L,  96.459,   36.95, "home",         3L,  97.167, 25.643,   1.83,  3.653,
    53045L,  96.459,   36.95, "home",         4L,   93.18, 17.996,  1.219,   3.33,
    53045L,  96.459,   36.95, "home",         5L,  86.792, 25.745,  3.791,   1.62,
    53045L,  96.459,   36.95, "home",         6L,  84.984, 35.293,  3.911, -0.287,
    53045L,  96.459,   36.95, "home",         7L,  86.594, 18.464,   0.87,  1.518,
    53045L,  96.459,   36.95, "home",         8L,   77.45, 49.913,  1.886, -0.328,
    53045L,  96.459,   36.95, "home",         9L,  61.843, 24.825,   0.87, -0.663,
    53045L,  96.459,   36.95, "home",        10L,  60.302, 34.679,   0.84, -0.566,
    53045L,  96.459,   36.95, "home",        11L, 102.456, 33.149, -0.101,  1.396,
    53045L,  96.459,   36.95, "home",        12L,      NA,     NA,     NA,     NA,
    53045L,  96.459,   36.95, "home",        13L,      NA,     NA,     NA,     NA,
    53045L,  96.459,   36.95, "home",        14L,      NA,     NA,     NA,     NA,
    53045L,  96.459,   36.95, "away",        15L,  70.325, 11.456,  1.612,  0.891,
    53045L,  96.459,   36.95, "away",        16L,  58.461,  23.17,  2.059, -0.245,
    53045L,  96.459,   36.95, "away",        17L,   55.62, 38.335,  1.144, -0.646,
    53045L,  96.459,   36.95, "away",        18L,  56.507, 52.303,  0.514, -1.222,
    53045L,  96.459,   36.95, "away",        19L,  88.761, 18.115,   1.32,  2.868,
    53045L,  96.459,   36.95, "away",        20L,  74.045, 28.987,  2.831,  0.566,
    53045L,  96.459,   36.95, "away",        21L,  62.541, 38.972,  1.012, -0.702,
    53045L,  96.459,   36.95, "away",        22L,  82.102, 44.844,  3.634, -0.328,
    53045L,  96.459,   36.95, "away",        23L,  94.391, 25.988,    0.3,  1.401,
    53045L,  96.459,   36.95, "away",        24L,  96.478, 37.038,  1.894,  0.146,
    53045L,  96.459,   36.95, "away",        25L,   21.27, 36.108,  0.041,      0,
    53045L,  96.459,   36.95, "away",        26L,      NA,     NA,     NA,     NA
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
target_x <- 94
target_y <- 63
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
pis <- c(5L, 6L) %>% map_dfr(f_get_p_intercepts)

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
    lab = glue::glue('t = 5 s, p_intercept = {p_intercept_5}
                         t = 6 s, p_intercept = {p_intercept_6}'),
    lab_norm = glue::glue('t = 5 s, p_intercept = {p_intercept_norm_5}
                              t = 6 s, p_intercept = {p_intercept_norm_6}')
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
  .gg_pitch() +
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
    label.fontsize = 14,
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
  .gg_pitch() +
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
    label.fontsize = 14,
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
  .gg_pitch() +
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
    label.fontsize = 14,
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
#   .gg_pitch() +
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
  .gg_pitch() +
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
  .gg_pitch() +
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

tracking_start %>% 
  clipr::write_clip()
knitr::wr