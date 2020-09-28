
.event_id <- 823L
events_filt <-
  tibble::tribble(
    ~event_id,  ~side,  ~type, ~start_frame, ~end_frame, ~start_x, ~start_y, ~end_x, ~end_y,
    823L, "away", "pass",       53027L,     53045L,    93.45,    24.48,   96.6,  36.72
  )
tracking_start <-
  tibble::tribble(
    ~frame,   ~time, ~ball_x, ~ball_y,  ~side, ~player_id,      ~x,     ~y,   ~x_v,   ~y_v, ~next_x, ~next_y, ~next_time,
    53027L, 2121.08,  93.714,  24.556, "home",         1L,  90.723,  39.37,  5.906, -3.985,   90.96,  39.208,    2121.12,
    53027L, 2121.08,  93.714,  24.556, "home",         2L,  95.097, 27.145,    1.5, -2.023,  95.155,  27.084,    2121.12,
    53027L, 2121.08,  93.714,  24.556, "home",         3L,  96.008, 23.318,  1.418,  2.395,  96.061,  23.418,    2121.12,
    53027L, 2121.08,  93.714,  24.556, "home",         4L,  92.393, 15.638,  1.005,  3.473,   92.43,  15.771,    2121.12,
    53027L, 2121.08,  93.714,  24.556, "home",         5L,  83.958, 24.691,  4.238,    1.2,  84.125,  24.744,    2121.12,
    53027L, 2121.08,  93.714,  24.556, "home",         6L,  82.194, 35.629,  3.893, -0.619,  82.349,  35.601,    2121.12,
    53027L, 2121.08,  93.714,  24.556, "home",         7L,  85.792, 17.335,  1.703,  1.523,  85.853,    17.4,    2121.12,
    53027L, 2121.08,  93.714,  24.556, "home",         8L,  76.059, 50.162,  2.018, -0.493,  76.138,  50.148,    2121.12,
    53027L, 2121.08,  93.714,  24.556, "home",         9L,  61.219, 25.348,  0.863,  -0.77,  61.252,  25.312,    2121.12,
    53027L, 2121.08,  93.714,  24.556, "home",        10L,   59.69, 35.095,    0.9, -0.573,  59.727,  35.078,    2121.12,
    53027L, 2121.08,  93.714,  24.556, "home",        11L, 102.543, 32.284, -0.308,  0.624, 102.542,  32.329,    2121.12,
    53027L, 2121.08,  93.714,  24.556, "home",        12L,      NA,     NA,     NA,     NA,      NA,      NA,    2121.12,
    53027L, 2121.08,  93.714,  24.556, "home",        13L,      NA,     NA,     NA,     NA,      NA,      NA,    2121.12,
    53027L, 2121.08,  93.714,  24.556, "home",        14L,      NA,     NA,     NA,     NA,      NA,      NA,    2121.12,
    53027L, 2121.08,  93.714,  24.556, "away",        15L,  69.054, 10.832,  2.141,  0.852,  69.133,  10.861,    2121.12,
    53027L, 2121.08,  93.714,  24.556, "away",        16L,  56.943, 23.412,  2.231, -0.495,  57.031,  23.391,    2121.12,
    53027L, 2121.08,  93.714,  24.556, "away",        17L,  54.781, 38.852,  1.294, -0.748,  54.829,  38.825,    2121.12,
    53027L, 2121.08,  93.714,  24.556, "away",        18L,  56.141, 53.198,  0.484, -1.462,  56.161,  53.147,    2121.12,
    53027L, 2121.08,  93.714,  24.556, "away",        19L,  87.735, 15.909,  1.436,  3.172,  87.795,  16.037,    2121.12,
    53027L, 2121.08,  93.714,  24.556, "away",        20L,  71.968, 28.546,  2.933,  0.821,  72.086,  28.572,    2121.12,
    53027L, 2121.08,  93.714,  24.556, "away",        21L,   61.76, 39.501,  1.283, -0.775,  61.806,  39.469,    2121.12,
    53027L, 2121.08,  93.714,  24.556, "away",        22L,  79.454, 45.144,  3.656, -0.658,  79.603,  45.123,    2121.12,
    53027L, 2121.08,  93.714,  24.556, "away",        23L,  93.777, 24.662,  1.669,  2.883,   93.84,  24.767,    2121.12,
    53027L, 2121.08,  93.714,  24.556, "away",        24L,  94.886,  36.75,  2.891,  0.998,  94.996,  36.787,    2121.12,
    53027L, 2121.08,  93.714,  24.556, "away",        25L,  21.233,  36.14,  0.131, -0.214,  21.237,  36.133,    2121.12,
    53027L, 2121.08,  93.714,  24.556, "away",        26L,      NA,     NA,     NA,     NA,      NA,      NA,    2121.12
  )

tracking_end <-
  tibble::tribble(
    ~frame,  ~time, ~ball_x, ~ball_y,  ~side, ~player_id,      ~x,     ~y,   ~x_v,   ~y_v, ~next_x, ~next_y, ~next_time,
    53045L, 2121.8,  96.459,   36.95, "home",         1L,  94.716, 36.557,  5.299, -3.526,  94.923,  36.442,    2121.84,
    53045L, 2121.8,  96.459,   36.95, "home",         2L,  95.938, 26.589,   0.87, -0.435,  95.968,  26.586,    2121.84,
    53045L, 2121.8,  96.459,   36.95, "home",         3L,  97.167, 25.643,   1.83,  3.653,  97.251,  25.801,    2121.84,
    53045L, 2121.8,  96.459,   36.95, "home",         4L,   93.18, 17.996,  1.219,   3.33,  93.226,  18.124,    2121.84,
    53045L, 2121.8,  96.459,   36.95, "home",         5L,  86.792, 25.745,  3.791,   1.62,  86.941,  25.811,    2121.84,
    53045L, 2121.8,  96.459,   36.95, "home",         6L,  84.984, 35.293,  3.911, -0.287,  85.138,  35.279,    2121.84,
    53045L, 2121.8,  96.459,   36.95, "home",         7L,  86.594, 18.464,   0.87,  1.518,  86.625,  18.527,    2121.84,
    53045L, 2121.8,  96.459,   36.95, "home",         8L,   77.45, 49.913,  1.886, -0.328,  77.525,  49.902,    2121.84,
    53045L, 2121.8,  96.459,   36.95, "home",         9L,  61.843, 24.825,   0.87, -0.663,  61.879,  24.807,    2121.84,
    53045L, 2121.8,  96.459,   36.95, "home",        10L,  60.302, 34.679,   0.84, -0.566,  60.334,  34.665,    2121.84,
    53045L, 2121.8,  96.459,   36.95, "home",        11L, 102.456, 33.149, -0.101,  1.396, 102.442,  33.208,    2121.84,
    53045L, 2121.8,  96.459,   36.95, "home",        12L,      NA,     NA,     NA,     NA,      NA,      NA,    2121.84,
    53045L, 2121.8,  96.459,   36.95, "home",        13L,      NA,     NA,     NA,     NA,      NA,      NA,    2121.84,
    53045L, 2121.8,  96.459,   36.95, "home",        14L,      NA,     NA,     NA,     NA,      NA,      NA,    2121.84,
    53045L, 2121.8,  96.459,   36.95, "away",        15L,  70.325, 11.456,  1.612,  0.891,  70.385,  11.493,    2121.84,
    53045L, 2121.8,  96.459,   36.95, "away",        16L,  58.461,  23.17,  2.059, -0.245,  58.542,  23.162,    2121.84,
    53045L, 2121.8,  96.459,   36.95, "away",        17L,   55.62, 38.335,  1.144, -0.646,  55.665,  38.311,    2121.84,
    53045L, 2121.8,  96.459,   36.95, "away",        18L,  56.507, 52.303,  0.514, -1.222,  56.527,  52.256,    2121.84,
    53045L, 2121.8,  96.459,   36.95, "away",        19L,  88.761, 18.115,   1.32,  2.868,   88.81,  18.225,    2121.84,
    53045L, 2121.8,  96.459,   36.95, "away",        20L,  74.045, 28.987,  2.831,  0.566,  74.157,  29.012,    2121.84,
    53045L, 2121.8,  96.459,   36.95, "away",        21L,  62.541, 38.972,  1.012, -0.702,  62.579,  38.947,    2121.84,
    53045L, 2121.8,  96.459,   36.95, "away",        22L,  82.102, 44.844,  3.634, -0.328,  82.242,  44.834,    2121.84,
    53045L, 2121.8,  96.459,   36.95, "away",        23L,  94.391, 25.988,    0.3,  1.401,  94.381,  26.028,    2121.84,
    53045L, 2121.8,  96.459,   36.95, "away",        24L,  96.478, 37.038,  1.894,  0.146,  96.545,  37.043,    2121.84,
    53045L, 2121.8,  96.459,   36.95, "away",        25L,   21.27, 36.108,  0.041,      0,  21.271,  36.108,    2121.84,
    53045L, 2121.8,  96.459,   36.95, "away",        26L,      NA,     NA,     NA,     NA,      NA,      NA,    2121.84
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

# tracking_filt <- fs::path(.get_dir_data(), 'tracking_filt.fst') %>% feather::read_feather()
# tracking_filt

dir_data_dump <- fs::path(.get_dir_output(), 'dump')
fs::dir_create(dir_data_dump)
# pc <- 
#   tracking_start %>% 
#   mutate(
#     data = 
#       pmap(
#         list(
#           time = time, 
#           next_time = next_time, 
#           ball_x = ball_x, 
#           ball_y = ball_y, 
#           x = x, 
#           y = y, 
#           next_x = next_x, 
#           next_y = next_y, 
#           team = side, 
#           player = player_id,
#           dir = dir_data_dump,
#           # pitch_grid = .get_pitch_grid(),
#           overwrite = FALSE
#         ), 
#         do_calculate_pc
#       )
#   )
xt_grid <- import_xt_grid()
epv_grid <- import_epv_grid()
# pitch_grid <- .get_pitch_grid(n_cell_x = 50L, n_cell_y = 50L)
pitch_grid <- .get_pitch_grid(n_cell_x = 100L, n_cell_y = 100L)
# xt_grid %>% summarize(across(c(x, y), range))
# epv_grid %>% summarize(across(c(x, y), range))
# pitch_grid %>% summarize(across(c(x, y), range))
# pitch_grid <- epv_grid %>% distinct(x, y) %>% arrange(x, y)
# pitch_grid <- xt_grid %>% distinct(x, y) %>% arrange(x, y)
pc <-
  tracking_start %>% 
  drop_na() %>% 
  mutate(
    speed_x = .get_speed(x, next_x, time, next_time),
    speed_y = .get_speed(y, next_y, time, next_time),
    srat = .get_srat(speed_x, speed_y),
    theta = .get_theta(speed_x, speed_y),
    mu_x = .get_mu(x, speed_x),
    mu_y = .get_mu(y, speed_y),
    ri = .get_ri(x, y, ball_x, ball_y),
    R = map(theta, .get_R),
    S = map2(ri, srat, .get_S),
    Sigma = map2(R, S, .get_Sigma),
    I = pmap(list(x, y, mu_x, mu_y, Sigma), .calculate_I, pitch_grid = !!pitch_grid)
  ) %>% select(any_of(names(tracking_start)), I)
pc

# pc_agg <-
#   pc_slim %>%
#   nest(data = -c(frame, time, ball_x, ball_y)) %>% 
#   mutate(res = map(data, ~do_aggregate_pc(pc = ..1, dir = dir_data_dump, export = F, overwrite = TRUE))) %>% 
#   select(res) %>% 
#   unnest(res)
# pc_agg

pc_agg <-
  pc %>% 
  select(frame, time, player_id, side, player_x = x, player_y = y, I) %>% 
  mutate(pitch_grid = list(!!pitch_grid)) %>% 
  unnest(cols = c(I, pitch_grid)) %>% 
  group_by(frame, time, side, x, y) %>%
  summarise(side_sum = sum(I, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = side, values_from = side_sum) %>% 
  mutate(
    pc_orig = 1 / (1 + exp(home - away)) %>% round(3),
    pc_modified = (away / (home + away)) %>% round(3)
  )
pc_agg

viz_fb <-
  pc_agg %>% 
  ggplot() +
  aes(x = x, y = y) +
  .gg_pitch() +
  geom_raster(
    aes(fill = pc_modified),
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
viz_fb

data_fb_viz <- viz_fb %>% ggplot_build() %>% pluck('data') # %>% pluck(1) %>% as_tibble()
data_fb_viz

pc_grid_epv_start <-
  do_calculate_pc_for_event(
    tracking = tracking_start,
    events = events_filt %>% mutate(frame = start_frame),
    event_id = .event_id,
    epv_grid = epv_grid
  )
pc_grid_epv_start


pc_agg
pc_grid_epv_start_aug <- pc_grid_epv_start %>% mutate(idx = row_number())
f <- function(grid_compare, x, y) {
  grid_compare %>% 
    mutate(
      # dx = sqrt(x^2 + start_x^2),
      # dy = sqrt(y^2 + start_y^2)
      dx = abs(x - !!x),
      dy = abs(y - !!y)
    ) %>% 
    mutate(dz = sqrt(dx^2 + dy^2)) %>% 
    filter(dz == min(dz)) %>% 
    select(-dx, -dy, -dz) %>% 
    select(x2 = x, y2 = y)
}

# pitch_matched <-
#   epv_grid %>% 
#   select(x1 = x, y1 = y) %>% 
#   mutate(
#     pair = map2(x1, y1, ~f(grid_compare = !!pitch_grid, x = ..1, y = ..2))
#   ) %>% 
#   unnest(pair)
# pitch_matched

pitch_matched <-
  pitch_grid %>% 
  select(x1 = x, y1 = y) %>% 
  mutate(
    pair = map2(x1, y1, ~f(grid_compare = !!epv_grid, x = ..1, y = ..2))
  ) %>% 
  unnest(pair)
pitch_matched

# pc_diff <-
#   pitch_matched %>% 
#   left_join(pc_grid_epv_start %>% rename(x1 = x, y1 = y)) %>% 
#   left_join(pc_agg %>% rename(x2 = x, y2 = y)) %>% 
#   mutate(pcd = ppcf_att - pc_modified) %>% 
#   select(x1, y1, x2, y2, pc1 = ppcf_att, pc2 = pc_modified, pcd)
# pc_diff

pc_diff <-
  pitch_matched %>% 
  left_join(pc_grid_epv_start %>% rename(x2 = x, y2 = y)) %>% 
  left_join(pc_agg %>% rename(x1 = x, y1 = y)) %>% 
  mutate(pcd = ppcf_att - pc_modified) %>% 
  select(x1, y1, x2, y2, pc1 = ppcf_att, pc2 = pc_modified, pcd)
pc_diff

pc_diff %>% 
  ggplot() +
  aes(x = x1, y = y1) +
  .gg_pitch() +
  geom_raster(
    aes(fill = pcd),
    interpolate = TRUE,
    hjust = 0.5,
    vjust = 0.5,
    alpha = 0.4
  ) +
  # scale_fill_gradient2(low = 'purple', high = 'yellow', midpoint = 0) +
  # scale_color_gradient2(low = 'purple', high = 'yellow', midpoint = 0) +
  guides(fill = FALSE) +
  ggnewscale::new_scale_color()

# interpolate attempt ----
# # fmla <- formula(pc_modified ~ I((I(x^2) + I(y^2))^0.5)),
# fmla <- formula(pc_modified ~ I(x^2) + I(y^2) + I(x^2):I(y^2) + 0)
# # fit_fb <- pc_agg %>% lm(fmla, data = .)
# fit_fb <- pc_agg %>% mutate(across(pc_modified, ~if_else(.x > 0.5, 1, 2) %>% factor())) %>% glm(fmla, data = ., family = 'binomial')
# fit_fb
# 
# pred_fb <- fit_fb %>% broom::augment(newdata = epv_grid, type.predict = 'response')
# pred_fb
# # pred_fb %>% skimr::skim(.fitted)
# 
# pred_fb %>% 
#   ggplot() +
#   aes(x = x, y = y) +
#   .gg_pitch() +
#   geom_raster(
#     aes(fill = .fitted),
#     interpolate = TRUE,
#     hjust = 0.5,
#     vjust = 0.5,
#     alpha = 0.4
#   ) +
#   scale_fill_gradient2(low = pal2[['home']], high = pal2[['away']], midpoint = 0.5) +
#   scale_color_gradient2(low = pal2[['home']], high = pal2[['away']], midpoint = 0.5) +
#   guides(fill = FALSE) +
#   ggnewscale::new_scale_color() +
#   # labs(title = glue::glue('Metrica Sample Game 2, Event {.event_id}, Pitch Control')) +
#   .gg_constants()
# 
# x1 <- pc_agg %>% distinct(x) %>% pull(x)
# y1 <- pc_agg %>% distinct(y) %>% pull(y)
# x2 <- xt_grid %>% distinct(x) %>% pull(x)
# y2 <- xt_grid %>% distinct(y) %>% pull(y)
# # z1 <- pc_agg %>% select(x, y, pc_modified) %>% pivot_wider(names_from = x, values_from = pc_modified) %>% select(-y) %>% as.matrix()
# # z1 %>% dim()
# # y1 %>% length()
# # z2 <- pracma::interp2(x1, y1, z1, x2, y2, method = 'linear')
# # z2
# x_rng <- c(min(x2), max(x2))
# y_rng <- c(min(y2), max(y2))
# x_rng <- range(c(x1, x2))
# y_rng <- range(c(y1, y2))
# bw_x <- MASS::bandwidth.nrd(x_rng)
# bw_y <- MASS::bandwidth.nrd(y_rng)
# bw <- c(bw_x, bw_y)
# bw
# n <- 200L
# lims <- c(x_rng, y_rng)
# d21 <- MASS::kde2d(x1, y1, h = bw, n = n, lims = lims)
# d22 <- MASS::kde2d(x2, y2, h = bw, n = n, lims = lims)
# res <- d21
# res$z <- d21$z - d22$z
# colnames(res$z) <- res$y
# res$z
# res_res <-
#   res$z %>% 
#   as_tibble() %>% 
#   mutate(x = res$x) %>% 
#   pivot_longer(-x, names_to = 'y', values_to = "z") %>% 
#   mutate(
#     y = as.double(y)
#   )
# res_res
# viz_diff <-
#   res_res %>% 
#   ggplot() +
#   aes(x = x, y = y) +
#   .gg_pitch() +
#   geom_raster(
#     aes(fill = z),
#     interpolate = TRUE,
#     hjust = 0.5,
#     vjust = 0.5,
#     alpha = 0.4
#   ) +
#   scale_fill_gradient2(low = pal2[['home']], high = pal2[['away']], midpoint = 0.5) +
#   scale_color_gradient2(low = pal2[['home']], high = pal2[['away']], midpoint = 0.5) +
#   guides(fill = FALSE) +
#   ggnewscale::new_scale_color() +
#   # labs(title = glue::glue('Metrica Sample Game 2, Event {.event_id}, Pitch Control')) +
#   .gg_constants()
# viz_diff
df <- pc_grid_epv_start
rec <- 
  df %>% 
  recipes::recipe(formula(ppcf_att ~ x + y), data = .)
rec

spec <-
  parsnip::nearest_neighbor(neighbors = 8) %>% 
  # parsnip::svm_rbf(mode = 'regression') %>% 
  parsnip::set_engine('kknn')
spec

wf <-
  workflows::workflow() %>%
  workflows::add_recipe(rec) %>%
  workflows::add_model(spec)
wf

fit <-
  parsnip::fit(wf, df) %>% 
  workflows::pull_workflow_fit()
fit

pred <- fit %>% predict(new_data = pitch_grid) %>% bind_cols(pitch_grid)
pred

df %>% 
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

pred %>% 
  ggplot() +
  aes(x = x, y = y) +
  .gg_pitch() +
  geom_raster(
    aes(fill = .pred),
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

f <- function(x, y, .tracking = tracking_start %>% drop_na()) {
  .tracking %>% 
    mutate(
      z = sqrt((x - !!x)^2 + (y - !!y)^2)
    ) %>% 
    filter(z == min(z)) %>% 
    head(1) %>% 
    pull(side)
}

res_vor <-
  epv_grid %>% 
  mutate(
    owned_by = map2_chr(x, y, f)
  ) %>% 
  mutate(value = if_else(owned_by == 'away', 1, 0))
res_vor

res_vor %>% 
  ggplot() +
  aes(x = x, y = y) +
  .gg_pitch() +
  geom_raster(
    aes(fill = value),
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

bound_vor <- c(0, 105, 0, 68)
viz <-
  tracking_start %>% 
  drop_na() %>% 
  ggplot() +
  aes(x = x, y = y) +
  # ggsoccer::annotate_pitch(colour = 'black', fill = 'white') +
  .gg_pitch() +
  geom_point(
    aes(fill = side),
    shape = 21,
    color = 'black',
    size = 4,
    alpha = 1
  ) +
  ggforce::geom_voronoi_tile(
    aes(x = x, y = y, group = -1L, fill = side),
    bound = bound_vor,
    color = 'gray10',
    size = 1,
    alpha = 0.2
  ) 
viz
data_list <- viz %>% ggplot_build() %>% pluck('data')
data_list
data <- data_list %>% pluck(length(data_list)) %>% as_tibble()
data
data %>% 
  ggplot() +
  aes(x = x, y = y) +
  geom_point(aes(color = group)) +
  # scale_color_manual(values = ..identity..) +
  theme_minimal()

data <- tracking_start %>% drop_na()
bound <- c(range(data$x), range(data$y))
vor <- deldir::deldir(data$x, data$y, rw = bound, eps = 1e-9, suppressMsge = TRUE)
to_tile <- function(object) {
  # try_require('deldir', 'to_tile')
  tiles <- rbind(
    structure(object$dirsgs[, c(1:2, 5)], names = c('x', 'y', 'group')),
    structure(object$dirsgs[, c(1:2, 6)], names = c('x', 'y', 'group')),
    structure(object$dirsgs[, c(3:5)], names = c('x', 'y', 'group')),
    structure(object$dirsgs[, c(3:4, 6)], names = c('x', 'y', 'group'))
  )
  tiles <- unique(tiles)
  tiles <- rbind(
    tiles,
    data.frame(
      x = object$rw[c(1, 2, 2, 1)],
      y = object$rw[c(3, 3, 4, 4)],
      group = deldir::get.cnrind(
        object$summary$x,
        object$summary$y,
        object$rw
      )
    )
  )
  tiles$theta <- atan2(
    tiles$y - object$summary$y[tiles$group],
    tiles$x - object$summary$x[tiles$group]
  )
  tiles$theta <- ifelse(tiles$theta > 0, tiles$theta, tiles$theta + 2 * pi)
  tiles[order(tiles$group, tiles$theta), ]
}
clip_tiles <- function(tiles, radius, bound) {
  if (is.null(radius) && is.null(bound)) return(tiles)
  p <- seq(0, 2 * pi, length.out = 361)[-361]
  circ <- list(
    x = cos(p) * radius,
    y = sin(p) * radius
  )
  ggplot2:::dapply(tiles, 'group', function(tile) {
    final_tile <- list(x = tile$x, y = tile$y)
    if (!is.null(radius)) {
      circ_temp <- list(x = circ$x + tile$orig_x[1],
                        y = circ$y + tile$orig_y[1])
      final_tile <- polyclip::polyclip(final_tile, circ_temp, 'intersection')
    }
    if (!is.null(bound)) {
      final_tile <- polyclip::polyclip(final_tile, bound, 'intersection')
    }
    if (length(final_tile) == 0) return(NULL)
    ggplot2:::new_data_frame(list(
      x = final_tile[[1]]$x,
      y = final_tile[[1]]$y,
      group = tile$group[1]
    ))
  })
}
bound <- c(105, 68)
polybound <- as.data.frame(bound)
colnames(bound) <- c('x', 'y')
bound <- c(range(polybound$x), range(polybound$y))

tiles <- to_tile(vor)
tiles$orig_x <- data$x[vor$ind.orig[tiles$group]]
tiles$orig_y <- data$y[vor$ind.orig[tiles$group]]
tiles$group <- data$group[vor$ind.orig[tiles$group]]
max.radius = NULL
polybound = NULL
tiles <- clip_tiles(tiles, radius = max.radius, bound = polybound)
data$x <- NULL
data$y <- NULL
data <- merge(tiles, data, sort = FALSE, all.x = TRUE) %>% as_tibble()
data
data %>% 
  count(player_id)
