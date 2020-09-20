
game_id <- 2
events <- import_event_data(game_id = game_id, postprocess = TRUE)

.event_id <- 823L
# .start_frame <- 52941L
event <- events %>% filter(event_id == .event_id)
.start_frame <- event[['start_frame']]
# tracking %>% filter(frame == .start_frame) %>% select(-period, -frame, -time, -team) %>% mutate(across(where(is.double), ~round(.x, 3))) %>% clipr::write_clip()
tracking1 %>% select(-period, -frame, -time, -team, -ball_x, -ball_y) %>% mutate(across(where(is.double), ~round(.x, 3))) %>% clipr::write_clip()
# tony data
tracking <-
tibble::tribble(
  ~ball_x, ~ball_y,  ~side, ~player,     ~x,     ~y,  ~x_v,   ~y_v,
   94.606,  43.444, "home",      1L, 91.587,  28.63, 5.963,  3.985,
   94.606,  43.444, "home",      2L, 96.003, 40.855, 1.514,  2.023,
   94.606,  43.444, "home",      3L, 96.922, 44.682, 1.431, -2.395,
   94.606,  43.444, "home",      4L, 93.273, 52.362, 1.015, -3.473,
   94.606,  43.444, "home",      5L, 84.758, 43.309, 4.278,   -1.2,
   94.606,  43.444, "home",      6L, 82.977, 32.371,  3.93,  0.619,
   94.606,  43.444, "home",      7L, 86.609, 50.665, 1.719, -1.523,
   94.606,  43.444, "home",      8L, 76.783, 17.838, 2.037,  0.493,
   94.606,  43.444, "home",      9L, 61.802, 42.652, 0.871,   0.77,
   94.606,  43.444, "home",     10L, 60.259, 32.905, 0.909,  0.573,
   94.606,  43.444, "home",     11L, 103.52, 35.716, -0.31, -0.624,
   94.606,  43.444, "home",     12L,     NA,     NA,    NA,     NA,
   94.606,  43.444, "home",     13L,     NA,     NA,    NA,     NA,
   94.606,  43.444, "home",     14L,     NA,     NA,    NA,     NA,
   94.606,  43.444, "away",     15L, 69.712, 57.168, 2.162, -0.852,
   94.606,  43.444, "away",     16L, 57.485, 44.588, 2.253,  0.495,
   94.606,  43.444, "away",     17L, 55.302, 29.148, 1.306,  0.748,
   94.606,  43.444, "away",     18L, 56.676, 14.802, 0.488,  1.462,
   94.606,  43.444, "away",     19L,  88.57, 52.091,  1.45, -3.172,
   94.606,  43.444, "away",     20L, 72.653, 39.454,  2.96, -0.821,
   94.606,  43.444, "away",     21L, 62.348, 28.499, 1.295,  0.775,
   94.606,  43.444, "away",     22L,  80.21, 22.856, 3.691,  0.658,
   94.606,  43.444, "away",     23L,  94.67, 43.338, 1.685, -2.883,
   94.606,  43.444, "away",     24L,  95.79,  31.25, 2.919, -0.998,
   94.606,  43.444, "away",     25L, 21.435,  31.86, 0.133,  0.214,
   94.606,  43.444, "away",     26L,     NA,     NA,    NA,     NA
  ) %>% 
  mutate(frame = .start_frame)

viz <-
  tracking %>% 
  ggplot() +
  aes(x = x, y = y) +
  .pitch_gg() +
  geom_point(
    data = tracking %>% filter(!is.na(x)),
    aes(fill = side),
    size = 3,
    color = 'black',
    shape = 21
  ) +
  geom_point(
    data = tracking %>% distinct(x = ball_x, y = ball_y),
    # aes(fill = side),
    size = 3,
    fill = 'yellow',
    color = 'black',
    shape = 21
  )
viz

# # match up with python data
# tracking <-
# tibble::tribble(
#    ~ball_x,  ~ball_y,  ~side, ~player,        ~x,        ~y,    ~x_v,   ~y_v,
#   41.60606, -9.44384, "away",     15L,  16.71196, -23.16828,   2.067,  0.782,
#   41.60606, -9.44384, "away",     16L,   4.48486,  -10.5876,   2.173, -0.527,
#   41.60606, -9.44384, "away",     17L,   2.30232,    4.8518,   1.272,  -0.68,
#   41.60606, -9.44384, "away",     18L,   3.67608,  19.19776,  0.4505, -1.547,
#   41.60606, -9.44384, "away",     19L,  35.57042, -18.09072,   1.484,  3.196,
#   41.60606, -9.44384, "away",     20L,  19.65346,   -5.4536,   2.968,  0.748,
#   41.60606, -9.44384, "away",     21L,   9.34814,   5.50052,  1.1925, -0.765,
#   41.60606, -9.44384, "away",     22L,   27.2102,  11.14384,  3.6835, -0.816,
#   41.60606, -9.44384, "away",     23L,  41.66966,  -9.33776,   1.643,   2.72,
#   41.60606, -9.44384, "away",     24L,  42.79008,   2.74992,   2.703,  0.799,
#   41.60606, -9.44384, "away",     25L, -31.56468,   2.13996,   0.106,  -0.17,
#   41.60606, -9.44384, "away",     26L,        NA,        NA,      NA,     NA,
#   41.60606, -9.44384, "home",      1L,  38.58718,   5.36996,   5.989, -4.012,
#   41.60606, -9.44384, "home",      2L,  43.00314,  -6.85508,   1.378, -1.734,
#   41.60606, -9.44384, "home",      3L,  43.92216, -10.68212,  1.3515,  2.499,
#   41.60606, -9.44384, "home",      4L,  40.27258, -18.36204,  0.9275,  3.315,
#   41.60606, -9.44384, "home",      5L,   31.7576,  -9.30852,    4.24,  1.292,
#   41.60606, -9.44384, "home",      6L,   29.9768,    1.6286,   3.922, -0.663,
#   41.60606, -9.44384, "home",      7L,  33.60942, -16.66544,  1.5635,  1.513,
#   41.60606, -9.44384, "home",      8L,  23.78322,  16.16224,  1.9875, -0.527,
#   41.60606, -9.44384, "home",      9L,   8.80224,  -8.65164,   0.848, -0.901,
#   41.60606, -9.44384, "home",     10L,   7.25888,   1.09548,   0.901, -0.544,
#   41.60606, -9.44384, "home",     11L,   50.5196,  -1.71564, -0.1325,  0.918,
#   41.60606, -9.44384, "home",     12L,        NA,        NA,      NA,     NA,
#   41.60606, -9.44384, "home",     13L,        NA,        NA,      NA,     NA,
#   41.60606, -9.44384, "home",     14L,        NA,        NA,      NA,     NA
#   ) %>% 
#   mutate(frame = .start_frame)


players <-
  tracking %>%
  pull(player) %>%
  map(~player(player_id = .x, start_frame = .start_frame, events = events, tracking = tracking))
players

# ball_x <- tracking[1, ][['ball_x']]
# ball_y <- tracking[1, ][['ball_y']]
ball_x <- 41.34
ball_y <- -9.52
pass_start_x <- ball_x
pass_start_y <- ball_y
pass_end_x <- 44.52
pass_end_y <- 2.72
res_start <-
  calculate_pc_at_target(
    players = players,
    ball_x = ball_x,
    ball_y = ball_y,
    target_x = pass_start_x,
    target_y = pass_start_y
  )
res_start

res_end <-
  calculate_pc_at_target(
    players = players,
    ball_x = ball_x,
    ball_y = ball_y,
    target_x = pass_end_x,
    target_y = pass_end_y
  )
res_end
