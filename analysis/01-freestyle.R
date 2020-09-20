
library(tidyverse)
game_id <- 2

events <- import_event_data(game_id = game_id, postprocess = TRUE)
tracking_home <- import_tracking_data_timed(game_id = game_id, side = 'home', overwrite = F)
tracking_away <- import_tracking_data_timed(game_id = game_id, side = 'away', overwrite = F)
tracking <- bind_rows(tracking_home, tracking_away)
tracking

# for pc speed study ----
event_id <- 823L
events1 <- 
  events %>% 
  filter(event_id == !!event_id) %>% 
  mutate(frame = start_frame)
events1

tracking1 <-
  tracking %>% 
  inner_join(events1 %>% select(frame))
tracking1

# 
tracking %>% 
  inner_join(events1 %>% select(frame = end_frame)) %>% 
  distinct(ball_x, ball_y) %>% 
  deframe()

path_events1 <- fs::path('output', 'events1.csv')
# write_csv(events1, path = path_events1, na = '')
path_tracking1 <- fs::path('output', 'tracking1.csv')
# write_csv(tracking1, path = path_tracking1, na = '')

# plot event sequence ----
start_event_id <- 817L
n_event <- 7L
end_event_id <- start_event_id + n_event
events_filt <- 
  events %>% 
  filter(between(event_id, !!start_event_id, !!end_event_id)) %>% 
  mutate(frame = start_frame)
events_filt
# events_filt %>% glimpse()
#events_filt %>% select(matches('_[xy]$')) %>% skimr::skim()

arw <- arrow(length = unit(5, 'pt'), type = 'closed')
# ggplot() + geom_segment(data = tibble(x = 1, y = 1), aes(x = x, y = y, xend = x + 1, yend = y + 1), arrow = arw)
tracking_filt <-
  tracking %>% 
  inner_join(events_filt %>% select(frame))
tracking_filt

viz <-
  tracking_filt %>% 
  ggplot() +
  aes(x = x, y = y) +
  .pitch_gg() +
  geom_point(
    data = tracking_filt %>% filter(!is.na(x)) %>% filter(player == 11L),
    aes(fill = side),
    size = 3,
    color = 'black',
    shape = 21
  ) +
  scale_color_manual(values = c('home' = 'blue', 'away' = 'red')) +
  facet_wrap(~frame) +
  geom_point(
    data = events_filt,
    # aes(fill = side),
    aes(x = start_x, y = start_y),
    size = 3,
    fill = 'yellow',
    color = 'black',
    shape = 21
  ) +
  geom_segment(
    data = events_filt,
    # aes(fill = side),
    aes(x = start_x, y = start_y, xend = end_x, yend = end_y),
    size = 1,
    arrow = arw
  )
viz  

# plot pc ----
event_id <- 823L
pc <-
  do_calculate_pc_for_event(
    tracking = tracking,
    events = events,
    event_id = event_id
  ) %>% 
  select(-pc, -res)
pc

pc_slim <- pc %>% select(-pc, -res)
pc_slim

pal <- colorRampPalette(c('red', 'white', 'blue'))(10)

pc_slim %>% 
  ggplot() +
  aes(x = x - 1, y = y - 1, z = ppcf_att) +
  .pitch_gg() +
  geom_contour_filled(aes(fill = ..level.., color = ..level..), alpha = 0.7) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal)

events1 <- 
  events %>% 
  filter(event_id == !!event_id) %>% 
  mutate(frame = start_frame)
events1

arw <- arrow(length = unit(5, 'pt'), type = 'closed')

tracking1 <-
  tracking %>% 
  inner_join(events1 %>% select(frame))
tracking1

viz <-
  pc_slim %>% 
  ggplot() +
  aes(x, y) +
  .pitch_gg() +
  geom_tile(aes(fill = ppcf_att), alpha = 0.7) +
  scale_fill_gradient2(
    low = 'red', 
    high = 'blue', 
    midpoint = 0.5
  ) +
  guides(fill = FALSE) +
  ggnewscale::new_scale_fill() +
  geom_point(
    data = tracking1,
    aes(fill = side),
    size = 3,
    color = 'black',
    shape = 21
  ) +
  scale_fill_manual(values = c('home' = 'red', 'away' = 'blue')) +
  # facet_wrap(~frame) +
  geom_point(
    data = events1,
    # aes(fill = side),
    aes(x = start_x, y = start_y),
    size = 1,
    fill = 'yellow',
    color = 'black',
    shape = 23
  )
viz

# epv added ----
epv_grid <- import_epv_grid()
epv_grid
epv_grid %>% plot_epv()
xt_grid <- import_xt_grid()
xt_grid %>% plot_epv()
epv_grid %>% .filter_epv_grid(start_x, start_y)
epv_grid %>% .filter_epv_grid(end_x, end_y)
epv_grid %>% .filter_epv_grid(end_x + 2, end_y + 2)
epv_grid %>% .filter_epv_grid(end_x - 1, end_y  - 1)

epv_grid %>% 
  mutate(rx = dense_rank(x), ry = dense_rank(y)) %>% 
  mutate(
    # dx = sqrt(x^2 + start_x^2),
    # dy = sqrt(y^2 + start_y^2)
    dx = abs(x - !!start_x),
    dy = abs(y - !!start_y)
  ) %>% 
  mutate(dz = sqrt(dx^2 + dy^2)) %>% 
  filter(dz == min(dz)) %>% 
  select(-dx, -dy, -dz)

epv_grid %>% 
  mutate(rx = dense_rank(x), ry = dense_rank(y)) %>% 
  filter(rx == 11L, ry == 44L)
