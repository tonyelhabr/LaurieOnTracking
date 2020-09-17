
library(tidyverse)
game_id <- 2

events <- import_event_data(game_id = game_id, postprocess = TRUE)
tracking_home <- import_tracking_data_timed(game_id = game_id, side = 'home', overwrite = F)
tracking_away <- import_tracking_data_timed(game_id = game_id, side = 'away', overwrite = F)
tracking <- bind_rows(tracking_home, tracking_away)
tracking

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
    data = tracking_filt %>% filter(!is.na(x)),
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

event_id <- 823L
pc <-
  do_calculate_pc_for_event(
    tracking = tracking,
    events = events,
    event_id = event_id
  )
pc

pc_slim <- pc %>% select(-pc, -res)
pc_slim

pc_x <- pc_slim %>% pull(x) # distinct(x) %>% pull(x)
pc_y <- pc_slim %>% pull(y) # distinct(y) %>% pull(y)
pc_x_rng <- range(pc_x)
pc_y_rng <- range(pc_y)
bw_x <- MASS::bandwidth.nrd(pc_x)
bw_y <- MASS::bandwidth.nrd(pc_y)
bw <- c(bw_x, bw_y)
dxy <- MASS::kde2d(pc_x, pc_y, n = 100L, lims = c(pc_x_rng, pc_y_rng))
dxy

pc_z <-
  crossing(x = dxy$x, y = dxy$y) %>% 
  mutate(z = as.vector(dxy$z))

pc_slim %>% 
  ggplot(aes(x = x, y = y, z = ppcf_att)) +
  stat_contour(geom = 'polygon', aes(fill = ..level.., color = ..level..)) +
  # geom_tile(aes(fill = ppcf_att)) +
  stat_contour(bins = 15)

pc_slim %>% 
  ggplot(aes(x = x, y = y, z = ppcf_att)) +
  geom_contour_filled(aes(fill = ..level.., color = ..level..)) +
  # scale_fill_gradient2(
  #   low = 'red',
  #   high = 'blue',
  #   midpoint = 0.5
  # )
  scale_fill_brewer(palette = 'RdBu') +
  scale_color_brewer(palette = 'RdBu')
  # scale_color_gradient2(
  #   low = 'red', 
  #   high = 'blue', 
  #   midpoint = 0.5
  # ) 


pc_z <-
  dxy$z %>% 
  as_tibble() %>% 
  mutate(x = dxy$x) %>% 
  pivot_longer(-x, names_to = 'y', values_to = 'z') %>% 
  mutate(
    y = y %>% str_remove('^V') %>% as.double()
  )
pc_z
pc_z %>% skimr::skim() 

pc_z %>% 
  ggplot() +
  aes(x = x, y = y) +
  # .pitch_gg() +
  geom_raster(aes(x = x, y = y, fill = z)) +
  stat_contour(aes(color = ..level.., z = z)) +
  # scale_fill_gradient2(low="blue",mid="white", high="red", midpoint=0.0008) +
  # scale_color_gradient2(low="blue", mid="white", high="red", midpoint=0.0008) +
  # drop the legends
  guides(color=FALSE, fill = FALSE)


events1 <- 
  events %>% 
  filter(event_id == !!start_event_id) %>% 
  mutate(frame = start_frame)
events1

arw <- arrow(length = unit(5, 'pt'), type = 'closed')

tracking1 <-
  tracking %>% 
  inner_join(events1 %>% select(frame))
tracking1

viz <-
  pc %>% 
  select(-pc, -res) %>% 
  ggplot() +
  aes(x, y) +
  .pitch_gg() +
  # geom_point(aes(size = abs(ppcf_diff), color = ppcf_diff))
  geom_tile(aes(fill = ppcf_att), alpha = 0.7) +
  # .pitch_gg(
  #   pitch = .get_pitch(pitch_fill = NA, limits = TRUE)
  #   # pitch = .get_pitch()
  # ) +
  # scale_fill_viridis_c(option = 'B') +
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

