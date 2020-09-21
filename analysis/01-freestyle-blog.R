
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

tracking_start <-
  tracking %>% 
  inner_join(events_filt %>% select(frame = start_frame))
tracking_start

tracking_end <-
  tracking %>% 
  inner_join(events_filt %>% select(frame = end_frame))
tracking_end

epv_grid <- import_epv_grid()
epv_grid
xt_grid <- import_xt_grid()
xt_grid
epv_grid %>% plot_epv_grid()
xt_grid %>% plot_epv_grid()

pc_grid_epv_start <-
  do_calculate_pc_for_event(
    tracking = tracking_start,
    events = events_filt %>% mutate(frame = start_frame),
    event_id = .event_id,
    epv_grid = epv_grid
  )
pc_grid_epv_start

pc_grid_xt_start <-
  do_calculate_pc_for_event(
    tracking = tracking_start,
    events = events_filt %>% mutate(frame = start_frame),
    event_id = .event_id,
    epv_grid = xt_grid
  )
pc_grid_xt_start

n_rdbl <- 10L
pal1 <- colorRampPalette(c('red', 'white', 'blue'))(n_rdbl)
pal2 <- c('home' = 'red', 'away' = 'blue')
arw_v <- arrow(length = unit(3, 'pt'), type = 'closed')
# See https://stackoverflow.com/a/17313561/120898
pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

viz_pc_grid_epv_start <-
  pc_grid_epv_start %>% 
  ggplot() +
  aes(x = x, y = y) +
  .pitch_gg() +
  geom_contour_filled(
    aes(
      z = ppcf_att, 
      fill = ..level.., 
      color = ..level..
    ), 
    bins = n_rdbl,
    alpha = 0.5
  ) +
  scale_fill_manual(values = pal1) +
  scale_color_manual(values = pal1) +
  guides(fill = FALSE) +
  # ggnewscale::new_scale_fill() +
  ggnewscale::new_scale_color() +
  # scale_fill_manual(values = pal2) +
  scale_color_manual(values = pal2) +
  geom_segment(
    data = tracking_start %>% filter(!is.na(x)),
    # olor = 'black',
    size = 0.5,
    arrow = arw_v,
    aes(x = x, y = y, xend = x + x_v, yend = y + y_v, color = side)
  ) +
  ggrepel::geom_text_repel(
    data = tracking_start %>% filter(!is.na(x)),
    inherit.aes = FALSE,
    aes(x = x, y = y, color = side, label = player_id),
    force = 2,
    size = pts(8)
  ) +
  geom_point(
    data = tracking_start %>% filter(!is.na(x)),
    inherit.aes = FALSE,
    aes(x = x, y = y, color = side),
    size = 4
  ) +
  geom_point(
    data = events_filt,
    inherit.aes = FALSE,
    aes(x = start_x, y = start_y),
    size = 3,
    fill = 'yellow',
    color = 'black',
    shape = 21
  )
viz_pc_grid_epv


epv_grid_w_pc_start <- 
  inner_join(pc_grid_epv_start, epv_grid) %>% 
  mutate(epv = ppcf_att * value)
epv_grid_w_pc_start

n_blues <- 9L
# pal3 <- RColorBrewer::brewer.pal(n_blues, 'Blues')
pal3 <- colorRampPalette(c('white', 'blue'))(n_blues)



epv_grid_w_pc_start %>% 
  ggplot() +
  aes(x = x, y = y) %>% 
  .pitch_gg() +
  # geom_contour_filled(
  #   aes(z = epv, fill = ..level.., color = ..level..), 
  #   bins = n_blues, 
  #   alpha = 0.5
  # ) +
  geom_raster(
    aes(fill = epv)
  ) +
  # scale_fill_manual(values = pal3) +
  # scale_color_manual(values = pal3) +
  scale_fill_distiller(palette = 'Blues', direction = 1) +
  guides(fill = FALSE) +
  ggnewscale::new_scale_color() +
  # scale_fill_manual(values = pal2) +
  scale_color_manual(values = pal2) +
  geom_segment(
    data = tracking_start %>% filter(!is.na(x)),
    # olor = 'black',
    size = 0.5,
    arrow = arw_v,
    aes(x = x, y = y, xend = x + x_v, yend = y + y_v, color = side)
  ) +
  ggrepel::geom_text_repel(
    data = tracking_start %>% filter(!is.na(x)),
    inherit.aes = FALSE,
    aes(x = x, y = y, color = side, label = player_id),
    force = 2,
    size = pts(8)
  ) +
  geom_point(
    data = tracking_start %>% filter(!is.na(x)),
    inherit.aes = FALSE,
    aes(x = x, y = y, color = side),
    size = 4
  ) +
  geom_point(
    data = events_filt,
    inherit.aes = FALSE,
    aes(x = start_x, y = start_y),
    size = 3,
    fill = 'yellow',
    color = 'black',
    shape = 21
  )


.start_frame <- 53027L
do_compute_epv_added(events_filt, event_id = .event_id)
do_compute_xt_added(events_filt, event_id = .event_id)
do_compute_eepv_added(
  tracking = tracking_start,
  events = events_filt,
  event_id = .event_id
)


full_join(pc_grid, epv_grid)
pc_grid
epv_grid
inner_join(pc_grid, epv_grid)
