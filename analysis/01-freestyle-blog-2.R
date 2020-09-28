
# setup ----
extrafont::loadfonts(quiet = TRUE)
library(tidyverse)
library(patchwork)
library(ggtext)

.dir_proj <- here::here('data')
.dir_plot <- here::here('output', 'figs')
fs::dir_create(.dir_proj)
fs::dir_create(.dir_plot)
path_data <- fs::path(.dir_proj, 'pc.rds')
event_ids <- c(823, 1754, 1664)
if(!fs::file_exists(path_data)) {
  
  game_id <- 2
  events <- import_event_data(game_id = game_id, postprocess = TRUE)
  tracking_home <- import_tracking_data_timed(game_id = game_id, side = 'home', overwrite = F)
  tracking_away <- import_tracking_data_timed(game_id = game_id, side = 'away', overwrite = F)
  tracking <- bind_rows(tracking_home, tracking_away)
  tracking
  
  # TODO: Find more interesting frames.
  # frames <- tracking %>% distinct(frame)
  # set.seed(42L)
  # frames_sample <- frames %>% sample_frac(0.1)
  tracking_agg <-
    # frames_sample %>% 
    # inner_join(tracking) %>% 
    tracking %>% 
    # filter(frame >= 1000) %>% 
    mutate(keep = frame %% 1000 == 0) %>% 
    filter(keep) %>% 
    select(-keep) %>% 
    drop_na() %>% 
    group_by(period, time, frame) %>% 
    summarize(
      across(c(x, y), mean)
    ) %>% 
    ungroup() %>% 
    mutate(z = sqrt((x - 106 / 2)^2 + (y - 68 / 2)^2)) %>% 
    arrange(z)
  tracking_agg
  
  events
  events %>% filter(start_frame >= 6000)
  start_frame <- 6099L
  event_id <- 110L
  
  # more setup ----
  pitch_grid_fb <- .get_pitch_grid(n_cell_x = 100L, n_cell_y = 100L)
  pitch_grid_spearman <- import_epv_grid() %>% select(x, y)
  
  .add_lead_cols <- function(tracking) {
    res <-
      tracking %>% 
      group_by(player_id) %>% 
      mutate(
        across(c(x, y, time), ~dplyr::lead(.x, 1L), .names = 'next_{col}')
      ) %>% 
      ungroup()
    res
  }
  
  .do_knn <- function(trn, tst, fmla, ...) {
    col_y <- fmla[[2]] %>% as.character()
    col_y_sym <- col_y %>% sym()
    rec <- recipes::recipe(fmla, data = trn)
    spec <-
      parsnip::nearest_neighbor(...) %>% 
      parsnip::set_engine('kknn')
    wf <-
      workflows::workflow() %>%
      workflows::add_recipe(rec) %>%
      workflows::add_model(spec)
    fit <-
      parsnip::fit(wf, trn) %>% 
      workflows::pull_workflow_fit()
    pred <- 
      fit %>% 
      predict(new_data = tst) %>%
      rename(!!col_y_sym := .pred) %>% 
      bind_cols(tst)
    pred
  }
  
  .do_knn_fb <- partial(.do_knn, fmla = formula(value ~ x + y), tst = pitch_grid_fb, neighbors = 8L, ... = )
  
  .identify_closest_side <- function(x, y, tracking) {
    res <-
      tracking %>% 
      mutate(
        z = sqrt((x - !!x)^2 + (y - !!y)^2)
      ) %>% 
      filter(z == min(z)) %>% 
      head(1) %>% 
      pull(side)
    res
  }
  
  do_compare <- 
    function(event_id, 
             events = events,
             tracking = tracking,
             pitch_grid_fb = pitch_grid_fb,
             pitch_grid_spearman = pitch_grid_spearman,
             pitch_grid_vor = pitch_grid_spearman) {
      
      events_filt <-
        events %>%
        filter(event_id == !!event_id)
      
      tracking_filt <-
        tracking %>%
        .add_lead_cols() %>% 
        inner_join(events_filt %>% select(frame = start_frame)) %>% 
        drop_na()
      
      # TODO: Put this in a function.
      pc_fb <-
        tracking_filt %>% 
        # Drop the players who aren't on the field.
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
          I = pmap(list(x, y, mu_x, mu_y, Sigma), .calculate_I, pitch_grid = !!pitch_grid_fb)
        ) %>% 
        select(any_of(names(tracking_filt)), I)
      pc_fb
      
      pc_agg_fb <-
        pc_fb %>% 
        select(frame, time, player_id, side, player_x = x, player_y = y, I) %>% 
        mutate(pitch_grid = list(!!pitch_grid_fb)) %>% 
        unnest(cols = c(I, pitch_grid)) %>% 
        group_by(frame, time, side, x, y) %>%
        summarise(side_sum = sum(I, na.rm = TRUE)) %>%
        ungroup() %>%
        pivot_wider(names_from = side, values_from = side_sum) %>% 
        # No real reason. Just makes the numbers easer to look at.
        mutate(
          value = (away / (home + away)) %>% round(3)
        )
      pc_agg_fb
      
      side <- events_filt[['side']]
      pc_agg_spearman <-
        do_calculate_pc_for_event(
          tracking = tracking_filt,
          events = events_filt %>% mutate(frame = start_frame),
          event_id = event_id,
          epv_grid = pitch_grid_spearman
        ) %>% 
        # mutate(ppcf_away = ifelse(.side == 'away', ppcf_att, ppcf_def)) %>% 
        select(x, y, value = ppcf_att)
      pc_agg_spearman %>% count(value)
      
      if(side == 'home') {
        pc_agg_spearman$value <- 1 - pc_agg_spearman$value
      }
      
      pred_spearman <- .do_knn_fb(pc_agg_spearman)
      
      f <- partial(.identify_closest_side, tracking = tracking_filt, ... = )
      pc_vor <-
        pitch_grid_vor %>% 
        mutate(
          owned_by = map2_chr(x, y, f)
        ) %>% 
        mutate(value = if_else(owned_by == 'away', 1, 0)) %>% 
        select(x, y, value)
      pc_vor
      
      pred_vor <- .do_knn_fb(pc_vor)
      
      suppressMessages(
        res_wide <-
          list(
            pc_agg_fb %>% rename(value_fb = value), 
            pred_spearman %>% rename(value_spearman = value),
            pred_vor %>% rename(value_vor = value)
          ) %>% 
          reduce(full_join)
      )
      
      res_long <-
        res_wide %>% 
        select(x, y, matches('^value_')) %>% 
        pivot_longer(
          matches('^value_'),
          names_to = c('dummy', 'method'),
          names_sep = '_',
          values_to = 'value'
        ) %>% 
        select(x, y, method, value)
      
      res_d_long <-
        res_wide %>% 
        mutate(
          diff_fb_spearman = value_fb - value_spearman,
          diff_fb_vor = value_fb - value_vor,
          diff_spearman_vor = value_spearman - value_vor
        ) %>% 
        select(x, y, matches('^diff_')) %>% 
        pivot_longer(
          matches('^diff_'),
          names_to = c('dummy', 'method_1', 'method_2'),
          names_sep = '_',
          values_to = 'diff'
        ) %>% 
        select(x, y, method_1, method_2, diff)
      res_d_long
      
      list(
        vals = res_long, 
        diffs = res_d_long,
        tracking_filt = tracking_filt,
        events_filt = events_filt
      )
    }
  
  do_compare_timed <- .time_it(do_compare)
  pc <-
    event_ids %>% 
    tibble(event_id = .) %>% 
    mutate(
      pc = 
        map(event_id,
            ~do_compare_timed(
              event_id = .x,
              events = events,
              tracking = tracking,
              pitch_grid_fb = pitch_grid_fb,
              pitch_grid_spearman = pitch_grid_spearman,
              pitch_grid_vor = pitch_grid_spearman
            )
        )
    )
  pc
  write_rds(pc, path_data)
} else {
  pc <- read_rds(path_data)
}

pal2 <- c('home' = 'red', 'away' = 'blue')
pal2_diff <- c('method1' = 'magenta', 'method2' = 'limegreen')
arw <- arrow(length = unit(3, 'pt'), type = 'closed')
# See https://stackoverflow.com/a/17313561/120898
pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

.gg_constants <- function(..., tracking, events) {
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
    # geom_segment(
    #   data = events,
    #   aes(x = start_x, y = start_y, xend = end_x, yend = end_y),
    #   # curvature = -0.2
    #   size = 1,
    #   arrow = arw,
    #   color = 'black'
    # ),
    # labs(
    #   caption = '**Viz:** @TonyElHabr | **Data:** Metrica Sports'
    # ),
    theme(
      plot.title = ggtext::element_markdown('Karla', face = 'bold', size = 18, color = 'gray20', hjust = 0.5),
      # plot.title.position = 'plot',
      plot.subtitle = ggtext::element_markdown('Karla', face = 'bold', size = 14, color = 'gray20', hjust = 0.5),
      # plot.subtitle = element_text(size = 16, hjust = 0.5),
      plot.margin = margin(10, 10, 10, 10),
      plot.caption = ggtext::element_markdown('Karla', size = 14, color = 'gray20', hjust = 0),
      plot.caption.position = 'plot'
    )
  )
}

.plot1 <-
  function(data,
           gg_constants,
           ...,
           color_low = pal2[['home']],
           color_high = pal2[['away']],
           color_midpoint = 0.5) {
    
  res <-
    data %>% 
    ggplot() +
    aes(x = x, y = y) +
    theme_void() +
    .gg_pitch(aspect_ratio = 105/68) +
    geom_raster(
      aes(fill = value),
      interpolate = TRUE,
      hjust = 0.5,
      vjust = 0.5,
      alpha = 0.4
    ) +
    scale_fill_gradient2(low = color_low, high = color_high, midpoint = color_midpoint) +
    scale_color_gradient2(low = color_low, high = color_high, midpoint = color_midpoint) +
    guides(fill = FALSE) +
    coord_flip() +
    # scale_x_continuous(trans = 'reverse') +
    scale_y_continuous(trans = 'reverse') +
    ggnewscale::new_scale_color() +
    gg_constants
  res
}

.plot1_diff <-
  partial(
    .plot1,
    color_low = pal2_diff[['method1']],
    color_high = pal2_diff[['method2']],
    color_midpoint = 0,
    ... =
  )

.select_pull_pluck <- function(data, ..., pos = 1) {
  data %>% 
    select(...) %>% 
    pull(...) %>% 
    pluck(pos)
}

.methods_valid <- c('fb', 'spearman', 'vor')
.prettify_method <- function(method) {
  case_when(
    method == 'fb' ~ 'Fernandez & Bornn (2019)', 
    method == 'spearman' ~ 'Spearman (2018)', 
    method == 'vor' ~ 'Nearest'
  )
}

pc_wide <- pc %>% unnest_wider(pc)
plot_h2h <-
  function(event_id,
           method_1 = 'fb',
           method_2 = 'spearman',
           path_export = fs::path(
             .dir_plot,
             sprintf('pc_compare_%s_%s_v_%s.png', event_id, method_1, method_2)
           ),
           width = 12L,
           height = 10L) {
             
  # browser()
  # event_id <- event_ids[2]

  f <- function(data, ...) {
    data %>% 
      filter(event_id == !!event_id) %>% 
      .select_pull_pluck(...)
  }
  
  vals <- pc_wide %>% f(vals)
  diffs <- pc_wide %>% f(diffs)
  tracking_filt <- pc_wide %>% f(tracking_filt)
  events_filt <- pc_wide %>% f(events_filt)
  
  gg_constants <- .gg_constants(tracking = tracking_filt, events = events_filt)
  
  method_1_pretty <- method_1 %>% .prettify_method()
  method_2_pretty <- method_2 %>% .prettify_method()
  
  color_lhs <- pal2_diff[['method1']]
  color_rhs <- pal2_diff[['method2']]
  
  viz_lhs <- 
    vals %>% 
    filter(method == method_1) %>% 
    .plot1(gg_constants) +
    labs(
      caption = 'Viz: @TonyElHabr',
      title = glue::glue('<span style="color:{color_lhs}">{method_1_pretty}</span>')
    )
  viz_lhs
  
  viz_rhs <- 
    vals %>% 
    filter(method == method_2) %>% 
    .plot1(gg_constants) +
    labs(
      caption = '',
      title = glue::glue('<span style="color:{color_rhs}">{method_2_pretty}</span>')
    )

  viz_mid <- 
    diffs %>% 
    filter(method_1 == !!method_1 & method_2 == !!method_2) %>% 
    rename(value = diff) %>% 
    .plot1_diff(gg_constants) +
    labs(
      caption = '',
      title = 'Pitch Control Comparison'
    )
  
  viz_mid
  res <- viz_lhs + viz_mid + viz_rhs
  ggsave(plot = res, filename = path_export, width = width, height = height, type = 'cairo')
  res
}

# main ----
# pc_wide <- pc %>% unnest_wider(pc)
# cors <-
#   pc_wide %>% 
#   select(event_id, vals) %>% 
#   mutate(
#     cors = 
#       map(
#         vals, 
#         ~suppressMessages(
#           pivot_wider(.x, names_from = method, values_from = value) %>%
#             select(-x, -y) %>% 
#             corrr::correlate()
#         )
#       )
#   ) %>% 
#   select(-vals) %>% 
#   unnest(cors) %>% 
#   rename(method_1 = rowname) %>% 
#   pivot_longer(-c(event_id, method_1), names_to = 'method_2', values_to = 'cor') %>% 
#   filter(!is.na(cor)) %>% 
#   filter(method_1 < method_2)
# cors

res <-
  crossing(
    event_id = event_ids,
    method_1 = .methods_valid,
    method_2 = .methods_valid
  ) %>% 
  filter(method_1 < method_2) %>% 
  # head(1) %>% 
  mutate(res = pmap(list(event_id = event_id, method_1 = method_1, method_2 = method_2), plot_h2h))
res
