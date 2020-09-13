
.to_metrica_coordinates <- function(data, dims = .get_dims_metrica()) {
  res <-
    data %>%
    mutate(
      across(matches('_x$'), ~{(.x - 0.5) * dims[1]}),
      across(matches('_y$'), ~{-1 * ((.x - 0.5) * dims[2])})
    )
  res
}

.to_single_player_direction <- function(data) {
  res <- data %>% mutate(across(matches('_[x|y]$'), ~if_else(period == 2, -1 * .x, .x)))
  res
}

read_event_data <- function(dir, game_id, postprocess = FALSE) {
  path <- fs::path(dir, glue::glue('Sample_Game_{game_id}'), glue::glue('Sample_Game_{game_id}_RawEventsData.csv'))
  res <- path %>% read_csv() %>% janitor::clean_names()
  if(!postprocess) {
    return(res)
  }
  res <- 
    res %>% 
    .to_metrica_coordinates() %>% 
    .to_single_player_direction()
  res
}

.toupper1 <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

# Reference: https://www.robert-hickman.eu/post/fall_back_in_to_space/
.fix_tracking_names <- function(data, side) {
  rgx <- '^X[0-9]*$'
  nms <- names(data)
  names(data)[grep(rgx, nms)-1] <- paste0(nms[grep(rgx, nms)-1], '_x')
  nms <- names(data)
  names(data)[grep(rgx, nms)] <- gsub('_x$', '_y', nms[grep(rgx, nms)-1])
  res <-
    data %>% 
    rename_with(tolower) %>% 
    rename_with(~str_replace(.x, 'player', sprintf('%s_', side)), matches('^player')) %>% 
    rename(time = 3)
  res
}


.fix_tracking_names_alternative <- function(data, side) {
  player_nums <- 
    data %>% 
    names() %>% 
    str_subset('^Player.*[0-9]$') %>% 
    str_remove_all('[A-Za-z\\s]')
  nms <- data %>% names()
  n_player <- player_nums %>% length()
  nms_out <- nms
  walk2(
    seq(1, n_player),
    player_nums,
    ~{
      idx1 <- ..1 * 2 + 2
      idx2 <- ..1 * 2 + 3
      nms_out[idx1] <<- glue::glue('{side}_{..2}_x')
      nms_out[idx2] <<- glue::glue('{side}_{..2}_y')
      invisible()
    }
  )
  nms_out
  nms_out[-2] <- 'ball_x'
  nms_out[-1] <- 'ball_y'
  res
}

read_tracking_data <- function(dir, game_id, side = .get_valid_sides(), postprocess = FALSE) {
  side <- .validate_side(side)
  path <- fs::path(dir, glue::glue('Sample_Game_{game_id}'), glue::glue('Sample_Game_{game_id}_RawTrackingData_{.toupper1(side)}_Team.csv'))
  suppressWarnings(data <- read_csv(path, skip = 2))
  res <- data %>% .fix_tracking_names(side = side)
  if(!postprocess) {
    return(res)
  }
  res <- 
    res %>% 
    .to_metrica_coordinates() %>% 
    .to_single_player_direction()
  res
}

find_goalkeeper <- function(data) {
  # side <- .validate_side(side)
  rgx <- .get_metrica_col_rgx()
  res <-
    data %>% 
    select(matches('_x$')) %>% 
    slice(1) %>% 
    pivot_longer(matches('.*')) %>% 
    filter(!is.na(value)) %>% 
    filter(abs(value) == max(abs(value))) %>% 
    # slice(1) %>% # In case there are somehow more than one row
    pull(name) %>% 
    str_replace_all(rgx, '\\1_\\2')
  res
}

find_playing_direction <- function(data, gk_number) {
  # side <- .validate_side(side)
  col <- glue::glue('{gk_number}_x')
  col_sym <- sym(col)
  val <-
    data %>% 
    select(!!col_sym) %>% 
    slice(1) %>% 
    pull(!!col_sym) %>% 
    sign()
  res <- val * -1
}
