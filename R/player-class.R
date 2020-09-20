
new_player <-
  function(player_id = integer(),
           is_gk = logical(),
           side = character(),
           is_attack = logical(),
           tti = double(),
           in_frame = logical(),
           start_frame = integer(),
           x = double(),
           y = double(),
           x_v = double(),
           y_v = double(),
           vmax = double(),
           reaction_time = double(),
           tti_sigma = double(),
           lambda_att = double(),
           lambda_def = double()) {

    # I like using quotes for the names of list elements because they technically don't exist in the list before hand. Quotes implicitly imply that we are creating something new.
    res <-
      vctrs::new_rcrd(
        list(
          'player_id' = player_id,
          'is_gk' = is_gk,
          'side' = side,
          'is_attack' = is_attack,
          'tti' = tti,
          'in_frame' = in_frame,
          'name' = sprintf('%s_%s', side, player_id),
          'start_frame' = start_frame,
          'x' = x,
          'y' = y,
          'x_v' = x_v,
          'y_v' = y_v,
          'vmax' = vmax,
          'reaction_time' = reaction_time,
          'tti_sigma' = tti_sigma,
          'lambda_att' = lambda_att,
          'lambda_def' = lambda_def,
          'ppcf' = 0
        ),
        class = 'player'
      )
    res
  }

validate_player <- function(player) {
  
  vctrs::vec_assert(vctrs::field(player, 'player_id'), integer())
  side <- vctrs::field(player, 'side')
  vctrs::vec_assert(side, character())
  .validate_side(side)
  vctrs::vec_assert(vctrs::field(player, 'is_attack'), logical())
  vctrs::vec_assert(vctrs::field(player, 'tti'), double())
  vctrs::vec_assert(vctrs::field(player, 'in_frame'), logical())
  vctrs::vec_assert(vctrs::field(player, 'start_frame'), integer())
  vctrs::vec_assert(vctrs::field(player, 'x'), double())
  vctrs::vec_assert(vctrs::field(player, 'y'), double())
  vctrs::vec_assert(vctrs::field(player, 'x_v'), double())
  vctrs::vec_assert(vctrs::field(player, 'y_v'), double())
  vctrs::vec_assert(vctrs::field(player, 'vmax'), double())
  vctrs::vec_assert(vctrs::field(player, 'reaction_time'), double())
  vctrs::vec_assert(vctrs::field(player, 'tti_sigma'), double())
  vctrs::vec_assert(vctrs::field(player, 'lambda_att'), double())
  vctrs::vec_assert(vctrs::field(player, 'lambda_def'), double())
  player
}

player <- 
  function(player_id = 1L,
           start_frame = 1L,
           events,
           tracking,
           params = get_default_pc_params()) {
    
    player_id <- as.integer(player_id)
    start_frame <- as.integer(start_frame)

    vctrs::vec_assert(params, list())
    nms_req <- c('max_player_speed', 'reaction_time', 'tti_sigma', 'lambda_att', 'lambda_def')
    assertthat::assert_that(all(nms_req %in% names(params)))
    # TODO: Implement checks for the data types of the columns. (Even though stronger checking is done in `new_player()`, the user may pass in a "malformed" params that has the required names but whose key-value pairs don't have the correct types.
    
    assertthat::assert_that(is.data.frame(events))
    nms_req <- c('start_frame', 'team')
    assertthat::assert_that(all(nms_req %in% names(events)))
    
    event <- events %>% filter(start_frame == !!start_frame)
    assertthat::assert_that(nrow(event) == 1L)
    
    assertthat::assert_that(is.data.frame(tracking))
    nms_req <- c('frame', 'player', 'side', 'x', 'y', 'x_v', 'y_v')
    assertthat::assert_that(all(nms_req %in% names(tracking)))
    
    frame <- tracking %>% filter(frame == !!start_frame, player == player_id)
    # browser()
    assertthat::assert_that(nrow(frame) == 1L)

    side <- frame[['side']]
    # .validate_side(side)
    
    gk_numbers <- tracking %>% pull_gk_numbers()
    # vctrs::vec_assert(gk_numbers, character()) # Probably overkill
    assertthat::assert_that(
      length(gk_numbers) == 2, 
      identical(sort(names(gk_numbers)), c('away', 'home'))
    )
    is_gk <- any(player_id %in% gk_numbers)
    
    x <- frame[['x']]
    y <- frame[['y']]
    player <-
      new_player(
        player_id = player_id,
        is_gk = is_gk,
        is_attack = side == tolower(event[['team']]),
        tti = -1,
        in_frame = !is.na(x) & !is.na(y),
        side = side,
        start_frame = start_frame,
        x = x,
        y = y,
        x_v = frame[['x_v']],
        y_v = frame[['y_v']],
        vmax = params[['max_player_speed']],
        reaction_time = params[['reaction_time']],
        tti_sigma = params[['tti_sigma']],
        lambda_att = params[['lambda_att']],
        lambda_def = params[['lambda_def']]
      )
    player <- validate_player(player)
    player
}

format.player <- function(x, ...) {
  suffix <- if(vctrs::field(x, 'in_frame')) {
    sprintf('located at `(%.2f, %.2f)` with velocity = `<%.1f, %.1f>`', vctrs::field(x, 'x'), vctrs::field(x, 'y'), vctrs::field(x, 'x_v'), vctrs::field(x, 'y_v'))
  } else {
    'not on the pitch'
  }
  prefix <- sprintf('`player_id = %s`%s on %s team (%s) is ', vctrs::field(x, 'player_id'), ifelse(vctrs::field(x, 'is_gk'), ' (goalkeeper)', ''), vctrs::field(x, 'side'), ifelse(vctrs::field(x, 'is_attack'), 'attacking', 'defending'))
  msg <- paste0(prefix, suffix)
  cat(msg)
}

.norm <- function(x1, x2, y1, y2) {
  # res <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  # res <- matrix(c(x1, y1, x2, y2), nrow = 2, byrow = TRUE)
  m <- matrix(c(x1, y1)) - matrix(c(x2, y2))
  res <- sqrt(sum(m^2))
  res
}

.get_tti.player <- function(x, rx2, ry2, ...) {
  ri <- vctrs::field(x, 'reaction_time')
  rx1 <- vctrs::field(x, 'x') + vctrs::field(x, 'x_v') * ri
  ry1 <- vctrs::field(x, 'y') + vctrs::field(x, 'y_v') * ri
  res <- ri + .norm(rx1, rx2, ry1, ry2) / vctrs::field(x, 'vmax')
  res
}

.get_p_intercept.player <- function(x, t, ...) {
  den_term <- (-pi / sqrt(3) / vctrs::field(x, 'tti_sigma')) * (t - vctrs::field(x, 'tti'))
  den <- 1 + exp(den_term)
  res <- 1 / den
  # assertthat::assert_that(res > 0, msg = sprintf('Probability to intercept (`%.2f`) cannot be < 0.', res))
  res
}

.msg_cls_err <- function(x, f) {
  cls <- class(x)[1]
  sprintf('`%s()` doesn\'t know how to handle class `%s`!', f, cls) 
}

`.set_tti<-.player` <- function(x, value) {
  vctrs::field(x, 'tti') <- value
  x
}

# TODO!
# `.set_in_frame<-.player` <- function(x, value) {
#   vctrs::field(x, 'in_frame') <- value
#   x
# }

`.set_ppcf<-.player` <- function(x, value) {
  vctrs::field(x, 'ppcf') <- value
  x
}

.get_tti.default <- function(x, ...) {
  stop(.msg_cls_err(x, '.get_tti'), call. = FALSE)
}

.get_p_intercept.default <- function(x, ...) {
  stop(.msg_cls_err(x, '.get_p_intercept'), call. = FALSE)
}

`.set_tti<-.default` <- function(x, ...) {
  stop(.msg_cls_err(x, '.set_tti'), call. = FALSE)
}

`.set_ppcf<-.default` <- function(x, ...) {
  stop(.msg_cls_err(x, '.set_ppcf'), call. = FALSE)
}

.get_tti <- function(x, ...) {
  UseMethod('.get_tti')
}

.get_p_intercept <- function(x, ...) {
  UseMethod('.get_p_intercept')
}

`.set_tti<-` <- function(x, ...) {
  UseMethod('.set_tti<-')
}

`.set_ppcf<-` <- function(x, ...) {
  UseMethod('.set_ppcf<-')
}
