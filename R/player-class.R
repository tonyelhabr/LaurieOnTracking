
library(vctrs)

# TODO: Make a validation function!
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
    
    vctrs::vec_assert(player_id, integer())
    vctrs::vec_assert(side, character())
    .validate_side(side)
    vctrs::vec_assert(start_frame, integer())
    # TODO: More assertion.
    
    # browser()
    res <-
      vctrs::new_rcrd(
        # structure(
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

player <- 
  function(player_id = 1L,
           # side = 'home',
           start_frame = 1L,
           events,
           tracking,
           params = get_default_model_params()) {
    
    player_id <- as.integer(player_id)
    start_frame <- as.integer(start_frame)

    vctrs::vec_assert(params, list())
    nms_req <- c('max_player_speed', 'reaction_time', 'tti_sigma', 'lambda_att', 'lambda_def')
    assertthat::assert_that(all(nms_req %in% names(params)))
    # TODO: Implement checks for the data types. (Even though stronger checking is done in `new_player()`, the user may pass in a "malformed" params that has the required names but whose key-value pairs don't have the correct types.
    
    assertthat::assert_that(is.data.frame(events))
    nms_req <- c('start_frame', 'team')
    assertthat::assert_that(all(nms_req %in% names(events)))
    
    event <- events %>% filter(start_frame == !!start_frame)
    assertthat::assert_that(nrow(event) == 1L)
    
    assertthat::assert_that(is.data.frame(tracking))
    
    nms_req <- c('frame', 'player', 'side', 'x', 'y', 'x_v', 'y_v')
    assertthat::assert_that(all(nms_req %in% names(tracking)))
    
    frame <- tracking %>% filter(frame == !!start_frame, player == player_id)
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

    res <-
      new_player(
        player_id = player_id,
        is_gk = is_gk,
        is_attack = side == tolower(event[['team']]),
        tti = -1,
        in_frame = TRUE,
        side = side,
        start_frame = start_frame,
        x = frame[['x']],
        y = frame[['y']],
        # pos = list('x' = frame[['x']], 'y' = frame[['y']]),
        x_v = frame[['x_v']],
        y_v = frame[['y_v']],
        # v = list('x' = frame[['x_v']], 'y' = frame[['y_v']]),
        vmax = params[['max_player_speed']],
        reaction_time = params[['reaction_time']],
        tti_sigma = params[['tti_sigma']],
        lambda_att = params[['lambda_att']],
        lambda_def = params[['lambda_def']]
      )
    res
}

format.player <- function(x, ...) {
  info <- sprintf('`player_id = %s`%s on %s team is located at `(%.2f, %.2f)` with velocity = `<%.1f, %.1f>` and has `tti = %.2f` and `ppcf = %.3f`', field(x, 'player_id'), ifelse(field(x, 'is_gk'), ' (goalkeeper)', ''), field(x, 'side'), field(x, 'x'), field(x, 'y'), field(x, 'x_v'), field(x, 'y_v'), field(x, 'tti'), field(x, 'ppcf'))
  # if(field(x, 'is_gk')) {
  #   info <- sprintf('%s. Player is the goalkeeper', info)
  # }
  # info <- sprintf('<player> %s located at (%s) with velocity <%s>', field(x, 'player_id'), field(x, 'pos'), field(x, 'v'))
  paste(info, sep = '\n')
  # print(format(info))
  # cat(info, sep = '\n')
  # invisible(x)
}

obj_print_data.player <- function(x) {
  cat(format(x), sep = '\n')
}

# norm(matrix(c(1, 2, 4, 2.5), nrow = 2, byrow = TRUE))
.norm <- function(x1, x2, y1, y2) {
  sqrt((x2 - x1)^2 + (y2 - y2)^2)
}

.tti.player <- function(x, rx2, ry2, ...) {
  ri <- field(x, 'reaction_time')
  rx1 <- field(x, 'x') + field(x, 'x_v') * ri
  ry1 <- field(x, 'y') + field(x, 'y_v') * ri
  # rxd <- rx2 - rx1
  # ryd <- ry2 - ry1
  res <- ri + .norm(rx1, rx2, ry1, ry2) / field(x, 'vmax')
  res
}

.p_intercept.player <- function(x, t, ...) {
  den <- 1 + (-pi / sqrt(3) / field(x, 'tti_sigma') * (t - field(x, 'tti')))
  res <- 1 / den
  # assertthat::assert_that(res > 0, msg = sprintf('Probability to intercept (`%.2f`) cannot be < 0.', res))
  res
}

`.update_tti<-.player` <- function(x, value) {
  field(x, 'tti') <- value
  x
}

# TODO!
# `.update_in_frame<-.player` <- function(x, value) {
#   field(x, 'in_frame') <- value
#   x
# }

`.update_ppcf<-.player` <- function(x, value) {
  field(x, 'ppcf') <- value
  x
}

.tti.default <- function(x, ...) {
  cls <- class(x)[1]
  err <- sprintf('`.tti()` doesn\'t know how to handle class `%s`!', cls) 
  stop(err, call. = FALSE)
}

.p_intercept.default <- function(x, ...) {
  cls <- class(x)[1]
  err <- sprintf('`.p_intercept()` doesn\'t know how to handle class `%s`!', cls) 
  stop(err, call. = FALSE)
}

`.update_tti<-default` <- function(x, ...) {
  cls <- class(x)[1]
  err <- sprintf('`.update_tti()` doesn\'t know how to handle class `%s`!', cls) 
  stop(err, call. = FALSE)
}

`.update_ppcf<-default` <- function(x, ...) {
  cls <- class(x)[1]
  err <- sprintf('`.update_ppcf()` doesn\'t know how to handle class `%s`!', cls) 
  stop(err, call. = FALSE)
}


.tti <- function(x, ...) {
  UseMethod('.tti')
}

.p_intercept <- function(x, ...) {
  UseMethod('.p_intercept')
}

`.update_tti<-` <- function(x, ...) {
  UseMethod('.update_tti<-')
}

`.update_ppcf<-` <- function(x, ...) {
  UseMethod('.update_ppcf<-')
}
