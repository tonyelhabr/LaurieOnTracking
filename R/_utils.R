
.get_dims_opta <- function() {
  c(100, 100)
}

.get_dims_actual <- function() {
  c(105, 68)
}

# TODO: Change `dims` to `rng`?
.get_valid_coords <- function() {
  c('x', 'y')
}

.validate_coord <- function(coord) {
  match.arg(coord)
}

.get_rng_actual <- function(coord = .get_valid_coords()) {
  .validate_coord(coord)
  switch(coord, x = c(0, 105), y = c(0, 68))
}

.rescale <- function(x, rng1, rng2) {
  rng2[1] + ((x - rng1[1]) * (rng2[2] - rng2[1])) / (rng1[2] - rng1[1])
}

.get_valid_sides <- function() {
  c('away', 'home')
}

.validate_side <- function(x = .get_valid_sides()) {
  match.arg(x)
}

.get_default_pc_params <- function(time_to_control_veto = 3) {
  params <-
    list(
      max_player_accel = 7,
      max_player_speed = 5,
      reaction_time = 0.7,
      tti_sigma = 0.45,
      kappa_def = 1,
      lambda_att = 4.3,
      average_ball_speed = 15,
      int_dt = 0.04,
      max_int_time = 10,
      iter_min = 5,
      model_converge_tol = 0.01
    )
  params$lambda_def = 4.3 * params[['kappa_def']]
  params$lambda_gk = 3.0 * params[['lambda_def']]
  params$time_to_control_att = time_to_control_veto * log(10) * sqrt(3) * params[['tti_sigma']] * pi * (1 / params[['lambda_att']])
  params$time_to_control_def = time_to_control_veto * log(10) * sqrt(3) * params[['tti_sigma']] * pi * (1 / params[['lambda_def']])
  params
}

save_plot <-
  function(viz,
           file = deparse(substitute(viz)),
           ext = 'png',
           # dir = here::here('output'),
           dir = here::here('output', 'figs'),
           path = fs::path(dir, sprintf('%s.%s', file, ext)),
           height = 8,
           scaler = 105 / 68,
           multiplier = 1,
           adder = -2,
           width = height * scaler * multiplier + adder,
           ...) {
    ggsave(plot = viz, filename = path, width = width, height = height, type = 'cairo', ...)
  }
