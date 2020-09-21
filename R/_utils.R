
.get_dims_opta <- function() {
  c(100, 100)
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
