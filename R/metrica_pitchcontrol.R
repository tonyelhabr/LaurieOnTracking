
get_default_model_params <- function(time_to_control_veto = 3) {
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
      model_converge_tol = 0.01
    )
  
  params <-
    append(
      params,
      c(
        lambda_def = 4.3 * params[['kappa_def']],
        lambda_gk = 3.0 * params[['lambda_def']],
        time_to_control_att = time_to_control_veto * log(10) * sqrt(3) * params[['tti_sigma']] * pi * (1 / params[['lambda_att']]),
        time_to_control_def = time_to_control_veto * log(10) * sqrt(3) * params[['tti_sigma']] * pi * (1 / params[['lambda_def']])
      )
    )
  params
}
