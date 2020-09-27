
# Reference: https://github.com/Torvaney/ggsoccer/blob/master/R/dimensions.R
.pitch_international <- list(
  length = 105,
  width = 68,
  penalty_box_length = 16.5,
  penalty_box_width = 40.32,
  six_yard_box_length = 5.5,
  six_yard_box_width = 18.32,
  penalty_spot_distance = 11,
  goal_width = 7.32,
  origin_x = 0,
  origin_y = 0
)

..get_pitch <- function(pitch_fill = 'white', pitch_color = 'black', limits = FALSE, dimension = .pitch_international) {
  ggsoccer::annotate_pitch(
    dimension = dimension,
    fill = pitch_fill, 
    colour = pitch_color,
    limits = limits
  )
}

.gg_pitch <- function(pitch = ..get_pitch(), ...) {
  res <-
    list(
      ...,
      pitch,
      # coord_flip(
      #   xlim = xlim,
      #   ylim = ylim
      # ),
      # coord_flip(),
      ggsoccer::theme_pitch(), # changing the default aspect ratio cuz it looks weird
      theme(legend.position = 'none')
    )
  res
}
