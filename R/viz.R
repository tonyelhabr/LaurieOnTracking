
.get_pitch <- function(pitch_fill = 'white', pitch_color = 'black', limits = FALSE) {
  ggsoccer::annotate_pitch(
    fill = pitch_fill, 
    colour = pitch_color,
    limits = limits,
    dimension = ggsoccer::pitch_opta
  )
}

.pitch_gg <- function(pitch = .get_pitch(), ...) {
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
