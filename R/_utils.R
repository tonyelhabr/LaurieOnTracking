
.get_dims_opta <- function() {
  # c(106, 68)
  c(100, 100)
}

.get_valid_sides <- function() {
  c('away', 'home')
}
.validate_side <- function(x = .get_valid_sides()) {
  match.arg(x)
}

.get_col_rgx_metrica <- function() {
  '(home|away)_([0-9]+)_(x|y)$'
}