
.get_dims_metrica <- function() {
  c(106, 68)
}


.get_valid_sides <- function() {
  c('away', 'home')
}
.validate_side <- function(x = .get_valid_sides()) {
  match.arg(x)
}

.get_metrica_col_rgx <- function() {
  '(home|away)_([0-9]+)_(x|y)$'
}