
.get_verbose <- function() {
  # getOption('ercot.verbose')
  TRUE
}

# Reference: https://glue.tidyverse.org/articles/transformers.html
.vv_transformer <- function(text, envir) {
  regex <- '=$'
  if (!grepl(regex, text)) {
    return(glue::identity_transformer(text, envir))
  }
  
  text <- sub(regex, '', text)
  res <- glue::identity_transformer(text, envir)
  n <- length(res)
  res <- glue::glue_collapse(res, sep = ', ')
  if (n > 1) {
    res <- c('[', res, ']')
  }
  glue::glue_collapse(cli::bg_cyan(glue::glue('`{text}` = "{res}"')))
}

.display_info <- function(x, ..., .envir = parent.frame(), .verbose = .get_verbose(), .f_glue = glue::glue_collapse) {
  if (!.verbose) {
    return(invisible(x))
  }
  x <- .f_glue(x, '\n')
  x <- glue::glue(x, .envir = .envir)
  cli::cat_line(x)
}

.display_info_var <- partial(.display_info, .f_glue = .vv_transformer, ... = )

# NOTE: Ignore `verbose` for these.
.display_warning <- function(x, ..., .envir = parent.frame(), .verbose = .get_verbose()) {
  usethis::ui_warn(x, .envir = .envir)
}

.display_error <- function(x, ..., .envir = parent.frame(), verbose = .get_verbose()) {
  usethis::ui_stop(x, .envir = .envir)
}


.time_it <- function(f, ..., .name = NULL, .verbose = .get_verbose()) {
  
  if(is.null(.name)) {
    .name <- as.character(sys.call())
  }
  
  function(...) {
    time_1 <- Sys.time()
    .display_info('Starting {cli::bg_black(.name)} at {cli::bg_black(time_1)}.', .verbose = .verbose)
    res <- f(...)
    time_2 <- Sys.time()
    dur <- (time_2 - time_1) %>% lubridate::as.duration()
    dur_s <- dur %>% as.numeric('seconds') %>% scales::comma(accuracy = 0.1)
    dur_m <- dur %>% as.numeric('minutes') %>% scales::comma(accuracy = 0.1)
    parenth <- 
      ifelse(
        as.numeric(dur, 'seconds') >= 31L, 
        glue::glue(' (~{cli::bg_black(dur_m)} minutes)') %>% as.character(), 
        ''
      )
    .display_info('Finished {cli::bg_black(.name)} at {cli::bg_black(time_2)}. It took {cli::bg_black(dur_s)} seconds{parenth} to complete.', .verbose = .verbose)
    invisible(res)
  }
}
