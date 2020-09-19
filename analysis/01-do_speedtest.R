
source(file.path('analysis', '01-speedtest_source.R'))
profvis::profvis(f())

i <- 1
while(TRUE || i < 5) {
  cat(i, sep = '\n')
  i <- i + 1
}
i

i <- 1
while(FALSE || i < 5) {
  cat(i, sep = '\n')
  i <- i + 1
}
i

pc <- f()
pc %>% 
  mutate(to_drop = map_lgl(res, ~pluck(.x, 'i_last') %>% is.null())) %>% 
  filter(!to_drop) %>% 
  mutate(i_last = map_dbl(res, ~pluck(.x, 'p_tot'))) %>% 
  arrange(desc(i_last)) %>% 
  unnest(res) %>% 
  unnest(res)
