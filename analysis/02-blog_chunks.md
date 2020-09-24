    tracking_start

    ## # A tibble: 26 x 9
    ##    frame ball_x ball_y side  player_id     x     y   x_v    y_v
    ##    <int>  <dbl>  <dbl> <chr>     <int> <dbl> <dbl> <dbl>  <dbl>
    ##  1 53027  93.71  24.56 home          1 90.72 39.37 5.906 -3.985
    ##  2 53027  93.71  24.56 home          2 95.10 27.14 1.5   -2.023
    ##  3 53027  93.71  24.56 home          3 96.01 23.32 1.418  2.395
    ##  4 53027  93.71  24.56 home          4 92.39 15.64 1.005  3.473
    ##  5 53027  93.71  24.56 home          5 83.96 24.69 4.238  1.2  
    ##  6 53027  93.71  24.56 home          6 82.19 35.63 3.893 -0.619
    ##  7 53027  93.71  24.56 home          7 85.79 17.34 1.703  1.523
    ##  8 53027  93.71  24.56 home          8 76.06 50.16 2.018 -0.493
    ##  9 53027  93.71  24.56 home          9 61.22 25.35 0.863 -0.77 
    ## 10 53027  93.71  24.56 home         10 59.69 35.10 0.9   -0.573
    ## # ... with 16 more rows

    10L:12L %>% map(~player(player_id = .x, frame = 53027L, tracking = tracking_start, events = events_filt))

    ## [[1]]
    ## <player[1]>
    ## `player_id = 10` with `position = (10.00, 35.09)` and `velocity = <0.9, -0.6>`
    ## 
    ## [[2]]
    ## <player[1]>
    ## `player_id = 11` with `position = (11.00, 32.28)` and `velocity = <-0.3, 0.6>`
    ## 
    ## [[3]]
    ## <player[1]>
    ## `player_id = 12` is not on the pitch

    players <- 8L:10L %>% map(~player(player_id = .x, frame = 53027L, tracking = tracking_start, events = events_filt))
    target_x <- 94
    target_y <- 63
    for(i in seq_along(players)) {
      value <- .get_tti(players[[i]], x2 = target_x, y2 = target_y)
      .set_tti(players[[i]]) <- value
    }
    map(players, ~vctrs::field(.x, 'tti'))

    ## [[1]]
    ## [1] 4.92839
    ## 
    ## [[2]]
    ## [1] 10.6878
    ## 
    ## [[3]]
    ## [1] 9.49904
