Lab 05 - La Quinta is Spanish for next to Denny’s, Pt. 2
================
Shatavia Bellmon
2/23/26

### Load packages and data

``` r
library(tidyverse) 
library(readr) 
```

``` r
states <- read_csv("data/states.csv")
dennys <- read_csv("data/dennys.csv")
laquinta <- read_csv("data/laquinta.csv")
```

### Exercise 1

``` r
dn <- dennys
```

``` r
dn_ak <- dn %>%
  filter(state == "AK")
nrow(dn_ak)
```

    ## [1] 3

There are three Denny’s in Alaska.

``` r
lq <- laquinta
```

``` r
lq_ak <- lq %>%
  filter(state == "AK")
nrow(lq_ak)
```

    ## [1] 2

There are two Laquintas in Alaska.

### Exercise 2

``` r
num_parings <- nrow(dn_ak) * nrow((lq_ak))
```

There needs to be 6 parings.

### Exercise 3

``` r
dn_lq_ak <- full_join(dn_ak, lq_ak,
  by = "state"
)
```

    ## Warning in full_join(dn_ak, lq_ak, by = "state"): Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 1 of `x` matches multiple rows in `y`.
    ## ℹ Row 1 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

``` r
dn_lq_ak
```

    ## # A tibble: 6 × 11
    ##   address.x     city.x state zip.x longitude.x latitude.x address.y city.y zip.y
    ##   <chr>         <chr>  <chr> <chr>       <dbl>      <dbl> <chr>     <chr>  <chr>
    ## 1 2900 Denali   Ancho… AK    99503       -150.       61.2 3501 Min… Ancho… 99503
    ## 2 2900 Denali   Ancho… AK    99503       -150.       61.2 4920 Dal… Fairb… 99709
    ## 3 3850 Debarr … Ancho… AK    99508       -150.       61.2 3501 Min… Ancho… 99503
    ## 4 3850 Debarr … Ancho… AK    99508       -150.       61.2 4920 Dal… Fairb… 99709
    ## 5 1929 Airport… Fairb… AK    99701       -148.       64.8 3501 Min… Ancho… 99503
    ## 6 1929 Airport… Fairb… AK    99701       -148.       64.8 4920 Dal… Fairb… 99709
    ## # ℹ 2 more variables: longitude.y <dbl>, latitude.y <dbl>

### Exercise 4

View(dn_lq_ak)

There are six observations in the dn_lq_ak data frame. The variables in
this data frame are address.x, city.x, state, zip.x, longitude.x,
latitude.x, address.y, city.y, zip.y, longitude.y, and latitude.y.

### Exercise 5

``` r
haversine <- function(long1, lat1, long2, lat2, round = 3) {
  # convert to radians
  long1 <- long1 * pi / 180
  lat1 <- lat1 * pi / 180
  long2 <- long2 * pi / 180
  lat2 <- lat2 * pi / 180

  R <- 6371 # Earth mean radius in km

  a <- sin((lat2 - lat1) / 2)^2 + cos(lat1) * cos(lat2) * sin((long2 - long1) / 2)^2
  d <- R * 2 * asin(sqrt(a))

  return(round(d, round)) # distance in km
}
```

### Exercise 6

``` r
dn_lq_ak <- dn_lq_ak %>%
  mutate(distance = haversine(longitude.x, latitude.x, longitude.y, longitude.y))
```

### Exercise 7

``` r
dn_lq_ak_min <- dn_lq_ak %>%
  group_by(address.x) %>%
  summarize(closest_distance = min(distance))
```

### Exercise 8

``` r
summary(dn_lq_ak_min$closest_distance)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   16148   16351   16555   16420   16555   16556

``` r
ggplot(dn_lq_ak_min, aes(x = closest_distance)) +
  geom_histogram(binwidth = 5, fill = "orange", color = "red") +
  labs(title = "Distance to Nearest La Quinta from Denny's - Alaska)", 
       x = "Distance (km)", y = "Count")
```

![](lab-05_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

The distribution of distances between Denny’s and the nearest La Quinta
in Alaska is left skewed with a greater distance being more prevalent
amoung the different locations,

View(dn_lq_ak)

### Exercise 9

### Exercise 10

### Exercise 11
