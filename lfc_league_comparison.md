Liverpool League Comparison
================
Ryan Estrellado
12/26/2018

``` r
library(tidyverse) 
```

    ## ── Attaching packages ───────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.0.0     ✔ purrr   0.2.5
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.6
    ## ✔ tidyr   0.8.1     ✔ stringr 1.3.1
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## Warning: package 'ggplot2' was built under R version 3.4.4

    ## Warning: package 'tidyr' was built under R version 3.4.4

    ## Warning: package 'purrr' was built under R version 3.4.4

    ## Warning: package 'dplyr' was built under R version 3.4.4

    ## Warning: package 'stringr' was built under R version 3.4.4

    ## ── Conflicts ──────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(googlesheets)
```

    ## Warning: package 'googlesheets' was built under R version 3.4.4

``` r
library(devtools)
```

    ## Warning: package 'devtools' was built under R version 3.4.4

``` r
install_github('jalapic/engsoccerdata', username = "jalapic")
```

    ## Skipping install of 'engsoccerdata' from a github remote, the SHA1 (ddf4dc82) has not changed since last install.
    ##   Use `force = TRUE` to force installation

``` r
library(engsoccerdata)
```

``` r
lfc <- england %>% 
  filter(Season %in% c(2008, 2013, 2018), 
         home == "Liverpool" | visitor == "Liverpool") %>% 
  select(-c(FT, division, tier, totgoal, goaldif)) %>% 
  mutate(points = 0, 
         # Home wins
         points = ifelse(home == "Liverpool" & result == "H", 3, points), 
         # Away wins
         points = ifelse(visitor == "Liverpool" & result == "A", 3, points), 
         # Draws
         points = ifelse(result == "D", 1, points)) 
```

    ## Warning: package 'bindrcpp' was built under R version 3.4.4

2018-2019 data is updated and stored in a [Google Sheet](%22https://docs.google.com/spreadsheets/d/1xLXf6uISIuYE2SOAA267-PBpTlO-11AZrmDJsjq4M_c/edit?usp=sharing%22)

``` r
# Read in 2018-2019 data 
key <- extract_key_from_url("https://docs.google.com/spreadsheets/d/1xLXf6uISIuYE2SOAA267-PBpTlO-11AZrmDJsjq4M_c/edit?usp=sharing") 

lfc_18 <- gs_key(key) %>% 
  gs_read()
```

    ## Sheet successfully identified: "lfc_2018_2019"

    ## Accessing worksheet titled 'Sheet1'.

    ## Parsed with column specification:
    ## cols(
    ##   Date = col_date(format = ""),
    ##   season = col_integer(),
    ##   home = col_character(),
    ##   visitor = col_character(),
    ##   hgoal = col_integer(),
    ##   vgoal = col_integer(),
    ##   result = col_character()
    ## )
