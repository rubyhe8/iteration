iteration_and_listcols
================
ruby
2025-10-28

``` r
library(tidyverse)
library(rvest)

knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Make a list

``` r
l = 
  list(
    vec_numeric = 1:23,
    char_vec = c("Ruby"),
    mat = matrix(1:8, nrow = 2, ncol = 4),
    summary = summary(rnorm(1000, mean = 4))
  )

l
```

    ## $vec_numeric
    ##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
    ## 
    ## $char_vec
    ## [1] "Ruby"
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $summary
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.891   3.312   3.987   4.006   4.666   7.323

``` r
l[[1]]
```

    ##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23

## Make a different list

``` r
list_normals = 
  list(
    a = rnorm(30, mean = 3, sd = 1),
    b = rnorm(30, mean = 30, sd = 1),
    c = rnorm(30, mean = 3, sd = 10),
    d = rnorm(30, mean = 3, sd = 4)
  )

list_normals
```

    ## $a
    ##  [1]  3.8010447  2.2097527  4.9726166  3.0510389  1.3477285  3.2363599
    ##  [7]  1.2004990  2.7819140  3.8049591  2.8071140  4.4264041  3.1611667
    ## [13]  4.0093481  0.7973102  2.7640298  2.9763527  3.7839251  4.5566202
    ## [19]  3.1595903  3.0934036  1.4796038  3.4649192  3.1558963  3.5384509
    ## [25]  1.7855512 -0.2502459  2.5777631  2.9785223  3.4276385  2.6051738
    ## 
    ## $b
    ##  [1] 31.55340 30.73467 31.50336 30.22342 28.68254 30.19099 31.57872 32.10174
    ##  [9] 30.15878 29.89613 31.31397 30.90243 30.96712 31.98335 30.66703 30.86811
    ## [17] 29.03675 31.76848 30.26283 30.68873 31.17716 29.96076 29.98176 30.55619
    ## [25] 30.04219 30.18492 30.16569 29.06491 29.31114 28.79440
    ## 
    ## $c
    ##  [1]  26.6630225  -0.5463701  10.1465014  15.3833066   9.7401240  -1.7289496
    ##  [7]  -6.8083730  -6.4634396   4.7259171  -9.1958315  -6.7654336   4.0007060
    ## [13]  -3.0099050   2.5933702   7.3505754   8.1018844   9.5660164  12.2986893
    ## [19]  31.3433525  -9.1905703  16.7757085 -18.6237735   4.9914848   7.4566271
    ## [25]   9.7059343   8.6466979  22.1812026  -0.2205624  26.6214684  24.5152222
    ## 
    ## $d
    ##  [1] -1.2002560  1.1937265  7.0872236  6.2741487  5.4516991 -1.5476346
    ##  [7]  7.7210726  2.1077909 -0.0777028  8.9295925  3.2924316  9.1449384
    ## [13]  4.1840527  4.6543295  4.0598006  0.1824287  3.1882340  0.4069321
    ## [19]  4.4938912  1.3249174 13.4894245  2.2868684  1.7589624 10.1068418
    ## [25] -3.7338110  1.5377943  6.1201259  3.7354877  2.4613079 -0.2910808

(copy and paste function from last time)

``` r
mean_and_sd = function(x) {
  
  if(!is.numeric(x)) {
    stop("The input x should be numeric")
  }
  
  if(length(x) < 5) {
    stop("Only comupute mean and sd when the input has 5 or more numbers")
  }
  
  mean_x = mean(x, na.rm = TRUE)
  sd_x = sd(x, na.rm = TRUE)
  
  tibble(
    mean = mean_x, 
    sd = sd_x)
  
}
```

``` r
mean_and_sd(list_normals[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.89  1.14

Use a loop to iterate!

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  output[[i]] = mean_and_sd(list_normals[[i]])
}

output
```

    ## [[1]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.89  1.14
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  30.5 0.927
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.68  12.0
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.61  3.83

Use `map` to do the same thing.

``` r
output = map(list_normals, mean_and_sd)


output = map(list_normals, median)
```

Check out some `map` variants

collapse maps into a dataframe

``` r
map_dfr(list_normals, mean_and_sd)
```

    ## # A tibble: 4 × 2
    ##    mean     sd
    ##   <dbl>  <dbl>
    ## 1  2.89  1.14 
    ## 2 30.5   0.927
    ## 3  6.68 12.0  
    ## 4  3.61  3.83

can keep track of an input id

``` r
map_dfr(list_normals, mean_and_sd, .id = "sample")
```

    ## # A tibble: 4 × 3
    ##   sample  mean     sd
    ##   <chr>  <dbl>  <dbl>
    ## 1 a       2.89  1.14 
    ## 2 b      30.5   0.927
    ## 3 c       6.68 12.0  
    ## 4 d       3.61  3.83

make result of single numbers into a dataframe of single numbers

``` r
map_dbl(list_normals, median)
```

    ##         a         b         c         d 
    ##  3.072221 30.409508  7.403601  3.240333

## LIST COLUMNS

try to put my list into a dataframe

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"),
    sample = list_normals
  )
```

did this really work - can check using`pull()`

``` r
pull(listcol_df, sample)
```

    ## $a
    ##  [1]  3.8010447  2.2097527  4.9726166  3.0510389  1.3477285  3.2363599
    ##  [7]  1.2004990  2.7819140  3.8049591  2.8071140  4.4264041  3.1611667
    ## [13]  4.0093481  0.7973102  2.7640298  2.9763527  3.7839251  4.5566202
    ## [19]  3.1595903  3.0934036  1.4796038  3.4649192  3.1558963  3.5384509
    ## [25]  1.7855512 -0.2502459  2.5777631  2.9785223  3.4276385  2.6051738
    ## 
    ## $b
    ##  [1] 31.55340 30.73467 31.50336 30.22342 28.68254 30.19099 31.57872 32.10174
    ##  [9] 30.15878 29.89613 31.31397 30.90243 30.96712 31.98335 30.66703 30.86811
    ## [17] 29.03675 31.76848 30.26283 30.68873 31.17716 29.96076 29.98176 30.55619
    ## [25] 30.04219 30.18492 30.16569 29.06491 29.31114 28.79440
    ## 
    ## $c
    ##  [1]  26.6630225  -0.5463701  10.1465014  15.3833066   9.7401240  -1.7289496
    ##  [7]  -6.8083730  -6.4634396   4.7259171  -9.1958315  -6.7654336   4.0007060
    ## [13]  -3.0099050   2.5933702   7.3505754   8.1018844   9.5660164  12.2986893
    ## [19]  31.3433525  -9.1905703  16.7757085 -18.6237735   4.9914848   7.4566271
    ## [25]   9.7059343   8.6466979  22.1812026  -0.2205624  26.6214684  24.5152222
    ## 
    ## $d
    ##  [1] -1.2002560  1.1937265  7.0872236  6.2741487  5.4516991 -1.5476346
    ##  [7]  7.7210726  2.1077909 -0.0777028  8.9295925  3.2924316  9.1449384
    ## [13]  4.1840527  4.6543295  4.0598006  0.1824287  3.1882340  0.4069321
    ## [19]  4.4938912  1.3249174 13.4894245  2.2868684  1.7589624 10.1068418
    ## [25] -3.7338110  1.5377943  6.1201259  3.7354877  2.4613079 -0.2910808

apply `mean_and_sd`

``` r
mean_and_sd(pull(listcol_df, sample)[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.89  1.14

iterate using `map`

``` r
map(pull(listcol_df, sample), mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.89  1.14
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  30.5 0.927
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.68  12.0
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.61  3.83
