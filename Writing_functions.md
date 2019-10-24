Writing\_functions
================
Clement Mugenzi
10/24/2019

## Get started

We are going to write some code

``` r
x = rnorm(n = 30, mean = 4, sd = 2.3)
x_again = rnorm(n = 30, mean = 6, sd = .3) 
y = rnorm(n = 30, mean = 24, sd = 2.3)

(x - mean(x)) / sd(x)
```

    ##  [1] -0.14116356  1.93094062  0.78820854  0.62540970 -0.36811963
    ##  [6]  0.01014756  0.06897321  0.75254657 -0.55119417 -0.76402924
    ## [11]  0.29221668 -2.41942583 -0.74387654 -0.96301450 -0.98753015
    ## [16]  2.18043708  0.73901588 -0.49594466 -0.62420478  0.03582206
    ## [21] -0.55397612 -0.49956843 -0.02881828 -0.47130969  0.13845007
    ## [26] -1.13058172  2.03305729 -0.57860431  0.49031465  1.23582170

``` r
(x_again - mean(x_again)) / sd(x_again)
```

    ##  [1] -0.671286113  0.260995412  0.589347276  0.348378201  1.686805383
    ##  [6] -0.202999819 -0.001945705 -0.412024864  1.264730736  1.797237067
    ## [11]  0.201741668 -1.031026891 -0.555009578 -1.854964720  0.228279591
    ## [16] -0.147054668  0.573731901 -0.660812334  1.280300953  0.474446952
    ## [21] -1.567585009  0.945549338  1.059023808  0.397463029 -0.920173009
    ## [26] -0.802060448 -1.880058796  1.214369610 -1.067684645 -0.547714327

Now writing a function

``` r
z_score = function(x_arg) {
  
  if (!is.numeric(x_arg)) {
    stop("x should be numeric")
  } else if (length(x_arg) < 3) {
    stop("x should be longer than 3")
  } 
  
  (x_arg - mean(x_arg)) / sd(x_arg)
  
}
```

``` r
z_score(x_arg = 3)
```

    ## Error in z_score(x_arg = 3): x should be longer than 3

``` r
z_score(x_arg = "my name is jeff")
```

    ## Error in z_score(x_arg = "my name is jeff"): x should be numeric

``` r
z_score(x_arg = c(TRUE, TRUE, FALSE, TRUE))
```

    ## Error in z_score(x_arg = c(TRUE, TRUE, FALSE, TRUE)): x should be numeric

``` r
z_score(x_arg = iris)
```

    ## Error in z_score(x_arg = iris): x should be numeric

# Multiple Outputs

``` r
mean_and_sd = function(input_x) {
  
  if (!is.numeric(input_x)) {
    stop("x should be numeric")
  } else if (length(input_x) < 3) {
    stop("x should be longer than 3")
  } 
  
  list(
    mean_input = mean(input_x),
    sd_input = sd(input_x),
    z_score = (input_x - mean(input_x)) / sd(input_x)
  )
  
}
```

test this function

``` r
mean_and_sd(input_x = y)
```

    ## $mean_input
    ## [1] 23.5373
    ## 
    ## $sd_input
    ## [1] 2.29998
    ## 
    ## $z_score
    ##  [1]  1.56731673 -0.64159685  0.05625783  0.17629201  0.03370883
    ##  [6]  0.74164993 -2.56893324 -0.32181849  0.81125618  2.23687597
    ## [11] -0.68928914 -0.88133948  0.21736049 -0.43446176 -0.20195238
    ## [16] -1.29591495  0.30979788  1.18976431  1.19494969 -0.71423811
    ## [21]  0.38557612 -1.35408772 -0.41164418  0.69241621  0.38566050
    ## [26] -0.34643307 -1.68106924  0.53488889  0.54377021  0.46523685

# Multiple Inputs

``` r
sim_data = tibble(
  x = rnorm(30, mean = 1, sd = 1),
  y = 2 + 3 * x + rnorm(30, 0, 1)
)

ls_fit = lm(y ~ x, data = sim_data)
  
beta0_hat = coef(ls_fit)[1]
beta1_hat = coef(ls_fit)[2]
```

``` r
sim_regression = function(n, beta0, beta1) {
  sim_data = tibble(
  x = rnorm(n, mean = 1, sd = 1),
  y = beta0 + beta1 * x + rnorm(n, 0, 1)
)

ls_fit = lm(y ~ x, data = sim_data)
  tibble(
    beta0_hat = coef(ls_fit)[1],
    beta1_hat = coef(ls_fit)[2]
  )
}
sim_regression(n = 3000, beta0 = 17, beta1 = -3)
```

    ## # A tibble: 1 x 2
    ##   beta0_hat beta1_hat
    ##       <dbl>     <dbl>
    ## 1      17.0     -2.95
