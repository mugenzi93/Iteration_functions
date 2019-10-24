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

    ##  [1]  0.02975436 -0.94978109  0.78437132 -0.10147909  0.98623471
    ##  [6] -0.29123930  1.45178199  0.92924575  1.31455232 -0.28973538
    ## [11] -0.43242294 -0.48345952 -1.42708809  1.51456635 -1.19920459
    ## [16]  1.07682340  1.23747090 -1.61684730 -1.04492103 -1.36794835
    ## [21]  0.63378168  0.04147341  0.58555447 -0.45832641 -0.24308603
    ## [26] -1.29319993  0.53934067 -0.87482926  1.69483712 -0.74622015

``` r
(x_again - mean(x_again)) / sd(x_again)
```

    ##  [1]  1.75350811  0.58637252 -0.40866466 -1.93078535  0.37178094
    ##  [6]  0.30040950 -0.65497033  0.01942960 -1.50294620 -0.71488484
    ## [11]  1.28934143 -1.05296693  0.14125304  0.40162830 -1.25044181
    ## [16] -0.07207638 -0.23839790 -0.89882737 -1.22230042  1.03375412
    ## [21]  0.71573237  2.03729115 -0.53067350 -0.58017629  0.52959057
    ## [26]  0.26018784 -1.04572944  1.03217377  1.58746285  0.04392531

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
    ## [1] 23.64577
    ## 
    ## $sd_input
    ## [1] 2.108105
    ## 
    ## $z_score
    ##  [1]  0.705411750 -0.582980877  1.650384037  2.038743315  1.903032382
    ##  [6]  0.872553124 -1.494456526  0.417459300  0.003414578  0.584254741
    ## [11]  0.996752204 -0.219898315  0.070897769  0.802561839  0.298343961
    ## [16] -0.071730627  0.144445302 -0.552708251  0.037427412 -0.279309386
    ## [21] -1.319983276 -1.076622053  0.809974877 -0.594122099 -2.371947079
    ## [26] -0.945210465 -0.184257475 -0.834902522 -0.889723803  0.082196165
