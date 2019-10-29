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

    ##  [1]  0.277479164 -0.596607915 -1.268612546 -0.001371979  2.235835905
    ##  [6] -1.280505782 -0.251383689 -1.393021347  0.824124155 -0.244828335
    ## [11]  1.109961159 -0.487799528  0.753790392  0.520413677 -1.000935165
    ## [16] -0.166384560  0.259096773  1.070620006 -0.785634357 -0.308916278
    ## [21] -1.391270444 -0.200841759 -0.629134071  1.925454303  1.409999644
    ## [26] -0.120558348  0.472800069  0.459898883 -1.837763114  0.646095088

``` r
(x_again - mean(x_again)) / sd(x_again)
```

    ##  [1]  0.38798386 -0.90880980 -0.50252755 -0.32706738 -2.29348401
    ##  [6] -0.35619703  1.86612827 -1.21156358  0.74854263  1.72344856
    ## [11] -0.62449206  0.62773415 -0.38438423  1.32138323 -1.02077041
    ## [16] -0.61245153  0.03705317  0.29826861 -0.50958424  0.20328754
    ## [21]  1.33664567  0.67364360 -1.44485325 -1.04693715  0.79689757
    ## [26]  0.55309383 -0.63346211  1.24679902 -0.67484661  0.73052124

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
    ## [1] 24.45551
    ## 
    ## $sd_input
    ## [1] 2.485124
    ## 
    ## $z_score
    ##  [1] -2.04646815  0.14921713 -0.96472589 -1.04863428 -0.29676777
    ##  [6]  1.42880013  1.00222098 -2.05745176  1.39907300  0.01077767
    ## [11] -0.38525526  0.89781459 -0.10784577 -0.08848034  0.15511782
    ## [16]  0.74007839  0.35987722  0.47412799 -0.10399320  1.17182260
    ## [21] -0.36828187  0.94316111  1.12307086 -2.40092022  0.84064953
    ## [26]  0.25294071 -0.94249435 -0.58399785  0.21577230  0.23079469

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
    ## 1      17.0     -3.01

## Scrape lots of napoleon (on Amazon)

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-title") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text()

review_text = dynamite_html %>%
    html_nodes(".review-data:nth-child(4)") %>%
    html_text()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

Now as a function

``` r
read_page_reviews <- function(url) {
  
  h = read_html(url)
  
  review_titles = h %>%
    html_nodes("#cm_cr-review_list .review-title") %>%
    html_text()
  
  review_stars = h %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("\\d") %>%
    as.numeric()
  
  review_text = h %>%
    html_nodes(".review-data:nth-child(4)") %>%
    html_text()
  
  tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
}
```
