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

    ##  [1]  1.063034760 -0.431122370 -1.718543591 -0.517876189  1.034348527
    ##  [6]  0.876200697  1.109180458  0.397065034 -1.325861456  0.867915525
    ## [11] -0.508318374  1.079010561  2.891406150 -0.738667793 -0.204433846
    ## [16] -1.088361836 -0.978253242 -1.263577480  0.027943748  0.231393572
    ## [21] -0.004413309 -0.904469200  0.316051004 -0.224002083 -0.699561117
    ## [26]  0.251620443 -0.256717834 -0.301626987  1.597613741 -0.576977515

``` r
(x_again - mean(x_again)) / sd(x_again)
```

    ##  [1]  1.018352672  0.455655127 -0.600762328  0.828301526  0.247041864
    ##  [6] -0.515201170 -0.294625593 -1.489404495  0.805749787  1.550901015
    ## [11]  0.387839148  1.635259889  2.216991125 -1.867467243  0.003214584
    ## [16]  0.276929469 -0.318545484 -1.039852369 -1.653638431 -0.605905101
    ## [21] -0.235871272  0.757798950  0.151614346  0.291396119 -1.776149969
    ## [26] -0.740594030  0.630228173  0.497699333 -0.027457317 -0.589498326

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
    ## [1] 23.47529
    ## 
    ## $sd_input
    ## [1] 2.350279
    ## 
    ## $z_score
    ##  [1]  0.53016025  0.16940520 -0.99902608  1.87969535 -0.40150175
    ##  [6]  0.14410806 -1.10093724  0.11497848 -1.08997195 -0.28320949
    ## [11] -0.48512623  0.06739361 -2.48351553  0.68525609  1.42602245
    ## [16] -0.09776950 -0.84219471  1.00661519  0.34808060 -1.23701633
    ## [21]  1.26227035  0.88238699 -1.63754668  1.38022009 -0.10532000
    ## [26] -0.44306304 -0.26946676  0.25287099  1.37275446 -0.04655289

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
    ## 1      17.0     -2.96

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
