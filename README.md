
<!-- README.md is generated from README.Rmd. Please edit that file -->

# but

<!-- badges: start -->
<!-- badges: end -->

The goal of but is to allow users to easily edit existing functions to
suit their specific needs. Edit the formals of a function and apply
additional processing to its inputs and output.

## Installation

You can install the development version of but from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("hrryt/but")
```

## Examples

``` r
library(but)

max_rm <- max |> but(na.rm = TRUE)
max_rm(0, NA, 2, 1)
#> [1] 2
(x <- log(c(0, NA, 1)))
#> [1] -Inf   NA    0
min(x)
#> [1] NA
min_inf <- min |> but(if(-Inf %in% c(...)) return(-Inf))
min_inf(x)
#> [1] -Inf
read.csv |> but(stringsAsFactors = TRUE, on.exit(unlink(file)))
#> function (file, header = TRUE, sep = ",", quote = "\"", dec = ".", 
#>     fill = TRUE, comment.char = "", ..., stringsAsFactors = TRUE) 
#> {
#>     on.exit(unlink(file))
#>     .f(file = file, header = header, sep = sep, quote = quote, 
#>         dec = dec, fill = fill, comment.char = comment.char, 
#>         ..., stringsAsFactors = stringsAsFactors)
#> }
#> <environment: 0x000001ce6fea0e38>
args(lm)
#> function (formula, data, subset, weights, na.action, method = "qr", 
#>     model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, 
#>     contrasts = NULL, offset, ...) 
#> NULL
lm4pipe <- lm |> but(data = , .first = TRUE, .wrap = FALSE)
args(lm4pipe)
#> function (data, formula, subset, weights, na.action, method = "qr", 
#>     model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, 
#>     contrasts = NULL, offset, ...) 
#> NULL
mtcars |> subset(cyl == 4) |> lm4pipe(mpg ~ disp)
#> 
#> Call:
#> lm4pipe(data = subset(mtcars, cyl == 4), formula = mpg ~ disp)
#> 
#> Coefficients:
#> (Intercept)         disp  
#>     40.8720      -0.1351
(square <- matrix |> but(
  nrow = sqrt(length(data)), ncol = .rm, ncol <- nrow,
  data = 0, data <- as.numeric(data)
))
#> function (data = 0, nrow = sqrt(length(data)), byrow = FALSE, 
#>     dimnames = NULL) 
#> {
#>     ncol <- nrow
#>     data <- as.numeric(data)
#>     .f(data = data, nrow = nrow, ncol = ncol, byrow = byrow, 
#>         dimnames = dimnames)
#> }
#> <environment: 0x000001ce704f9820>
square(1:9, byrow = TRUE)
#>      [,1] [,2] [,3]
#> [1,]    1    2    3
#> [2,]    4    5    6
#> [3,]    7    8    9
square(TRUE, 3)
#>      [,1] [,2] [,3]
#> [1,]    1    1    1
#> [2,]    1    1    1
#> [3,]    1    1    1
aq <- transform(airquality, Month = factor(Month, labels = month.abb[5:9]))
(subset_drop <- subset |> but(drop = TRUE, droplevels(.out)))
#> function (x, ..., drop = TRUE) 
#> {
#>     .out <- .f(x = x, ..., drop = drop)
#>     droplevels(.out)
#> }
#> <environment: 0x000001ce6ebbcb38>
table(subset     (aq, Month != "Jul")$Month)
#> 
#> May Jun Jul Aug Sep 
#>  31  30   0  31  30
table(subset_drop(aq, Month != "Jul")$Month)
#> 
#> May Jun Aug Sep 
#>  31  30  31  30
```
