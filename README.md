
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
```

Use named arguments to change the default values of a function.

``` r
max_rm <- max |> but(na.rm = TRUE)
max_rm(0, NA, 2, 1)
#> [1] 2
```

`but()` can act on calls as well as functions.

Use unnamed arguments to preprocess inputs.

``` r
(x <- log(c(0, NA, 1)))
#> [1] -Inf   NA    0
min(x)
#> [1] NA
min(x) |> but(if(-Inf %in% c(...)) return(-Inf))
#> [1] -Inf
min_inf <- min |> but(if(-Inf %in% c(...)) return(-Inf))
min_inf(x)
#> [1] -Inf
```

Reference `.out` to modify the output of a function.

Use `on.exit()` to clean up after a function call.

``` r
(strsplit1 <- strsplit |> but(.out[[1]]))
#> function (x, split, fixed = FALSE, perl = FALSE, useBytes = FALSE) 
#> {
#>     .out <- strsplit(x = x, split = split, fixed = fixed, perl = perl, 
#>         useBytes = useBytes)
#>     .out[[1]]
#> }
#> <environment: 0x60a950035998>
strsplit1("a.b.c", ".", fixed = TRUE)
#> [1] "a" "b" "c"
read.csv |> but(stringsAsFactors = TRUE, on.exit(unlink(file)))
#> function (file, header = TRUE, sep = ",", quote = "\"", dec = ".", 
#>     fill = TRUE, comment.char = "", ..., stringsAsFactors = TRUE) 
#> {
#>     on.exit(unlink(file))
#>     read.csv(file = file, header = header, sep = sep, quote = quote, 
#>         dec = dec, fill = fill, comment.char = comment.char, 
#>         ..., stringsAsFactors = stringsAsFactors)
#> }
#> <environment: 0x60a9501d68c8>
```

Make `data` the first argument of `lm()` so it can be piped in.

``` r
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
```

Specify exactly what the call to `lm()` should look like with `:=`.

This process supports data masking with `rlang`.

``` r
resample <- function(x) x[sample(nrow(x), replace = TRUE), , drop = FALSE]
lm_resample <- lm |> but(data := resample({{data}}), .nse = TRUE)
lm_resample(mpg ~ disp, subset(mtcars, cyl == 4))$call
#> lm(formula = mpg ~ disp, data = resample(~subset(mtcars, cyl == 
#>     4)))

lm4pipe_resample <- lm |> but(
  data = , .first = TRUE, .nse = TRUE,
  data := resampled_data, resampled_data <- resample(data)
)
mtcars |> subset(cyl == 4) |> lm4pipe_resample(mpg ~ disp)
#> 
#> Call:
#> lm(formula = mpg ~ disp, data = resampled_data)
#> 
#> Coefficients:
#> (Intercept)         disp  
#>     43.4341      -0.1621
```

Change the default value of `drop` in `[`.

``` r
(m <- diag(4))
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    0    0    0
#> [2,]    0    1    0    0
#> [3,]    0    0    1    0
#> [4,]    0    0    0    1
m[2, ]
#> [1] 0 1 0 0
`[` <- `[` |> but(drop = FALSE, .store = TRUE)
#> Warning in but(`[`, drop = FALSE, .store = TRUE): .f is a primitive without a
#> well-defined argument list
m[2, ]
#>      [,1] [,2] [,3] [,4]
#> [1,]    0    1    0    0
m[2, , drop = TRUE]
#> [1] 0 1 0 0
rm(`[`)
```

Remove unwanted arguments with `.rm`.

Modify the call directly with `:=`.

``` r
(square <- matrix |> but(nrow = sqrt(length(data)), ncol = .rm, ncol := nrow))
#> function (data = NA, nrow = sqrt(length(data)), byrow = FALSE, 
#>     dimnames = NULL) 
#> {
#>     matrix(data = data, nrow = nrow, ncol = nrow, byrow = byrow, 
#>         dimnames = dimnames)
#> }
#> <environment: 0x60a9509d16b0>
square(1:9, byrow = TRUE)
#>      [,1] [,2] [,3]
#> [1,]    1    2    3
#> [2,]    4    5    6
#> [3,]    7    8    9
```

See `?but` for more information and examples.
