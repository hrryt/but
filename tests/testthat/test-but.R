test_that("argument mixing works", {
  y <- (0:10) / 4
  expect_equal(but(split, f = floor(x))(y), split(y, floor(y)))
})
test_that("extra arguments work", {
  expect_equal(but(rbind, a = 1)(b = 2), rbind(b = 2, a = 1))
  expect_equal(but(`%o%`, n = , X = seq_len(n), Y = X, .first = TRUE)(3), 1:3 %o% 1:3)
})
test_that("adding defaults works", {
  expect_equal(but(`mode<-`, value = "integer")(matrix((1:3)/2)), matrix(c(0, 1, 1)))
})
test_that("primitives work", {
  expect_equal(but(c, use.names = FALSE)(a = 1), 1)
})
test_that("NULL arguments work", {
  expect_equal(but(attributes, x = NULL)(), NULL)
})
test_that(".first works", {
  expect_equal(but(matrix, ncol =, data = 0, nrow = ncol, .first = TRUE)(2), matrix(0, 2, 2))
  expect_equal(but(rbind, a = 1, .first = TRUE)(b = 2), rbind(a = 1, b = 2))
})
test_that("missings are respected", {
  miss <- but(missing)
  expect_true(miss())
  expect_false(miss(NULL))
  expect_error(matrix() |> but(data = ), "argument \"data\" is missing, with no default")
})
test_that("primitives without a well-defined argument list work with warning", {
  expect_warning(
    `last<-` <- but(`[[<-`, x = , i = length(x), value = , .first = TRUE),
    ".f is a primitive without a well-defined argument list"
  )
  y <- 1:3
  last(y) <- 0
  expect_equal(y, c(1,2,0))
})
test_that(".wrap = FALSE avoids NSE pitfalls", {
  lm_for_pipe <- lm |> but(data =, .first = TRUE, .wrap = FALSE)
  expect_equal(
    (mtcars |> lm_for_pipe(mpg~disp))$call,
    quote(lm_for_pipe(data = mtcars, formula = mpg ~ disp))
  )
})
test_that(".out works", {
  slapply <- lapply |> but(.out |> simplify2array())
  x <- c(lapply(3:6, seq), list(c(NA,1)))
  expect_equal(slapply(x, fivenum, na.rm = FALSE), out <- sapply(x, fivenum, na.rm = FALSE))
  fapply <- lapply |> but(FUN = fivenum, na.rm = FALSE, .out |> simplify2array())
  expect_equal(fapply(x), out)
})
test_that("multiple .outs work", {
  mat <- matrix |> but(.out[is.na(.out)] <- replacement, replacement = 0, .out)
  expect_equal(mat(c(1, NA), 2, 3), matrix(c(1, 0), 2, 3))
})
test_that("unnamed arguments error without .wrap", {
  expect_error(
    but(matrix, data = 0, .out |> as.dist(), .wrap = FALSE),
    ".wrap must be TRUE if unnamed arguments are provided"
  )
})
test_that(".out errors if named", {
  expect_error(
    but(matrix, out = as.dist(.out)), "arguments that reference .out must be unnamed"
  )
})
test_that("unnamed arguments are inserted before the call to .f", {
  square <- matrix |> but(
    nrow = sqrt(length(data)), ncol = .rm, ncol <- nrow, data = 0, data <- as.numeric(data)
  )
  expect_equal(square(1:9, byrow = TRUE), matrix(1:9, 3, 3, byrow = TRUE))
  expect_equal(square(TRUE, 3), matrix(1, 3, 3))
})
test_that(".nse avoids NSE pitfalls", {
  resample <- function(x) x[sample(nrow(x), replace = TRUE), , drop = TRUE]
  lmx <- lm |> but(data = , .first = TRUE)
  lm2 <- lm |> but(data = , .first = TRUE, .nse = TRUE)
  lm3 <- lm |> but(data = , .first = TRUE, .nse = TRUE, data := resample(data))
  lm4 <- lm |> but(data = , .first = TRUE, .nse = TRUE, data := resampled_data,
                   resampled_data <- resample(data))
  lm5 <- lm |> but(data =, .first = TRUE, .nse = TRUE, data := resample({{data}}))
  lm6 <- lm |> but(data =, .first = TRUE, .nse = TRUE, data := resample2({{data}}),
                   resample2 <- resample)
  expect_error(lmx(mtcars, mpg~disp))
  mt <- resample(mtcars)
  expect_equal(lm2(mt, mpg~disp)$call, quote(lm(formula = mpg ~ disp, data = mt)))
  expect_equal(lm3(mtcars, mpg~disp)$call, quote(lm(formula = mpg ~ disp, data = resample(data))))
  expect_equal(lm4(mtcars, mpg~disp)$call, quote(lm(formula = mpg ~ disp, data = resampled_data)))
  mcall <- quote(lm(formula = mpg ~ disp, data = resample(quosure)))
  mcall[[3]][[2]] <- rlang::quo(subset(mtcars, cyl == 4))
  expect_equal((mtcars |> subset(cyl == 4) |> lm5(mpg ~ disp))$call, mcall)
  mcall[[3]][[1]] <- quote(resample2)
  expect_equal((mtcars |> subset(cyl == 4) |> lm6(mpg ~ disp))$call, mcall)
})
test_that(".nse works with .out", {
  resample <- function(x) x[sample(nrow(x), replace = TRUE), , drop = TRUE]
  lmcall <- lm |> but(data = , .first = TRUE, .nse = TRUE, data := resampled_data,
                   resampled_data <- resample(data), .out$call)
  expect_equal(lmcall(mtcars, mpg~disp), quote(lm(formula = mpg ~ disp, data = resampled_data)))
})
test_that(".nse works with .store", {
  resample <- function(x) x[sample(nrow(x), replace = TRUE), , drop = TRUE]
  lmf <- lm |> but(data = , .first = TRUE, .nse = TRUE, data := resampled_data,
                   resampled_data <- resample(data), .store = TRUE)
  expect_equal(lmf(mtcars, mpg~disp)$call, quote(.f(formula = mpg ~ disp, data = resampled_data)))
})
test_that("user-defined functions are found", {
  get_first <- function(x) x[1]
  mat <- matrix |> but(data <- get_first(data))
  expect_equal(mat(1:3), matrix(1))
})
test_that(".nse warns without .wrap", {
  expect_warning(but(lm, .wrap = FALSE, .nse = TRUE), ".wrap is FALSE, ignoring .nse")
})
test_that("unnamed arguments error without .wrap", {
  expect_error(but(lm, .wrap = FALSE, .out$call), ".wrap must be TRUE if unnamed arguments are provided")
})
test_that(".rm warns without .wrap", {
  expect_warning(
    mat <- matrix |> but(nrow = .rm, .wrap = FALSE),
    "removing arguments is dangerous when .wrap is FALSE"
  )
  expect_error(mat(), "did not find an argument")
})
test_that("walri with .rm work without .nse", {
  expect_equal(max(NA, na.rm = TRUE) |> but(na.rm := .rm), NA_integer_)
})
test_that("walri with .rm work with .nse", {
  expect_equal(max(NA, na.rm = TRUE) |> but(na.rm := .rm, .nse = TRUE), NA_integer_)
  expect_equal(max(NA, na.rm = TRUE) |> but(na.rm := .rm, a := 1, .nse = TRUE), NA_integer_)
})
test_that("walri with no RHS work", {
  expect_equal(quote() |> but(`:=`(expr, )), quote(expr=))
})
test_that("unnamed walri error with .nse", {
  expect_error(
    but(lm, .nse = TRUE, `:=`(, x)),
    "first argument of every `:=` operation must be specified if .nse is TRUE"
  )
  expect_equal(c(0) |> but(`:=`(, 1)), c(0, 1))
})
test_that("but works with calls", {
  x <- c(0, -Inf)
  expect_equal(min(NA, x) |> but(if(-Inf %in% c(...)) return(-Inf)), -Inf)
  resample <- function(x) x[sample(nrow(x), replace = TRUE), , drop = TRUE]
  model <- lm(mtcars, mpg~disp) |> but(
    data = , .first = TRUE, .nse = TRUE, data := resample(data)
  )
  expect_equal(model$call, quote(lm(formula = mpg ~ disp, data = resample(data))))
})
test_that("missing .f errors", {
  expect_error(but(), "argument \".f\" is missing, with no default")
})
