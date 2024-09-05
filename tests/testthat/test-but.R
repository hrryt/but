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
  expect_error(but(matrix, data =)(), "argument \"data\" is missing, with no default")
})
test_that("primitives without a well-defined argument list work with warning", {
  expect_warning(
    `last<-` <- but(`[[<-`, x = , i = length(x), value = ),
    ".f is a primitive without a well-defined argument list"
  )
  y <- 1:3
  last(y) <- 0
  expect_equal(y, c(1,2,0))
})
test_that(".wrap = FALSE avoids NSE pitfalls", {
  lm_for_pipe <- lm |> but(data =, .first = TRUE, .wrap = FALSE)
  expect_s3_class(mtcars |> subset(cyl == 4) |> lm_for_pipe(mpg ~ disp), "lm")
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
test_that(".out and .rm error without .wrap", {
  error <- ".wrap must be TRUE if .out or .rm is referenced"
  expect_error(but(matrix, ncol = .rm, .wrap = FALSE), error)
  expect_error(but(matrix, data = 0, .out |> as.dist(), .wrap = FALSE), error)
})
test_that(".out errors if named", {
  expect_error(
    but(matrix, out = as.dist(.out)), "any arguments that reference .out must not be named"
  )
})
test_that("unnamed arguments are inserted before the call to .f", {
  square <- matrix |> but(
    nrow = sqrt(length(data)), ncol = .rm, ncol <- nrow, data = 0, data <- as.numeric(data)
  )
  expect_equal(square(1:9, byrow = TRUE), matrix(1:9, 3, 3, byrow = TRUE))
  expect_equal(square(TRUE, 3), matrix(1, 3, 3))
})
