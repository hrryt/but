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
test_that("piping works", {
  expect_equal(matrix |> but(ncol = nrow) |> do(0, 2), matrix(0, 2, 2))
})
test_that("unnamed arguments error", {
  expect_error(but(matrix, 0), "all arguments bar .f and .out must be named")
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
  x <- c(lapply(3:6, seq),list(c(NA,1)))
  expect_equal(slapply(x, fivenum, na.rm = FALSE), sapply(x, fivenum, na.rm = FALSE))
})
