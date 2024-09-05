#' Modify a Function's Formals, Inputs and Output
#'
#' Constructs a function that calls the input function with modified formal arguments
#' and optionally additional processing of its inputs and output.
#'
#' If `.f` is a primitive without a well-defined argument list, a warning is given,
#' its formals are assumed to be `alist(... = )`, and `.first` is set to `TRUE`.
#'
#' @section Named arguments:
#'
#' Each named argument supplied to `but()` replaces the formal argument of `.f`
#' with that name, or if it is not present in the formals of `.f`,
#' is added to the end of the argument list.
#'
#' A formal argument can be removed by quoting `.rm`, e.g. `but(.f, x = .rm)`.
#'
#' @section Unnamed arguments:
#'
#' Unnamed arguments supplied to `but()` are used as language objects
#' to build the body of the returned function.
#'
#' If an unnamed argument references `.out`, e.g. `but(.f, g(.out))`,
#' it is appended to the body after the call to `.f` is assigned to `.out`,
#' such that the output of `.f` can be modified before it is returned.
#'
#' Otherwise, unnamed arguments are added to the body before the call to `.f`,
#' for example so that arguments can be modified before being passed to `.f`.
#'
#' @param .f 	a function (a primitive or a closure, i.e., “non-primitive”)
#' @param ... modified formals and instructions for pre- and post-processing
#' @param .first should supplied formals come first, and in the order specified?
#' @param .wrap should `.f` be wrapped in a new function or its formals modified directly?
#' @param .pass_all should arguments not present in the formals of `.f` be passed to `.f`
#' if it has [dots] to absorb them?
#'
#' @returns A function.
#' @seealso [`|>`].
#'
#' @examples
#' max_rm <- max |> but(na.rm = TRUE)
#' max_rm(0, NA, 2, 1)
#' (x <- log(c(0, NA, 1)))
#' min(x)
#' min_inf <- min |> but(if(-Inf %in% c(...)) return(-Inf))
#' min_inf(x)
#'
#' read.csv |> but(stringsAsFactors = TRUE, on.exit(unlink(file)))
#'
#' # remove arguments with .rm
#' (square <- matrix |> but(
#'   nrow = sqrt(length(data)), ncol = .rm, ncol <- nrow,
#'   data = 0, data <- as.numeric(data)
#' ))
#' square(1:9, byrow = TRUE)
#' square(TRUE, 3)
#'
#' aq <- transform(airquality, Month = factor(Month, labels = month.abb[5:9]))
#' # an argument that references .out
#' (subset_drop <- subset |> but(drop = TRUE, droplevels(.out)))
#' table(subset     (aq, Month != "Jul")$Month)
#' table(subset_drop(aq, Month != "Jul")$Month)
#'
#' # use .first to order arguments
#' (start_repeats <- grepl |> but(
#'   x = , n = 2, pattern = .rm, .first = TRUE,
#'   pattern <- sprintf("^%s{%i}", substr(x, 1, 1), n)
#' ))
#' start_repeats("hhi", 3)
#' start_repeats("Hhello", ignore.case = TRUE)
#'
#' `+` #primitive
#' double <- `+` |> but(e2 = e1)
#' double(4)
#'
#' but(lm) # lm(*, weights = weights) will error if run
#' # use .wrap = FALSE to avoid the pitfalls of
#' # non-standard evaluation in the body of .f
#' lm_for_pipe <- lm |> but(data = , .first = TRUE, .wrap = FALSE)
#' mtcars |> subset(cyl == 4) |> lm_for_pipe(mpg ~ disp)
#'
#' # use .pass_all = FALSE to avoid passing
#' # extra arguments to the dots of .f
#' (times_table <- outer |> but(
#'   n = , X = .rm, Y = .rm, Y <- X <- seq_len(n),
#'   .first = TRUE, .pass_all = FALSE
#' ))
#' times_table(4)
#'
#' (numbers <- seq(1, 3, 0.5))
#' but(split, f = floor(x))(numbers) #equivalent to using pipe
#'
#' #dangerous; see warning
#' `last<-` <- but(`[[<-`, x = , i = length(x), value = )
#' last(numbers) <- 0
#' numbers
#'
#' @export
but <- function(.f, ..., .first = FALSE, .wrap = TRUE, .pass_all = TRUE) {
  .f <- match.fun(.f)
  use <- nzchar(names(d <- dots(match.call())))
  has_out <- sum(out <- references_out(d))
  r <- is_rm(d)
  stopifnot(
    "any arguments that reference .out must not be named" = !any(out & use),
    ".wrap must be TRUE if .f is primitive" = .wrap || !is.primitive(.f),
    ".wrap must be TRUE if .out or .rm is referenced" = .wrap || !(has_out | any(r))
  )
  if(i <- is.null(a <- args(.f))) {
    warning(".f is a primitive without a well-defined argument list")
    .first <- TRUE
  }
  fn <- modify(fm <- args2formals(a, i), d[use], .first)
  if(!any(names(fm) == "...")) .pass_all <- FALSE
  if(.wrap) wrapper(.f, fn, fm, .pass_all, has_out, d, out, use | out, r) else
    `formals<-`(.f, value = fn)
}
