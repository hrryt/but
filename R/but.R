#' Modify a Function's Formals or Output
#'
#' Constructs a function that calls the input function with modified formal arguments
#' and/or additional processing of its output.
#'
#' Each named argument supplied to `but()` replaces the formal argument of `.f`
#' with that name. Otherwise it is added to the end of the argument list and,
#' if `.pass_all`, passed to `.f `.
#'
#' If `.f` is a primitive without a well-defined argument list, a warning is given,
#' its formals are assumed to be `alist(... = )`, and `.first` is set to `TRUE`.
#'
#' If an argument supplied to `but()` 'references' `.out` (see examples),
#' it is treated as a language object and appended to the body of the returned function
#' after the call to `.f` is assigned to `.out`, such that the output of `.f`
#' can be modified before it is returned.
#'
#' @param .f 	a function (a primitive or a closure, i.e., “non-primitive”)
#' @param ... modified formals
#' @param .first should supplied formals come first, and in the order specified?
#' @param .wrap should `.f` be wrapped in a new function or its formals modified directly?
#' @param .pass_all should arguments not present in the formals of `.f` be passed to `.f`
#' if it has [dots] to absorb them?
#'
#' @returns A function.
#' @seealso [`|>`], [do()].
#'
#' @examples
#' # supply default arguments to read.table
#' read.csv |> but(stringsAsFactors = TRUE, strip.white = TRUE)
#'
#' `+` #primitive
#' double <- `+` |> but(e2 = e1)
#' double(4)
#'
#' start_repeats <- grepl |> but( #create new argument n
#'   x = , n = 2, pattern = sprintf("^%s{%i}", substr(x, 1, 1), n),
#'   .first = TRUE
#' )
#' start_repeats("hhi", 3)
#' start_repeats("Hhello", ignore.case = TRUE)
#'
#' # an argument that references .out
#' (slapply <- lapply |> but(.out |> simplify2array()))
#' 3:9 |> slapply(seq) |> slapply(fivenum, na.rm = FALSE)
#' cor_dist <- cor |> but({
#'   dd <- as.dist((1 - .out)/2)
#'   plot(hclust(dd))
#'   dd
#' })
#' cor_dist(USJudgeRatings, method = "spearman") |> round(2)
#'
#' but(lm) # lm(weights = weights) will error if run
#' # use .wrap = FALSE to avoid the pitfalls of
#' # non-standard evaluation in the body of .f
#' lm_for_pipe <- lm |> but(data = , .first = TRUE, .wrap = FALSE)
#' mtcars |> subset(cyl == 4) |> lm_for_pipe(mpg ~ disp)
#'
#' # use .pass_all = FALSE to avoid passing
#' # extra arguments to the dots of .f
#' outer |> but(
#'   n = , X = seq_len(n), Y = X,
#'   .first = TRUE, .pass_all = FALSE
#' ) |> print() |> do(4) |> print()
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
  .f
  use <- nzchar(names(d <- dots(match.call())))
  has_out <- sum(out <- vapply(d, references_out, logical(1)))
  stopifnot(
    ".f must be a function" = is.function(.f),
    "all arguments bar .f and .out must be named" = all(xor(out, use)),
    ".wrap must be TRUE if .f is primitive" = !is.primitive(.f) || .wrap,
    "at most one argument can reference .out" = has_out <= 1,
    ".wrap must be TRUE if .out is referenced" = !has_out || .wrap
  )
  if(i <- is.null(a <- args(.f))) {
    warning(".f is a primitive without a well-defined argument list")
    .first <- TRUE
  }
  fn <- modify(fm <- args2formals(a, i), d[use], .first)
  if(!any(names(fm) == "...")) .pass_all <- FALSE
  if(.wrap) wrapper(.f, fn, fm, .pass_all, out, has_out, d) else
    `formals<-`(.f, value = fn)
}

dots <- function(m, f = sys.function(sys.parent())) {
  as.list(m <- m[-1])[!names(m) %in% (n <- names(formals(f)))[n != "..."]]
}
args2formals <- function(a, i) {
  if(i) alist(... = ) else if(is.null(fm <- formals(a))) list() else fm
}
modify <- function(fm, x, .first) {
  fm[nx <- names(x)] <- x
  if(.first) c(x, fm[!names(fm) %in% nx]) else fm
}
wrapper <- function(.f, fn, fm, .pass_all, out, has_out, d) {
  bod <- f_call(names(if(.pass_all) fn else fm))
  as.function(c(fn, bodify(bod, out, has_out, d)), list2env(list(.f = .f)))
}
f_call <- function(n) {
  as.call(c(list(quote(.f)), `names<-`(lapply(n, as.symbol), `[<-`(n, n == "...", value = ""))))
}
references_out <- function(i) {
  any(grepl(".out", i, fixed = TRUE))
}
bodify <- function(bod, out, has_out, d) {
  if(has_out) call("{", call("<-", quote(.out), bod), d[out][[1]]) else bod
}
