#' Modify a Function's Formals, Inputs and Output
#'
#' Constructs a function that calls the input function with modified formal arguments
#' and optionally additional processing of its inputs and output.
#'
#' This documentation assumes `.f` is a function, but it can be a call, in which
#' case `but()` is applied to the function of the call before evaluation of the call.
#' `but(f(x), y = x)` is equivalent to `but(f, y = x)(x)`.
#'
#' If `.f` is a primitive without a well-defined argument list,
#' its formals are assumed to be `alist(... = )` and a warning is given.
#'
#' If `.nse` is `TRUE`, a call to `.f` is constructed from `match.call()`
#' in the body of the returned function. This is useful if `.f` is, for example,
#' a modelling function that captures its call and displays it when printing.
#'
#' @section Named arguments:
#'
#' Each named argument supplied to `but()` replaces the default value of the
#' argument of `.f` with that name, or if it is not present in the formals of `.f`,
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
#' The walrus operator `:=` can be used to modify the call to `.f` directly,
#' regardless the value of `.nse`. For example, `but(.f, x := y)` might be
#' equivalent to `function(...) .f(..., x = y)`. This accepts data masking
#' if `.nse` is `TRUE`.
#'
#' @param .f 	a function (a primitive or a closure, i.e., 'non-primitive')
#' @param ... modified formals and instructions for pre- and post-processing
#' @param .first should supplied formals come first, and in the order specified?
#' @param .nse should the **n**on-**s**tandard **e**valuation of `.f` be accounted for
#' using [match.call()]?
#' @param .wrap should `.f` be wrapped in a new function or its formals modified directly?
#' @param .store should `.f` be stored in the environment of the returned function?
#' @param .pass_all should arguments not present in the formals of `.f` be passed to `.f`
#' if it has [dots] to absorb them?
#'
#' @returns A function.
#' @seealso [`|>`].
#'
#' @examples
#' # named arguments specify default values to change
#' (max_rm <- max |> but(na.rm = TRUE))
#' max_rm(0, NA, 2, 1)
#' (x <- log(c(0, NA, 1)))
#' min(x)
#' # unnamed arguments are added to the body of the returned function
#' (min_inf <- min |> but(if(-Inf %in% c(...)) return(-Inf)))
#' min_inf(x)
#' # applying but directly to a call saves dealing with the intermediate function
#' min(x) |> but(if(-Inf %in% c(...)) return(-Inf))
#'
#' args(lm)
#' # leaving data's value missing ensures there is no default value
#' # .first = TRUE puts data to the front of the argument list
#' (lm4pipe <- lm |> but(data = , .first = TRUE))
#' # non-standard evaluation in the body of lm makes wrapping difficult
#' try(mtcars |> subset(cyl == 4) |> lm4pipe(mpg ~ disp))
#' # use .wrap = FALSE to modify the formals of lm directly instead
#' lm4pipe <- lm |> but(data = , .first = TRUE, .wrap = FALSE)
#' mtcars |> subset(cyl == 4) |> lm4pipe(mpg ~ disp)
#'
#' resample <- function(x) x[sample(nrow(x), replace = TRUE), , drop = FALSE]
#' # wrapping that respects lm's NSE can be achieved with .nse = TRUE
#' # this allows the use of := to specify what the call to lm should look like
#' (lm4pipe_resample <- lm |> but(
#'   data = , .first = TRUE, .nse = TRUE,
#'   data := resampled_data, resampled_data <- resample(data)
#' ))
#' mtcars |> subset(cyl == 4) |> lm4pipe_resample(mpg ~ disp)
#' # the RHS of := also accepts data masking
#' lm_resample <- lm |> but(data := resample({{data}}), .nse = TRUE)
#' lm_resample(mpg ~ disp, subset(mtcars, cyl == 4))
#'
#' (m <- diag(4))
#' m[2, ] # `[` defaults to drop = TRUE
#' # .store = TRUE ensures the initial `[` function is stored as .f
#' (`[` <- `[` |> but(drop = FALSE, .store = TRUE))
#' environment(`[`)$.f
#' m[2, ] # `[` now defaults to drop = FALSE
#' m[2, , drop = TRUE]
#' rm(`[`)
#'
#' (none <- Negate(any))
#' # unnamed arguments that reference .out are added after the call to .f
#' (none <- any |> but(!.out))
#' strsplit |> but(.out[[1]])
#' subset |> but(drop = TRUE, droplevels(.out))
#' # use on.exit() if post-processing does not reference .out
#' read.csv |> but(on.exit(unlink(file)))
#'
#' # use .rm to remove arguments
#' (square <- matrix |> but(ncol = .rm, ncol <- nrow, nrow = sqrt(length(data))))
#' # use := to modify the call directly
#' (square <- matrix |> but(ncol = .rm, ncol := nrow, nrow = sqrt(length(data))))
#' square(1:9, byrow = TRUE)
#' square(1, 3)
#'
#' # use := .rm to remove arguments from the call
#' read.csv |> but(
#'   keep.white = TRUE, strip.white := !keep.white, keep.white := .rm
#' )
#' # use .pass_all = FALSE to avoid passing
#' # extra arguments to the dots of .f
#' read.csv |> but(
#'   keep.white = TRUE, strip.white := !keep.white, .pass_all = FALSE
#' )
#'
#' (cb1 <- cbind |> but(`:=`(, 1))) # := missing first argument
#' cb1(1:6, 1:2)
#'
#' @export
but <- function(.f, ..., .first = FALSE, .nse = FALSE,
                .wrap = TRUE, .store = FALSE, .pass_all = TRUE) {
  if(missing(.f)) force(.f)
  s <- substitute(.f)
  is_call <- is.call(s)
  if(is_call) .f <- s[[1]]
  .q <- if(.store) quote(.f) else substitute(.f)
  .f <- match.fun(.f)
  named <- nzchar(names(d <- dots(match.call())))
  has_out <- any(out <- references_out(d))
  has_walri <- any(walrus <- is_walrus_call(d))
  r <- is_rm(d)
  stopifnot(
    ".wrap must be TRUE if .f is primitive" = .wrap || !is.primitive(.f),
    ".wrap must be TRUE if unnamed arguments are provided" = .wrap || all(named),
    "arguments that reference .out must be unnamed" = all(!named | !out)
  )
  if(.nse && !.wrap) warning(".wrap is FALSE, ignoring .nse")
  if(i <- is.null(a <- args(.f)))
    warning(".f is a primitive without a well-defined argument list")
  fn <- modify(fm <- args2formals(a, i), d[named], .first)
  fn_rmd <- fn[!names(fn) %in% names(d)[r]]
  if(!any(names(fm) == "...")) .pass_all <- FALSE
  .f <- if(.wrap) {
    .r <- walrus_list(d[walrus])
    di <- d[!(named | out | walrus)]
    env <- new.env(parent = parent.frame())
    if(.nse) {
      stopifnot(
        "first argument of every `:=` operation must be specified if .nse is TRUE" =
          all(nzchar(names(.r)))
      )
      di <- c(di, list(quote(.m <- match.call())))
      env$.q <- .q
      fcall <- quote(rlang::eval_tidy(.m))
      if(has_walri) {
        .d <- is_rm(.r)
        .da <- any(.d)
        if(!all(.d)) {
          di <- c(di, list(quote(.r <- do.call(rlang::exprs, .r))))
          di <- c(di, list(quote(.m[names(.r)] <- .r)))
          env$.r <- if(.da) .r[!.d] else .r
        }
        if(.da) {
          di <- c(di, list(quote(.m[.d] <- NULL)))
          env$.d <- names(.r)[.d]
        }
      }
      di <- c(di, list(quote(.m[[1]] <- .q)))
    } else fcall <- f_call(.q, names(if(.pass_all) fn else fm), .r)
    bod <- bodify(fcall, has_out, d[out], di)
    if(.store) env$.f <- .f
    as.function(c(fn_rmd, bod), env)
  } else {
    if(any(r)) warning("removing arguments is dangerous when .wrap is FALSE")
    `formals<-`(.f, value = fn_rmd)
  }
  if(is_call) {
    s[[1]] <- quote(.f)
    return(eval(s, envir = list(.f = .f), enclos = parent.frame()))
  }
  .f
}
