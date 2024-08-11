#' Execute a Function Call
#'
#' Constructs and executes a function call from a function and extra arguments.
#'
#' @param .f a function
#' @param ... passed to `.f`
#'
#' @returns The result of `.f(...)`.
#' @seealso [but()], [`|>`].
#'
#' @examples
#' matrix |> but(ncol = nrow) |> do(0, nrow = 2)
#' sapply(c(mean, median, IQR), do, print(runif(5)))
#'
#' @export
do <- function(.f, ...) .f(...)
