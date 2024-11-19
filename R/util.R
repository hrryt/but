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
f_call <- function(.q, n, .r) {
  n <- `names<-`(lapply(n, as.symbol), `[<-`(n, n == "...", value = ""))
  .d <- is_rm(.r)
  keep <- .r[!.d]
  n[names(keep)] <- keep
  n[names(.r)[.d]] <- NULL
  as.call(c(list(.q), n))
}
bodify <- function(bod, has_out, o, i) {
  as.call(c(quote(`{`), c(i, if(has_out) list(call("<-", quote(.out), bod)) else bod, o)))
}

r_out <- function(i) ".out" %in% all.vars(i)
w_call <- function(i) is.call(i) && i[[1]] == quote(`:=`)
is_rm <- function(d) vapply(d, identical, logical(1), quote(.rm), USE.NAMES = FALSE)
references_out <- function(d) vapply(d, r_out, logical(1), USE.NAMES = FALSE)
is_walrus_call <- function(d) vapply(d, w_call, logical(1), USE.NAMES = FALSE)
walrus_list <- function(w) `names<-`(lapply(w, `[[`, 3), lapply(w, `[[`, 2))
