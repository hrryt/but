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
wrapper <- function(.f, fn, fm, .pass_all, has_out, d, o, i, r) {
  bod <- bodify(f_call(names(if(.pass_all) fn else fm)), has_out, d[o], d[!i])
  as.function(c(fn[!names(fn) %in% names(d)[r]], bod), list2env(list(.f = .f)))
}
f_call <- function(n) {
  as.call(c(list(quote(.f)), `names<-`(lapply(n, as.symbol), `[<-`(n, n == "...", value = ""))))
}
r_out <- function(i) any(grepl("out", i, fixed = TRUE))
references_out <- function(d) vapply(d, r_out, logical(1))
is_rm <- function(d) vapply(d, identical, logical(1), quote(.rm))
bodify <- function(bod, has_out, o, i) {
  as.call(c(quote(`{`), c(i, if(has_out) list(call("<-", quote(.out), bod)) else bod, o)))
}
