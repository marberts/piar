# None of these functions are exported

different_length <- function(...) {
  res <- lengths(Filter(Negate(is.null), list(...)))
  any(res != res[1L])
}

named_extract <- function(x, nm) {
  nm <- as.character(nm)
  structure(x[nm], names = nm)
}

sequential_names <- function(...) {
  f <- interaction(...)
  unsplit(Map(seq_len, tabulate(f)), f)
}

paste_until <- function(x, i) {
  paste(x[seq_len(i)], collapse = "")
}

nested_names <- function(x) {
  as.character(unlist(lapply(x, names), use.names = FALSE))
}

empty_contrib <- function(x) {
  res <- rep.int(list(numeric(0L)), length(x))
  names(res) <- x
  list(res)
}

has_contrib <- function(x) {
  Position(\(x) any(lengths(x) > 0L), x$contrib, nomatch = 0L) > 0L
}
