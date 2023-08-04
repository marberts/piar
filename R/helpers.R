# None of these functions are exported

different_length <- function(...) {
  res <- lengths(Filter(Negate(is.null), list(...)))
  any(res != res[1L])
}

sequential_names <- function(...) {
  f <- interaction(...)
  unsplit(Map(seq_len, tabulate(f)), f)
}

valid_product_names <- function(x, period, ea) {
  x <- as.character(x)
  if (anyNA(x) || any(x == "")) {
    stop("each product must have a non-missing name")
  }
  f <- interaction(period, ea)
  unsplit(lapply(split(x, f), make.unique), f)
}

paste_until <- function(x, i) {
  paste(x[seq_len(i)], collapse = "")
}

nested_names <- function(x) {
  as.character(unlist(lapply(x, names), use.names = FALSE))
}

empty_contrib <- function(x) {
  res <- rep.int(list(structure(numeric(0L), names = character(0L))), length(x))
  names(res) <- x
  list(res)
}

has_contrib <- function(x) {
  Position(\(x) any(lengths(x) > 0L), x$contrib, nomatch = 0L) > 0L
}
