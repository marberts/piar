# None of these functions are exported

different_length <- function(...) {
  res <- lengths(Filter(Negate(is.null), list(...)))
  any(res != res[1L])
}

sequential_names <- function(...) {
  f <- interaction(...)
  unsplit(Map(seq_len, tabulate(f)), f)
}

valid_product_names <- function(x, period) {
  x <- as.character(x)
  period <- as.factor(period)
  if (anyNA(x) || any(x == "")) {
    stop("each product must have a non-missing name")
  }
  unsplit(lapply(split(x, period), make.unique), period)
}

paste_until <- function(x, i) {
  paste(x[seq_len(i)], collapse = "")
}

nested_names <- function(x) {
  as.character(unlist(lapply(x, names), use.names = FALSE))
}

has_contrib <- function(x) {
  Position(\(x) any(lengths(x) > 0L), x$contrib, nomatch = 0L) > 0L
}

index_skeleton <- function(levels, time) {
  index <- rep.int(NA_real_, length(levels))
  rep.int(list(index), length(time))
}

empty_contrib <- function(x) {
  res <- rep.int(list(numeric(0L)), length(x))
  list(res)
}

contrib_skeleton <- function(levels, time) {
  rep.int(empty_contrib(levels), length(time))
}
