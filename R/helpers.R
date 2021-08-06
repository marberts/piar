# None of these functions are exported

different_length <- function(...) {
  res <- lengths(list(...))
  any(res != res[1])
}

any_negative <- function(...) {
  min(..., 1, na.rm = TRUE) <= 0 # the 1 stops the warnings with length-0 inputs
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
