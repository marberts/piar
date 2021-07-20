# None of these functions are exported

same_length <- function(...) {
  res <- lengths(list(...))
  all(res == res[1])
}

any_negative <- function(...) {
  min(..., 1, na.rm = TRUE) <= 0 # the 1 stops the warnings with length-0 inputs
}

named_extract <- function(x, nm) {
  res <- x[nm]
  names(res) <- nm
  res
}

sequential_names <- function(x, ...) {
  f <- interaction(...)
  unsplit(Map(seq, tabulate(f)), f)
}

paste_until <- function(x, i) {
  paste(x[seq_len(i)], collapse = "")
}
