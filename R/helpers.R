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

list2matrix <- function(x) {
  if (any(lengths(x))) { # cbind returns NULL for empty lists
    do.call(cbind, x)
  } else {
    matrix(numeric(0), ncol = 0, nrow = 0, dimnames = list(NULL, NULL))
  }
}

aggregate2pias <- function(x, w) {
  structure(list(child = x$pias$child,
                 parent = x$pias$parent,
                 levels = x$levels,
                 eas = x$pias$eas,
                 weights = structure(w, names = x$pias$eas),
                 height = x$pias$height),
            class = "pias")
}

empty_contrib <- function(x) {
  list(structure(rep(list(numeric(0)), length(x)), names = x))
}