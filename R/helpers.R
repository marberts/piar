# None of these functions are exported

as_TorF <- function(x) {
  x <- as.logical(x)[1L]
  if (is.na(x)) {
    stop("cannot coerce NA to TRUE or FALSE")
  }
  x
}

different_length <- function(...) {
  res <- lengths(list(...))
  any(res != res[1L])
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

aggregate2pias <- function(x, w) {
  structure(list(child = x$pias$child,
                 parent = x$pias$parent,
                 levels = x$levels,
                 eas = x$pias$eas,
                 weights = structure(w, names = x$pias$eas),
                 height = x$pias$height),
            class = "pias")
}

pias2list <- function(x) {
  if (x$height == 1L) return(list(x$eas))
  res <- vector("list", length(x$parent))
  res[[1L]] <- x$parent[[1L]]
  # walk up the parent nodes to reconstruct the inputs that generated 'x'
  for (i in seq_along(x$parent)[-1L]) {
    res[[i]] <- x$parent[[i]][res[[i - 1L]]]
  }
  top <- names(x$child[[length(x$child)]])[res[[length(res)]]]
  c(list(top), lapply(rev(res), names))
}

empty_contrib <- function(x) {
  list(structure(rep.int(list(numeric(0L)), length(x)), names = x))
}

has_contrib <- function(x) {
  Position(\(x) any(lengths(x) > 0L), x$contrib, nomatch = 0L) > 0L
}
