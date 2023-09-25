aggregation_structure <- function(x, w = NULL) {
  x <- lapply(x, as.character)
  len <- length(x)
  ea <- as.character(unlist(x[len], use.names = FALSE))
  if (length(ea) == 0L) {
    stop("cannot make an aggregation structure with no elemental aggregates")
  }
  if (any(vapply(x, anyNA, logical(1L)))) {
    stop("'x' cannot contain NAs")
  }

  if (is.null(w)) {
    w <- rep.int(1, length(ea))
  }

  # basic argument checking to make sure inputs can make an
  # aggregation structure
  if (any(lengths(x) != length(w))) {
    stop("all arguments must be the same length")
  }
  if (anyDuplicated(ea)) {
    stop("there are duplicated elemental aggregates; the last vector in 'x' ",
         "should not have duplicates")
  }
  if (anyDuplicated(unlist(lapply(x, unique), use.names = FALSE))) {
    stop("there are duplicated nodes in the aggregation structure; the same ",
         "value cannot appear across multiple levels of 'x'")
  }
  upper <- x[-len] # nodes above eas
  lower <- x[-1L] # nodes below initial nodes
  child <- parent <- vector("list", len)[-1L]
  # produce a list for each level with all the parent and child nodes
  for (i in seq_along(upper)) {
    child[[i]] <- lapply(split(lower[[len - i]], upper[[len - i]]), unique)
    parent[[i]] <- lapply(split(upper[[len - i]], lower[[len - i]]), unique)
  }
  if (any(lengths(unlist(parent, recursive = FALSE)) > 1L)) {
    stop("some nodes in the price index aggregation structure have ",
         "multiple parent nodes; the aggregation structure does not ",
         "represent a nested hierarchy")
  }
  parent <- lapply(parent, unlist)
  # positional matching for child nodes is much faster for aggregation
  nm <- c(list(ea), lapply(child, names))
  for (i in seq_along(child)) {
    child[[i]] <- lapply(child[[i]], match, table = nm[[i]])
  }
  # same for parent nodes
  for (i in seq_along(parent)) {
    parent[[i]] <- match(parent[[i]][nm[[i]]], nm[-1L][[i]])
    names(parent[[i]]) <- nm[[i]]
  }
  levels <- c(nested_names(rev(child)), ea)
  validate_pias(new_aggregation_structure(child, parent, levels, ea, w, len))
}
