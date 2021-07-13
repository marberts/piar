aggregation_structure <- function(x, w = rep(1, length(ea))) {
  x <- lapply(x, as.character)
  len <- length(x)
  ea <- if (len) x[[len]] # x[[0]] is an error
  w <- as.numeric(w)
  # basic argument checking to make sure inputs can make an aggregation structure
  if (any(lengths(x) != length(w))) {
    stop(gettext("each vector in 'x' must be the same length as 'w'"))
  }
  if (anyDuplicated(ea)) {
    stop(gettext("there are duplicated elementary aggregates; the last vector in 'x' should not have duplicates"))
  }
  if (anyDuplicated(unlist(lapply(x, unique), use.names = FALSE))) {
    stop(gettext("there are duplicated nodes in the aggregation structure; the same value cannot appear across multiple levels of 'x'"))
  }
  upper <- x[-len] # nodes above eas
  lower <- x[-1] # nodes below initial nodes
  child <- parent <- vector("list", len)[-1]
  # produce a list for each level with all the parent and child nodes
  for (i in seq_along(upper)) {
    child[[i]] <- lapply(split(lower[[len - i]], upper[[len - i]]), unique)
    parent[[i]] <- lapply(split(upper[[len - i]], lower[[len - i]]), unique)
  }
  if (any(lengths(unlist(parent, recursive = FALSE)) > 1)) {
    warning(gettext("some nodes in the price index aggregation structure have multiple parent nodes; the aggregation structure does not represent a nested hierarchy"))
  }
  parent <- lapply(parent, unlist)
  # positional matching for child nodes is much faster for aggregation
  nm <- c(list(ea), lapply(child, names))
  for (i in seq_along(child)) {
    child[[i]] <- lapply(child[[i]], match, table = nm[[i]], incomparables = NA)
  }
  # same for parent nodes
  for (i in seq_along(parent)) {
    parent[[i]] <- match(parent[[i]][nm[[i]]], nm[-1][[i]], incomparables = NA)
    names(parent[[i]]) <- nm[[i]]
  }
  # return 'pias' object
  res <- list(child = as.list(child), 
              parent = as.list(parent),
              levels = as.character(c(names(unlist(rev(child), recursive = FALSE)), ea)),
              weights = structure(w, names = ea),
              height = len)
  structure(res, class = "pias")
}
