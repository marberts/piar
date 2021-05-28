#---- Aggregation structure ----
aggregation_structure <- function(x, w = rep(1, length(ea))) {
  x <- lapply(x, as.character)
  len <- length(x)
  ea <- if (len) x[[len]] else character(0) # x[[0]] is an error
  w <- as.numeric(w)
  # basic argument checking to make sure inputs can make an aggregation structure
  if (any(lengths(x) != length(w))) {
    stop("Each vector in 'x' must be the same length as 'w'")
  }
  if (anyDuplicated(ea)) {
    stop("There are duplicated elementary aggregates; the last vector in 'x' should not have duplicates")
  }
  if (anyDuplicated(unlist(lapply(x, unique), use.names = FALSE))) {
    stop("There are duplicated nodes in the aggregation structure; the same value cannot appear across multiple levels of 'x'")
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
    warning("some nodes in the price index aggregation structure have multiple parent nodes; the aggregation structure does not represent a nested hierachy")
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

#---- Methods ----
weights.pias <- function(object, ea_only = FALSE, na.rm = FALSE, ...) {
  if (ea_only) return(object$weights)
  if (!object$height) return(as.list(object$weights))
  res <- vector("list", object$height)
  res[[1]] <- object$weights
  # 'i' is defined in the loop below
  add_weights <- function(z) {
    sum(res[[i - 1]][z], na.rm = na.rm)
  }
  for (i in seq_along(res)[-1]) {
    res[[i]] <- vapply(object$child[[i - 1]], add_weights, numeric(1))
  }
  res
}

print.pias <- function(x, ...) {
  cat("Price index aggregation structure with", 
      length(x$weights), 
      "elementary",
      if (length(x$weights) == 1) "aggregate" else "aggregates", 
      "and", 
      length(x$child), 
      "upper-level", if (length(x$child) == 1) "index" else "indexes",
      "\n")
  print(c(rev(lapply(x$child, names)), list(names(x$weights))))
  invisible(x)
}

update.pias <- function(object, index, ...) {
  if (!inherits(index, "aggregate")) {
    stop("'index' is not an aggregate index; use aggregate() to make one")
  }
  if (!all(object$levels %in% index$levels)) {
    warning("not all weights in pias have a corresponding index value")
  }
  w <- index$weights
  object$weights[] <- w[[length(w)]][names(object$weights)]
  object
}
