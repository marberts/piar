#---- Make an aggregation structure ----
aggregation_structure <- function(x, w) {
  x <- lapply(x, as.character)
  if (any(vapply(x, anyNA, logical(1)))) {
    stop(gettext("'x' cannot contain NAs"))
  }
  len <- length(x)
  ea <- if (len) x[[len]] else character(0) # x[[0]] is an error
  w <- if (missing(w)) rep(1, length(ea)) else as.numeric(w)
  # basic argument checking to make sure inputs can make an aggregation structure
  if (length(ea) != length(w) || any(lengths(x) != length(w))) {
    stop(gettext("all arguments must be the same length"))
  }
  if (anyDuplicated(ea)) {
    stop(gettext("there are duplicated elemental aggregates; the last vector in 'x' should not have duplicates"))
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
    child[[i]] <- lapply(child[[i]], match, table = nm[[i]])
  }
  # same for parent nodes
  for (i in seq_along(parent)) {
    parent[[i]] <- match(parent[[i]][nm[[i]]], nm[-1][[i]])
    names(parent[[i]]) <- nm[[i]]
  }
  # return 'pias' object
  res <- list(child = as.list(child), 
              parent = as.list(parent),
              levels = c(nested_names(rev(child)), ea),
              eas = ea,
              weights = structure(w, names = ea),
              height = len)
  structure(res, class = "pias")
}

#---- Methods ----
weights.pias <- function(object, ea_only = FALSE, na.rm = FALSE, ...) {
  if (ea_only) return(object$weights)
  if (!object$height) return(list())
  res <- vector("list", object$height)
  res[[1]] <- object$weights
  # 'i' is defined in the loop below for looping over the height of pias
  add_weights <- function(z) {
    sum(res[[i - 1]][z], na.rm = na.rm)
  }
  for (i in seq_along(res)[-1]) {
    res[[i]] <- vapply(object$child[[i - 1]], add_weights, numeric(1))
  }
  rev(res)
}

print.pias <- function(x, ...) {
  print(c(rev(lapply(x$child, names)), if (x$height) list(x$eas)))
  invisible(x)
}

levels.pias <- function(x) {
  x$levels
}

update.pias <- function(object, index, period = end(index), ...) {
  if (!is_aggregate_index(index)) {
    stop(gettext("'index' is not an aggregate index; use aggregate() to make one"))
  }
  price_update <- factor_weights(index$r)
  if (!all(object$levels %in% index$levels)) {
    warning(gettext("not all weights in 'object' have a corresponding index value"))
  }
  # an aggregate index can have levels but no periods; elementals should be NA
  epr <- if (length(period)) {
    as.matrix(chain(index))[, period]
  } else {
    NA_real_
  }
  object$weights[] <- price_update(epr[object$eas], object$weights)
  object
}

#---- Expand classification ----
expand_classification <- function(class, width) {
  class <- as.character(class)
  width <- if (missing(width)) {
    rep(1, max(nchar(class), 0, na.rm = TRUE))
  } else {
    as.numeric(width)
  }
  if (anyNA(width)) {
    stop(gettext("'width' cannot contain NAs"))
  }
  w <- cumsum(width)
  class <- strsplit(class, character(0), fixed = TRUE)
  lapply(w, function(i) {
    vapply(class, paste_until, character(1), i)
  })
}
