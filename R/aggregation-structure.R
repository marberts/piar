#---- Make an aggregation structure ----
aggregation_structure <- function(x, w = NULL) {
  x <- lapply(x, as.character)
  len <- length(x)
  ea <- unlist(x[len], use.names = FALSE)
  if (length(ea) == 0L) {
    stop("cannot make an aggregation structure with no elemental aggregates")
  }
  if (any(vapply(x, anyNA, logical(1L)))) {
    stop("'x' cannot contain NAs")
  }

  w <- if (is.null(w)) {
    rep(1, length(ea))
  } else {
    as.numeric(w)
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
    warning("some nodes in the price index aggregation structure have ",
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
  # return 'pias' object
  res <- list(child = child,
              parent = parent,
              levels = c(nested_names(rev(child)), ea),
              eas = ea,
              weights = structure(w, names = ea),
              height = len)
  structure(res, class = "pias")
}

#---- Methods ----
weights.pias <- function(object, ea_only = FALSE, na.rm = FALSE, ...) {
  if (ea_only) {
    return(object$weights)
  }
  res <- vector("list", object$height)
  res[[1L]] <- object$weights
  # 'i' is defined in the loop below for looping over the height of pias
  add_weights <- function(z, i) {
    sum(res[[i - 1L]][z], na.rm = na.rm)
  }
  for (i in seq_along(res)[-1L]) {
    res[[i]] <- vapply(object$child[[i - 1L]], add_weights, numeric(1L), i)
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

`levels<-.pias` <- function(x, value) {
  stop("cannot replace levels attribute")
}

update.pias <- function(object, index, period = end(index), ...) {
  if (!is_aggregate_index(index)) {
    stop("'index' is not an aggregate index; use aggregate() to make one")
  }
  price_update <- factor_weights(index$r)
  if (!all(object$levels %in% index$levels)) {
    warning("not all weights in 'object' have a corresponding index value")
  }
  epr <- as.matrix(chain(index))[, period[1L]] # drop dimensions
  object$weights[] <- price_update(epr[object$eas], object$weights)
  object
}

as.matrix.pias <- function(x, ...) {
  nea <- length(x$eas)
  if (x$height == 1L) {
    return(matrix(numeric(0), ncol = nea, dimnames = list(NULL, x$eas)))
  }
  loc <- seq_len(nea)
  # don't need the eas
  lev <- lapply(pias2list(x)[-x$height], as.factor)
  rows <- vector("list", length(lev))
  # generate the rows for each level of the matrix and rbind together
  for (i in seq_along(rows)) {
    mat <- matrix(0, nrow = nlevels(lev[[i]]), ncol = nea,
                  dimnames = list(levels(lev[[i]]), x$eas))
    # splitting orders the rows of the matrix the same as the aggregation
    # structure
    cols <- split(loc, lev[[i]])
    w <- split(x$weights, lev[[i]])
    for (r in seq_len(nrow(mat))) {
      mat[r, cols[[r]]] <- scale_weights(w[[r]])
    }
    rows[[i]] <- mat
  }
  do.call(rbind, rows)
}

as.data.frame.pias <- function(x, ..., stringsAsFactors = FALSE) {
  colnames <- c(paste0("level", seq_along(x$child), recycle0 = TRUE), "ea")
  res <- as.data.frame(pias2list(x),
                       col.names = colnames,
                       stringsAsFactors = stringsAsFactors)
  res$weight <- x$weight
  res
}

#---- Expand classification ----
expand_classification <- function(class, width = 1L) {
  class <- as.character(class)
  width <- as.integer(width)
  if (any(width <= 0)) {
    stop("'width' must be strictly positive")
  }
  if (anyNA(width)) {
    stop("'width' cannot contain NAs")
  }

  if (length(width) == 1L) {
    longest <- max(nchar(class), 0L, na.rm = TRUE)
    width <- rep_len(width, ceiling(longest / width))
  }
  w <- cumsum(width)
  class <- strsplit(class, character(0L), fixed = TRUE)
  lapply(w, function(i) {
    vapply(class, paste_until, character(1L), i)
  })
}

#---- Test ----
# not exported
is_pias <- function(x) inherits(x, "pias")
