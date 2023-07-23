weights.aggregation_structure <- function(object,
                                          ea_only = FALSE,
                                          na.rm = FALSE, ...) {
  if (ea_only) {
    return(object$weights)
  }
  res <- vector("list", object$height)
  res[[1L]] <- object$weights

  for (i in seq_along(res)[-1L]) {
    res[[i]] <- vapply(object$child[[i - 1L]],
                       \(z) sum(res[[i - 1L]][z], na.rm = na.rm),
                       numeric(1L))
  }
  rev(res)
}

`weights<-` <- function(object, value) {
  UseMethod("weights<-")
}

`weights<-.aggregation_structure` <- function(object, value) {
  object$weights[] <- as.numeric(value)
  object
}

print.aggregation_structure <- function(x, ...) {
  print(c(rev(lapply(x$child, names)), if (x$height > 0L) list(x$eas)))
  invisible(x)
}

levels.aggregation_structure <- function(x) {
  x$levels
}

`levels<-.aggregation_structure` <- function(x, value) {
  stop("cannot replace levels attribute")
}

update.aggregation_structure <- function(object,
                                         index,
                                         period = end(index),
                                         ...) {
  if (!is_aggregate_index(index)) {
    stop("'index' is not an aggregate index; use aggregate() to make one")
  }
  price_update <- factor_weights(index$r)
  if (!all(object$levels %in% index$levels)) {
    warning("not all weights in 'object' have a corresponding index value")
  }
  epr <- chain(index)$index[[period]]
  weights(object) <- price_update(epr[object$eas], object$weights)
  object
}

as.matrix.aggregation_structure <- function(x, ...) {
  nea <- length(x$eas)
  if (x$height == 1L) {
    return(matrix(numeric(0), ncol = nea, dimnames = list(NULL, x$eas)))
  }
  loc <- seq_len(nea)
  # don't need the eas
  lev <- lapply(as.list(x)[-x$height], as.factor)
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

as.data.frame.aggregation_structure <- function(x, ...,
                                                stringsAsFactors = FALSE) {
  colnames <- c(paste0("level", seq_along(x$child), recycle0 = TRUE), "ea")
  res <- as.data.frame(as.list(x),
                       col.names = colnames,
                       stringsAsFactors = stringsAsFactors)
  res$weight <- x$weight
  res
}

as.list.aggregation_structure <- function(x, ...) {
  if (x$height == 1L) {
    return(list(x$eas))
  }
  res <- vector("list", length(x$parent))
  res[[1L]] <- x$parent[[1L]]
  # walk up the parent nodes to reconstruct the inputs that generated 'x'
  for (i in seq_along(x$parent)[-1L]) {
    res[[i]] <- x$parent[[i]][res[[i - 1L]]]
  }
  top <- names(x$child[[length(x$child)]])[res[[length(res)]]]
  c(list(top), lapply(rev(res), names))
}

# not exported
is_aggregation_structure <- function(x) {
  inherits(x, "aggregation_structure")
}
