as_aggregation_structure <- function(x, ...) {
  UseMethod("as_aggregation_structure")
}

as_aggregation_structure.default <- function(x, w = NULL, ...) {
  aggregation_structure(list(x), w)
}

as_aggregation_structure.agg_pindex <- function(x, w = NULL, ...) {
  eas <- x$pias$eas
  w <- if (is.null(w)) {
    rep.int(1, length(eas))
  } else {
    as.numeric(w)
  }
  names(w) <- eas
  pias(x$pias$child, x$pias$parent, x$levels, eas, w, x$pias$height)
}

as_aggregation_structure.data.frame <- function(x, ...) {
  x <- as.list(x)
  aggregation_structure(x[-length(x)], x[[length(x)]])
}

as_aggregation_structure.matrix <- function(x, ...) {
  aggregation_structure(as.data.frame(x))
}
