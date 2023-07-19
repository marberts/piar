as_aggregation_structure <- function(x, ...) {
  UseMethod("as_aggregation_structure")
}

as_aggregation_structure.default <- function(x, w = NULL, ...) {
  aggregation_structure(as.list(x), w)
}

as_aggregation_structure.aggregate_index <- function(x, w = NULL, ...) {
  eas <- x$pias$eas
  if (is.null(w)) {
    w <- rep.int(1, length(eas))
  } else {
    w <- as.numeric(w)
  }
  names(w) <- eas
  new_aggregation_structure(x$pias$child,
                            x$pias$parent,
                            x$levels,
                            eas,
                            w,
                            x$pias$height)
}

as_aggregation_structure.data.frame <- function(x, ...) {
  x <- as.list(x)
  aggregation_structure(x[-length(x)], x[[length(x)]])
}

as_aggregation_structure.matrix <- function(x, ...) {
  as_aggregation_structure(as.data.frame(x), ...)
}
