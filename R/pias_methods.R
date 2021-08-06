weights.pias <- function(object, ea_only = FALSE, na.rm = FALSE, ...) {
  if (ea_only) {
    return(object$weights)
  }
  if (!object$height) {
    return(as.list(object$weights))
  }
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
  print(c(rev(lapply(x$child, names)), list(names(x$weights))))
  invisible(x)
}

update.pias <- function(object, index, period = rev(index$periods)[1], ...) {
  if (!inherits(index, "aggregate")) {
    stop(gettext("'index' is not an aggregate index; use aggregate.index() to make one"))
  }
  if (!all(object$levels %in% index$levels)) {
    warning(gettext("not all weights in the aggregation structure have a corresponding index value"))
  }
  w <- index$weights
  if (length(w)) {
    object$weights[] <- w[[period]][names(object$weights)]
  }
  object
}
