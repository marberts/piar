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
    stop(gettext("'index' is not an aggregate index; use aggregate() to make one"))
  }
  if (!all(object$levels %in% index$levels)) {
    warning(gettext("not all weights in pias have a corresponding index value"))
  }
  w <- index$weights
  if (length(w)) object$weights[] <- w[[length(w)]][names(object$weights)]
  object
}
