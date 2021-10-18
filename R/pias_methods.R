weights.pias <- function(object, ea_only = FALSE, na.rm = FALSE, ...) {
  if (ea_only) {
    return(object$weights)
  }
  if (!object$height) {
    return(list())
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
  rev(res)
}

print.pias <- function(x, ...) {
  print(c(rev(lapply(x$child, names)), if (x$height) list(x$eas)))
  invisible(x)
}

levels.pias <- function(x) {
  x$levels
}