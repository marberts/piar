adjust_weights <- function(x, index, chained = TRUE, na.rm = FALSE, r = 1, maxit = 10, tol = 1e-6) {
  if (!inherits(x, "pias")) {
    stop(gettext("'x' must be a price index aggregation structure; use aggregation_structure() to make one"))
  }
  if (!inherits(index, "aggregate")) {
    stop(gettext("'index' must be an aggregate index; use aggregate.index() to make one"))
  }
  w0 <- x$weights
  eas <- names(w0)
  dim <- c(length(eas), length(index$periods))
  for (i in seq_len(maxit)) {
    epr <- (if (chained) cumprod(index) else index)[eas, , drop = FALSE]
    x$weights <- x$weights / .rowMeans(epr, dim[1], dim[2])
    index <- aggregate(index, x, chained, na.rm, r)
    wpu <- .rowMeans(weights(index), dim[1], dim[2])
    dist <- max(abs(wpu - w0), na.rm = TRUE)
    if (dist < tol) break
  }
  if (dist >= tol) {
    warning(gettext("weight adjustment did not converge"))
  }
  message(gettext("stopped after ", i, " iterations with a maximum absolute difference of ", dist))
  x
}