adjust <- function(x, ...) {
  UseMethod("adjust")
}

adjust.pias <- function(x, index, chained = TRUE, na.rm = FALSE, r = 1, maxit = 10, tol = 0.0001, ...) {
  if (!inherits(index, "aggregate")) {
    stop(gettext("'index' must be an 'aggregate' index object"))
  }
  w0 <- x$weights
  eas <- names(w0)
  dim <- c(length(eas), length(index$periods))
  for (i in seq_len(maxit)) {
    epr <- (if (chained) cumprod(index) else index)[eas, , drop = FALSE]
    x$weights <- x$weights / .rowMeans(epr, dim[1], dim[2])
    index <- aggregate(index, x, chained, na.rm, r)
    wpu <- .rowMeans(do.call(cbind, weights(index)), dim[1], dim[2])
    dist <- max(abs(wpu - w0), na.rm = TRUE)
    if (dist < tol) break
  }
  if (dist >= tol) {
    warning(gettext("weight adjustment did not converge"))
  }
  message(gettext("stopped after ", i, " iterations with a maximum absolute difference of ", dist))
  x
}