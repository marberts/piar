mean.index <- function(x, w = NULL, window = 3, na.rm = FALSE, r = 1, ...) {
  if (!is.null(w)) {
    if (length(w) != length(x$time) * length(x$levels)) {
      stop("'x' and 'w' must be the same length")
    }
    w <- split(as.numeric(w), gl(length(x$time), length(x$levels)))
  }
  gen_mean <- Vectorize(generalized_mean(r))
  len <- length(x$time) %/% window
  if (len == 0L) {
    stop("'x' must have at least 'window' time periods")
  }
  # get the starting location for each window
  loc <- seq.int(1L, by = window, length.out = len)
  periods <- x$time[loc]
  res <- index_skeleton(x$levels, periods)
  # loop over each window and calculate the mean for each level
  for (i in seq_along(loc)) {
    j <- seq(loc[i], length.out = window)
    index <- .mapply(c, x$index[j], list())
    weight <- if (is.null(w)) list(NULL) else .mapply(c, w[j], list())
    res[[i]][] <- gen_mean(index, weight, na.rm = na.rm)
  }
  x$index <- res
  x$contrib <- contrib_skeleton(x$levels, periods)
  x$time <- periods
  validate_index(x)
}
