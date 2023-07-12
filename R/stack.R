stack.aggregate_index <- function(x, y, ...) {
  if (is_aggregate_index(y)) {
    if (x$r != y$r) {
      stop("cannot stack indexes of different orders")
    }
    if (!identical(x$pias, y$pias)) {
      stop("'x' and 'y' must be generated from the same aggregation structure")
    }
  }
  NextMethod("stack")
}

stack.chainable_index <- function(x, y, ...) {
  if (!is_index(y)) {
    stop("'y' is not an index; use elemental_index() to make one")
  }
  if (!is_chainable_index(y)) {
    stop("cannot stack a period-over-period and a fixed-base index")
  }
  NextMethod("stack")
}

stack.direct_index <- function(x, y, ...) {
  if (!is_index(y)) {
    stop("'y' is not an index; use elemental_index() to make one")
  }
  if (is_chainable_index(y)) {
    stop("cannot stack a period-over-period and a fixed-base index")
  }
  NextMethod("stack")
}

stack.index <- function(x, y, ...) {
  if (!identical(x$levels, y$levels)) {
    stop("'x' and 'y' must be indexes for the same levels")
  }
  if (any(x$time %in% y$time)) {
    stop("the same time periods cannot appear in both 'x' and 'y'")
  }
  x$index <- c(x$index, y$index)
  x$contrib <- c(x$contrib, y$contrib)
  # it's safe to use c() and not union() because there can't be duplicate
  # periods
  x$time <- c(x$time, y$time)
  if (is_aggregate_index(y) && !is_aggregate_index(x)) {
    x$r <- y$r
    x$pias <- y$pias
    class(x) <- c("aggregate_index", class(x))
  }
  x
}

unstack.index <- function(x, ...) {
  res <- vector("list", length(x$time))
  names(res) <- x$time
  for (t in seq_along(res)) {
    res[[t]]$index <- x$index[t]
    res[[t]]$contrib <- x$contrib[t]
    res[[t]]$levels <- x$levels
    res[[t]]$time <- x$time[t]
    res[[t]]$r <- x$r
    res[[t]]$pias <- x$pias
    class(res[[t]]) <- class(x)
  }
  res
}
