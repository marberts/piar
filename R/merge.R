merge.aggregate_index <- function(x, y, ...) {
  x$r <- x$pias <- NULL
  class(x) <- class(x)[-1]
  NextMethod("merge")
}

merge.chainable_index <- function(x, y, ...) {
  if (!is_index(y)) {
    stop("'y' is not an index; use elemental_index() to make one")
  }
  if (!is_chainable_index(y)) {
    stop("cannot merge a fixed-base and period-over-period index")
  }
  NextMethod("merge")
}

merge.direct_index <- function(x, y, ...) {
  if (!is_index(y)) {
    stop("'y' is not an index; use elemental_index() to make one")
  }
  if (is_chainable_index(y)) {
    stop("cannot merge a fixed-base and period-over-period index")
  }
  NextMethod("merge")
}

merge.index <- function(x, y, ...) {
  if (!identical(x$time, y$time)) {
    stop("'x' and 'y' must be indexes for the same time periods")
  }
  if (any(x$levels %in% y$levels)) {
    stop("the same levels cannot appear in both 'x' and 'y'")
  }
  x$index <- Map(c, x$index, y$index)
  x$contrib <- Map(c, x$contrib, y$contrib)
  # it's safe to use c() and not union() because there can't be duplicate levels
  x$levels <- c(x$levels, y$levels)
  x
}
