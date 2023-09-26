merge.aggregate_piar_index <- function(x, y, ...) {
  x <- new_piar_index(x$index, x$contrib, x$levels, x$time,
                      is_chainable_index(x))
  NextMethod("merge")
}

merge.chainable_piar_index <- function(x, y, ...) {
  y <- as_index(y, chainable = TRUE)
  NextMethod("merge")
}

merge.direct_piar_index <- function(x, y, ...) {
  y <- as_index(y, chainable = FALSE)
  NextMethod("merge")
}

merge.piar_index <- function(x, y, ...) {
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
