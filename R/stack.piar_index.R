stack.aggregate_piar_index <- function(x, y, ...) {
  if (is_aggregate_index(y)) {
    if (x$r != y$r) {
      stop("cannot stack indexes of different orders")
    }
    if (!identical(x$pias, y$pias)) {
      stop("'x' and 'y' must be generated from the same aggregation structure")
    }
  } else {
    x <- new_piar_index(x$index, x$contrib, x$levels, x$time,
                        is_chainable_index(x))
  }
  NextMethod("stack")
}

stack.chainable_piar_index <- function(x, y, ...) {
  y <- as_index(y, chainable = TRUE)
  NextMethod("stack")
}

stack.direct_piar_index <- function(x, y, ...) {
  y <- as_index(y, chainable = FALSE)
  NextMethod("stack")
}



#' Stack price indexes
#' 
#' `stack()` combines two price indexes with common levels, stacking index
#' values and percent-change contributions for one index after the other.
#' 
#' `unstack()` breaks up a price index into a list of indexes for each
#' time period.
#' 
#' These methods can be used in a map-reduce to make an index with multiple
#' aggregation structures (like a Paasche index).
#' 
#' 
#' @aliases stack.aggregate_piar_index stack.chainable_piar_index
#' stack.direct_piar_index stack.piar_index unstack.piar_index
#' @param x A price index, as made by, e.g.,
#' [`elemental_index()`][elemental_index].
#' @param y A price index, or something that can coerced into one. If `x`
#' is a period-over-period index then `y` is coerced into a chainable
#' index; otherwise, `y` is coerced into a direct index.
#' @param ... Further arguments passed to or used by methods.
#' @return A price index that inherits from [chainable_piar_index()]
#' if `x` is a period-over-period index, or
#' [direct_piar_index()] if `x` is a fixed-base index. If both
#' `x` and `y` are aggregate indexes then the result will also
#' inherit from [`aggregate_piar_index()`][piar_index].
#' 
#' `unstack()` returns a list of price indexes with the same class as
#' `x`.
#' @seealso [`merge()`][merge.piar_index] to combine indexes for
#' different levels over the same time periods.
#' @examples
#' 
#' prices <- data.frame(
#'   rel = 1:8,
#'   period = rep(1:2, each = 4),
#'   ea = rep(letters[1:2], 4)
#' )
#' 
#' prices2 <- data.frame(
#'   rel = 1:8,
#'   period = rep(3:4, each = 4),
#'   ea = rep(letters[1:2], 4)
#' )
#' 
#' epr <- with(prices, elemental_index(rel, period, ea))
#' 
#' epr2 <- with(prices2, elemental_index(rel, period, ea))
#' 
#' stack(epr, epr2)
#' 
#' # Unstack does the reverse
#' 
#' all.equal(c(unstack(epr), unstack(epr2)),
#'           unstack(stack(epr, epr2)))
#' 
stack.piar_index <- function(x, y, ...) {
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
  validate_piar_index(x)
}

unstack.piar_index <- function(x, ...) {
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
