#' Stack price indexes
#'
#' @description
#' `stack()` combines two price indexes with common levels, stacking index
#' values and percent-change contributions for one index after the other.
#'
#' `unstack()` breaks up a price index into a list of indexes for each
#' time period.
#'
#' These methods can be used in a map-reduce to make an index with multiple
#' aggregation structures (like a Paasche index).
#'
#' @inheritParams merge.piar_index
#'
#' @returns
#' `stack()` returns a combined price index that inherits from the same class
#' as `x`.
#'
#' `unstack()` returns a list of price indexes with the same class as `x`.
#'
#' @note
#' It may be necessary to use `rebase()` prior to stacking fixed-based price
#' indexes to ensure they have the same base period.
#'
#' @examples
#' index1 <- as_index(matrix(1:6, 2))
#'
#' index2 <- index1
#' time(index2) <- 4:6
#'
#' stack(index1, index2)
#'
#' # Unstack does the reverse
#'
#' all.equal(
#'   c(unstack(index1), unstack(index2)),
#'   unstack(stack(index1, index2))
#' )
#'
#' @family index methods
#' @importFrom utils stack
#' @export
stack.piar_index <- function(x, y, ...) {
  if (any(x$levels != y$levels)) {
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

#' @export
stack.chainable_piar_index <- function(x, y, ...) {
  y <- as_index(y, chainable = TRUE)
  NextMethod("stack")
}

#' @export
stack.direct_piar_index <- function(x, y, ...) {
  y <- as_index(y, chainable = FALSE)
  NextMethod("stack")
}

#' @rdname stack.piar_index
#' @importFrom utils unstack
#' @export
unstack.piar_index <- function(x, ...) {
  res <- vector("list", length(x$time))
  names(res) <- x$time
  for (t in seq_along(res)) {
    res[[t]]$index <- x$index[t]
    res[[t]]$contrib <- x$contrib[t]
    res[[t]]$levels <- x$levels
    res[[t]]$time <- x$time[t]
    class(res[[t]]) <- class(x)
  }
  res
}
