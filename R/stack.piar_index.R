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
#' @name stack.piar_index
#' @aliases stack.piar_index
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
stack.chainable_piar_index <- function(x, y, ...) {
  y <- as_index(y, chainable = TRUE)
  NextMethod("stack")
}

#' @rdname stack.piar_index
#' @export
stack.direct_piar_index <- function(x, y, ...) {
  y <- as_index(y, chainable = FALSE)
  NextMethod("stack")
}

#' @export
stack.piar_index <- function(x, y, ...) {
  chkDots(...)
  if (length(x$levels) != length(y$levels) || !setequal(x$levels, y$levels)) {
    stop("'x' and 'y' must be indexes for the same levels")
  }
  if (any(x$time %in% y$time)) {
    stop("the same time periods cannot appear in both 'x' and 'y'")
  }
  if (any(x$levels != y$levels)) {
    y <- y[x$levels]
  }
  x$index <- c(x$index, y$index)
  x$contrib <- c(x$contrib, y$contrib)
  x$time <- c(x$time, y$time)
  x
}

#' @rdname stack.piar_index
#' @importFrom utils unstack
#' @export
unstack.chainable_piar_index <- function(x, ...) {
  NextMethod("unstack", chainable = TRUE)
}

#' @rdname stack.piar_index
#' @export
unstack.direct_piar_index <- function(x, ...) {
  NextMethod("unstack", chainable = FALSE)
}

#' @export
unstack.piar_index <- function(x, ..., chainable) {
  res <- vector("list", length(x$time))
  names(res) <- x$time
  for (t in seq_along(res)) {
    res[[t]] <- new_piar_index(
      x$index[t], x$contrib[t], x$levels, x$time[t], chainable
    )
  }
  res
}
