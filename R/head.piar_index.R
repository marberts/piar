#' Return the first/last parts of an index
#'
#' Extract the first/last parts of an index as if it were a matrix.
#'
#' @param x A price index, as made by, e.g., [elemental_index()].
#' @param n See [head()]/[tail()]. The default takes the
#' first/last 6 levels of `x`.
#' @param ... Further arguments passed to or used by methods.
#'
#' @returns
#' A price index that inherits from [`chainable_piar_index`]
#' if `x` is a period-over-period index, or
#' `direct_piar_index()` if `x` is a fixed-base index.
#'
#' @examples
#' prices <- data.frame(
#'   rel = 1:8,
#'   period = rep(1:2, each = 4),
#'   ea = rep(letters[1:2], 4)
#' )
#'
#' # Calculate Jevons elemental indexes
#'
#' epr <- with(prices, elemental_index(rel, period, ea))
#'
#' # Get the first/last time series
#'
#' head(epr, 1)
#'
#' tail(epr, 1)
#'
#' @family index methods
#' @export
head.piar_index <- function(x, n = 6L, ...) {
  nl <- levels <- length(x$levels)
  np <- periods <- length(x$time)
  if (!is.na(n[1L])) {
    if (n[1L] < 0L) {
      nl <- max(levels + n[1L], 0L)
    } else {
      nl <- min(n[1L], levels)
    }
  }
  if (!is.na(n[2L])) {
    if (n[2L] < 0L) {
      np <- max(periods + n[2L], 0L)
    } else {
      np <- min(n[2L], periods)
    }
  }
  x[seq_len(nl), seq_len(np)]
}

#' @rdname head.piar_index
#' @export
tail.piar_index <- function(x, n = 6L, ...) {
  nl <- levels <- length(x$levels)
  np <- periods <- length(x$time)
  if (!is.na(n[1L])) {
    if (n[1L] < 0L) {
      nl <- max(levels + n[1L], 0L)
    } else {
      nl <- min(n[1L], levels)
    }
  }
  if (!is.na(n[2L])) {
    if (n[2L] < 0L) {
      np <- max(periods + n[2L], 0L)
    } else {
      np <- min(n[2L], periods)
    }
  }
  i <- seq.int(to = levels, length.out = nl)
  j <- seq.int(to = periods, length.out = np)
  x[i, j]
}
