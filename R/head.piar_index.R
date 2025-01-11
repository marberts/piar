#' Return the first/last parts of an index
#'
#' Extract the first/last parts of an index as if it were a matrix.
#'
#' @param x A price index, as made by, e.g., [elemental_index()].
#' @param n See [head()]/[tail()]. The default takes the
#'   first/last 6 levels of `x`.
#' @param ... Not currently used.
#'
#' @returns
#' A price index that inherits from the same class as `x`.
#'
#' @examples
#' index <- as_index(matrix(1:9, 3))
#'
#' head(index, 1)
#'
#' tail(index, 1)
#'
#' @family index methods
#' @importFrom utils head
#' @export
head.piar_index <- function(x, n = 6L, ...) {
  chkDots(...)
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
#' @importFrom utils tail
#' @export
tail.piar_index <- function(x, n = 6L, ...) {
  chkDots(...)
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
