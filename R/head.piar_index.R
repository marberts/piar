#' Return the first/last parts of an index
#'
#' Extract the first/last parts of an index as if it were a matrix.
#'
#' @param x A price index, as made by, e.g., [elementary_index()].
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
# Default arguments preceed ... to agree with default method in utils.
head.piar_index <- function(x, n = 6L, ...) {
  chkDots(...)
  x$index <- head(x$index, n)
  x$contrib <- head(x$contrib, n)
  if (!is.na(n[1L])) {
    x$levels <- head(x$levels, n[1L])
  }
  if (!is.na(n[2L])) {
    x$time <- head(x$time, n[2L])
  }
  validate_piar_index(x)
}

#' @rdname head.piar_index
#' @importFrom utils tail
#' @export
tail.piar_index <- function(x, n = 6L, ...) {
  chkDots(...)
  x$index <- tail(x$index, n, keepnums = FALSE)
  x$contrib <- tail(x$contrib, n, keepnums = FALSE)
  if (!is.na(n[1L])) {
    x$levels <- tail(x$levels, n[1L])
  }
  if (!is.na(n[2L])) {
    x$time <- tail(x$time, n[2L])
  }
  validate_piar_index(x)
}
