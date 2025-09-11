#' Index window
#'
#' Extract and replace index values over a window of time periods.
#'
#' @param x A price index, as made by, e.g., [elementary_index()].
#' @param start The time period to start the window. The default in the first
#'   period of `x`.
#' @param end The time period to end the window. The default is the last period
#'   of `x`.
#' @param ... Not currently used.
#' @param value A numeric vector or price index.
#'
#' @returns
#' `window()` extracts a price index over a window of time periods that
#' inherits from the same class as `x`. The replacement method replaces these
#' with `value`.
#'
#' @examples
#' x <- as_index(matrix(1:9, 3))
#'
#' window(x, "2")
#'
#' window(x, "2") <- 1
#' x
#'
#' @family index methods
#' @importFrom stats window
#' @export
window.piar_index <- function(x, start = NULL, end = NULL, ...) {
  chkDots(...)
  x[, index_window(x, start, end)]
}

#' @rdname window.piar_index
#' @importFrom stats window<-
#' @export
`window<-.piar_index` <- function(x, start = NULL, end = NULL, ..., value) {
  chkDots(...)
  x[, index_window(x, start, end)] <- value
  x
}

#' Get the indexes for a window of time periods
#' @noRd
index_window <- function(x, start, end) {
  if (is.null(start)) {
    start <- start(x)
  }
  if (is.null(end)) {
    end <- end(x)
  }
  start <- match_time(as.character(start), x)
  end <- match_time(as.character(end), x)

  if (start > end) {
    stop("'start' must refer to a time period before 'end'")
  }
  seq.int(start, end)
}
