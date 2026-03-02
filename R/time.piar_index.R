#' Get the time periods for a price index
#'
#' Methods to get and set the time periods for a price index.
#'
#' @param x A price index, as made by, e.g., [elementary_index()].
#' @param value A character vector, or something that can be coerced into one,
#'   giving the replacement time periods for `x`.
#' @param ... Not currently used.
#'
#' @returns
#' `time()` returns a character vector with the time periods for a price index.
#' `start()` and `end()` return the first and last time period.
#'
#' `ntime()` returns the number of time periods, analogous to `nlevels()`.
#'
#' The replacement method returns a copy of `x` with the time periods in
#' `value`. (`set_time()` is an alias that's easier to use with pipes.)
#'
#' @importFrom stats time
#' @family index methods
#' @export
time.piar_index <- function(x, ...) {
  chkDots(...)
  x$time
}

#' @rdname time.piar_index
#' @export
`time<-` <- function(x, value) {
  UseMethod("time<-")
}

#' @rdname time.piar_index
#' @export
`time<-.piar_index` <- function(x, value) {
  x$time <- as.character(value)
  validate_time(x)
}

#' @rdname time.piar_index
#' @export
set_time <- `time<-`

#' @rdname time.piar_index
#' @importFrom stats start
#' @export
start.piar_index <- function(x, ...) {
  chkDots(...)
  x$time[1L]
}

#' @rdname time.piar_index
#' @importFrom stats end
#' @export
end.piar_index <- function(x, ...) {
  chkDots(...)
  last(x$time)
}

#' @rdname time.piar_index
#' @export
ntime <- function(x) {
  length(time(x))
}
