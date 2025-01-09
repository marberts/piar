#' Get the levels for a price index
#'
#' Methods to get and set the levels for a price index.
#'
#' @param x A price index, as made by, e.g., [elemental_index()].
#' @param value A character vector, or something that can be coerced into one,
#'   giving the replacement levels for `x`.
#'
#' @returns
#' `levels()` returns a character vector with the levels for a price index.
#'
#' The replacement method returns a copy of `x` with the levels in `value`.
#' (`set_levels()` is an alias that's easier to use with pipes.)
#'
#' @family index methods
#' @export
levels.piar_index <- function(x) {
  x$levels
}

#' @rdname levels.piar_index
#' @export
`levels<-.piar_index` <- function(x, value) {
  x$levels <- as.character(value)
  validate_piar_index(x)
}

#' @rdname levels.piar_index
#' @export
set_levels <- function(x, value) {
  levels(x) <- value
  x
}
