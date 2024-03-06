#' Get the levels for a price index
#'
#' Methods to get and set the levels for a price index.
#'
#' @param x A price index, as made by, e.g., [elemental_index()].
#' @param value A character vector, or something that can be coerced into one,
#' giving the replacement levels for `x`.
#'
#' @returns
#' `levels()` returns a character vector with the levels for a price index.
#'
#' The replacement method returns a copy of `x` with the levels in `value`.
#'
#' It's not generally possible to change the levels of an aggregate price
#' index, and in this case replacing the levels does not return an aggregate
#' index.
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

#' @export
`levels<-.aggregate_piar_index` <- function(x, value) {
  value <- as.character(value)
  if (identical(value, x$levels)) {
    x
  } else {
    piar_index(x$index, x$contrib, value, x$time, is_chainable_index(x))
  }
}
