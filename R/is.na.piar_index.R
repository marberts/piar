#' Missing values in a price index
#'
#' Identify missing values in a price index.
#'
#' @param x A price index, as made by, e.g., [elementary_index()].
#' @param recursive Check if `x` also has missing percent-change
#'   contributions. By default only index values are checked for missingness.
#'
#' @returns
#' `is.na()` returns a logical matrix, with a row for each level of `x` and a
#' columns for each time period, that indicates which index values are missing.
#'
#' `anyNA()` returns `TRUE` if any index values are missing, or percent-change
#' contributions (if `recursive = TRUE`).
#'
#' @examples
#' index <- as_index(matrix(c(1, 2, 3, NA, 5, NA), 2))
#'
#' anyNA(index)
#' is.na(index)
#'
#' # Carry forward imputation
#'
#' index[is.na(index)] <- 1
#' index
#'
#' @family index methods
#' @export
is.na.piar_index <- function(x) {
  is.na(as.matrix(x))
}

#' @rdname is.na.piar_index
#' @export
anyNA.piar_index <- function(x, recursive = FALSE) {
  res <- anyNA(x$index)
  if (recursive && !res) {
    anyNA(x$contrib, recursive = TRUE)
  } else {
    res
  }
}
