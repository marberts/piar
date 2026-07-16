#' Update weights
#'
#' Factor weights to turn the generalized mean of a product into the product
#' of generalized means. Useful for price-updating the weights in a
#' generalized-mean index.
#'
#' This function generalizes the result in section C.5 of Chapter 9 of the PPI
#' Manual for chaining the Young index, and gives a way to chain
#' generalized-mean price indexes over time. It returns a value such that
#'
#' \preformatted{gmean(x * y, w) ==
#'     gmean(x, w) * gmean(y, update_weights(x, w))}
#'
#' Factoring weights returns a value that is the same length as `x`,
#' so any missing values in `x` or `weights` will return `NA`.
#' Unless all values are `NA`, however, the result will still satisfy
#' the above identity when `na.rm = TRUE`.
#'
#' @family math functions
#' @export
#'
#' @inheritParams gmean
#'
#' @returns
#' A numeric vector the same length as `x`.
#'
#' @references
#' ILO, IMF, OECD, UNECE, and World Bank. (2004).
#' *Producer Price Index Manual: Theory and Practice*.
#' International Monetary Fund.
#'
#' @examples
#' x <- 1:3
#' y <- 4:6
#' w <- 3:1
#'
#' # Factor the arithmetic mean by chaining the calculation.
#' gmean(x * y, w)
#' gmean(x, w) * gmean(y, update_weights(x, w))
#'
#' # In cases where x and y have the same order, Chebyshev's
#' # inequality implies that the chained calculation is too small.
#' gmean(x * y, w) > gmean(x, w) * gmean(y, w)
update_weights <- function(x, weights = NULL, order = 1) {
  if (not_finite_scalar(order)) {
    stop("`order` must be a finite number")
  }
  if (!is.null(weights) && length(x) != length(weights)) {
    stop("`x` and `weights` must be the same length")
  }
  if (order == 0) {
    if (is.null(weights)) {
      weights <- rep.int(1, length(x))
    }
    if (anyNA(x)) {
      weights[is.na(x)] <- NA_real_
    }
    weights
  } else {
    if (is.null(weights)) x^order else weights * x^order
  }
}
