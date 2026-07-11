#' Generalized mean
#'
#' Calculated a weighted generalized mean
#'
#' @param x A strictly positive numeric vector.
#' @param weights A strictly positive numeric vector of weights, the same length
#'   as `x`. The default is to equally weight each element of `x`.
#' @param r A finite number giving the order of the generalized mean.
#'
#' @returns A number.
#'
#' @export
gmean <- function(x, weights = NULL, r = 1, na.rm = FALSE) {
  if (!is.finite(r)) {
    stop("`r` must be a finite number")
  }
  if (is.null(weights)) {
    if (na.rm && anyNA(x)) {
      x <- x[!is.na(x)]
    }
    if (r == 0) {
      exp(sum(log(x)) / length(x))
    } else if (r == 1) {
      # The arithmetic case is important enough for the optimization.
      sum(x) / length(x)
    } else if (r == -1) {
      # Same with the harmonic.
      length(x) / sum(1 / x)
    } else {
      (sum(x^r) / length(x))^(1 / r)
    }
  } else {
    if (length(x) != length(weights)) {
      stop("`x` and `weights` must be the same length")
    }
    if (na.rm && (anyNA(x) || anyNA(weights))) {
      keep <- !(is.na(x) | is.na(weights))
      x <- x[keep]
      weights <- weights[keep]
    }
    if (r == 0) {
      exp(sum(log(x) * weights) / sum(weights))
    } else if (r == 1) {
      sum(x * weights) / sum(weights)
    } else if (r == -1) {
      sum(weights) / sum(weights / x)
    } else {
      (sum(x^r * weights) / sum(weights))^(1 / r)
    }
  }
}
