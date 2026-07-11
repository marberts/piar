transmute_weights <- function(x, weights = NULL, r = 0, to = 1, mean = NA) {
  if (!is.finite(r)) {
    stop("`r` must be a finite number")
  }
  if (!is.finite(to)) {
    stop("`to` must be a finite number")
  }
  if (!is.null(weights) && length(x) != length(weights)) {
    stop("`x` and `weights` must be the same length")
  }
  if (is.na(mean)) {
    mean <- gmean(x, weights, r, na.rm = TRUE)
  }
  .transmute_weights(x, weights, r, to, mean)
}

#' Scale weights
#'
#' Scale a vector of weights so that they sum to 1.
#'
#' @inheritParams gmean
#'
#' @returns
#' A numeric vector that sums to 1. If there are `NA`s in `x` then the result
#' sums 1 to if these values are removed.
#'
#' @examples
#' scale_weights(1:5)
#'
#' scale_weights(c(1:5, NA))
#'
#' @family math functions
#' @export
scale_weights <- function(x) {
  x / sum(x, na.rm = TRUE)
}

#' Transmute weights
#' @noRd
.transmute_weights <- function(x, weights, from, to, mean) {
  if (from == to) {
    if (is.null(weights)) {
      weights <- rep.int(1, length(x))
    }
    if (anyNA(x)) {
      weights[is.na(x)] <- NA_real_
    }
  } else {
    ext_mean <- .extended_mean_pow(x, mean, from, to)
    weights <- if (is.null(weights)) ext_mean else weights * ext_mean
  }
  scale_weights(weights)
}

#' Simplified extended mean for transmuting weights
#' @noRd
.extended_mean_pow <- function(x, m, r, s, tol = .Machine$double.eps^0.5) {
  rdiff <- function(a, b, r) {
    if (r == 0) {
      log(a / b)
    } else if (r == 1) {
      a - b
    } else {
      (a^r - b^r) / r
    }
  }
  res <- rdiff(x, m, r) / rdiff(x, m, s)
  res[abs(x - m) <= tol] <- m^(r - s)
  res
}
