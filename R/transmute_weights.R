#' Transmute weights for a generalized mean
#'
#' Transmute weight to turn a generalized mean of a given order into a
#' generalized mean of order `to`. Useful for calculating additive and
#' multiplicative decompositions for generalized-mean indexes.
#'
#' @inheritParams gmean
#' @param to A finite number giving the order of the target generalized mean.
#' @param mean A finite number giving the generalized mean of `x` and `weights`,
#'   if known. The default computes this values.
#'
#' @returns
#' A numeric vector, the same length as `x`, that sums to 1.
#'
#' @references
#' See `vignette("decomposing-indexes")` for more details.
#'
#' @examples
#' x <- 1:3
#' w <- 3:1
#'
#' # Calculate the geometric mean as an arithmetic mean.
#' gmean(x, order = 0)
#' gmean(x, transmute_weights(x, order = 0, to = 1), order = 1)
#' @family math functions
#' @export
transmute_weights <- function(x, weights = NULL, order = 0, to = 1, mean = NA) {
  if (!is.finite(order)) {
    stop("`order` must be a finite number")
  }
  if (!is.finite(to)) {
    stop("`to` must be a finite number")
  }
  if (!is.null(weights) && length(x) != length(weights)) {
    stop("`x` and `weights` must be the same length")
  }
  if (is.na(mean)) {
    mean <- gmean(x, weights, order, na.rm = TRUE)
  }
  .transmute_weights(x, weights, order, to, mean)
}

#' Transmute weights for a nested generalized mean
#'
#' Transmute weights to turn a nested generalized mean of a given order into a
#' generalized mean of order `to`. Useful for calculating additive and
#' multiplicative decompositions for an index made
#' of nested generalized means (e.g., Fisher index).
#'
#' @inheritParams nested_gmean
#' @param to A finite number giving the order of the target generalized mean for
#'   the transmuted weights. The default constructs weights for an arithmetic
#'   mean.
#' @param pivot A finite number giving the pivot value for the transmuted
#'   weights. The default uses the order of the outer generalized mean,
#'   otherwise `to` is common alternative.
#'   See `vignette("decomposing-indexes")` for details.
#'
#' @returns
#' A numeric vector, the same length as `x`, that sums to 1.
#'
#' @references
#' See `vignette("decomposing-indexes")` for more details.
#'
#' @examples
#' x <- 1:3
#' w1 <- 3:1
#' w2 <- c(1, 2, 1)
#'
#' # Calculate the geometric mean of the arithmetic and harmonic means
#' # as an arithmetic mean.
#' nested_gmean(x, list(w1, w2))
#' gmean(x, transmute_weights2(x, list(w1, w2), to = 1))
#' @family math functions
#' @export
transmute_weights2 <- function(
  x,
  weights = list(NULL, NULL),
  order = c(1, -1),
  outer_weights = c(1, 1),
  outer_order = 0,
  to = 1,
  pivot = outer_order
) {
  if (different_length(x, weights[[1]], weights[[2]])) {
    stop("`x` and non-NULL components of `weights` must be the same length")
  }
  if (length(order) != 2L && !all(is.finite(order))) {
    stop("`order` must be a pair of finite numbers")
  }
  na_mask <- if (anyNA(x) || anyNA(weights[[1]]) || anyNA(weights[[2]])) {
    stats::complete.cases(x, weights)
  }
  if (!is.null(na_mask)) {
    x[!na_mask] <- NA_real_
  }
  inner_mean <- c(
    .gmean(x, weights[[1]], order[1], na_mask),
    .gmean(x, weights[[2]], order[2], na_mask)
  )
  outer_mean <- gmean(inner_mean, outer_weights, outer_order, na.rm = TRUE)
  v1 <- transmute_weights(x, weights[[1]], order[1], pivot, inner_mean[1])
  v2 <- transmute_weights(x, weights[[2]], order[2], pivot, inner_mean[2])
  t <- transmute_weights(
    inner_mean,
    outer_weights,
    outer_order,
    pivot,
    outer_mean
  )
  if (is.na(t[1])) {
    transmute_weights(x, v2 * t[2], pivot, to)
  } else if (is.na(t[2])) {
    transmute_weights(x, v1 * t[1], pivot, to)
  } else {
    transmute_weights(x, v1 * t[1] + v2 * t[2], pivot, to)
  }
}

#' Internal function to transmute weights
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
