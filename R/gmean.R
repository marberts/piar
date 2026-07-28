#' Generalized mean
#'
#' Calculated a weighted generalized mean.
#'
#' The generalized mean is also called the power mean, Hölder mean, or \eqn{l_p}
#' mean; see Bullen (2003, p. 175) for details.
#'
#' Both `x` and `weights` are usually strictly positive
#' (and finite), especially for the purpose of making a price index. This is not
#' enforced, but the results may not make sense if the generalized mean is not
#' defined. There are two exceptions to this.
#' 1. The convention by Hardy et al. (1952, p. 13) is used in cases where `x`
#' has zeros: the generalized mean is 0 whenever the weights are strictly
#' positive and `order < 0`. The analogous convention holds whenever at least
#' one element of `x` is `Inf`: the generalized mean is `Inf` whenever the
#' weights are strictly positive and `order > 0`.
#'
#' 2. Some authors let the weights be non-negative and sum to 1. If there are
#' zero weights then the corresponding element
#' of `x` has no impact on the result whenever `x` is strictly
#' positive. Unlike [weighted.mean()], however,
#' zero weights are not strong zeros, so infinite values in `x` will
#' propagate.
#'
#' The weights are scaled to sum to 1 to satisfy the definition of a
#' generalized mean.
#'
#' @family math functions
#' @export
#'
#' @param x `[numeric > 0]` A strictly positive numeric vector.
#' @param weights `[numeric >= 0]` A positive numeric vector of weights, the
#'   same length as `x`. The default is to equally weight each element of `x`.
#' @param order `[numeric(1)]` A finite number giving the order (or exponent) of
#'   the generalized mean. The default calculates an arithmetic mean.
#' @param na.rm `[logical(1)]` Should missing values be removed? By default,
#'   missing values are not removed.
#'
#' @returns
#' A numeric value for the generalized mean.
#'
#' @note
#' The generalized mean can be defined on the extended real line, so
#' that `order = -Inf / Inf` returns [min()]/[max()], to agree with the
#' definition by Bullen (2003). This is not implemented, and the order of the
#' generalized mean must be finite.
#'
#' @references
#' Bullen, P. S. (2003). *Handbook of Means and Their Inequalities*.
#' Springer Science+Business Media.
#'
#' Hardy, G., Littlewood, J. E., and Polya, G. (1952). *Inequalities* (2nd
#' edition). Cambridge University Press.
#'
#' @examples
#' x <- 1:3
#' w <- c(0.25, 0.25, 0.5)
#'
#' # Arithmetic mean.
#' gmean(x, w)
#'
#' # Geometric mean.
#' gmean(x, w, order = 0)
#'
#' # The Lehmer mean is a generalized mean with specific weights.
#' gmean(x, w * x)
gmean <- function(x, weights = NULL, order = 1, na.rm = FALSE) {
  if (not_finite_scalar(order)) {
    stop("`order` must be a finite number")
  }
  if (!is.null(weights) && length(x) != length(weights)) {
    stop("`x` and `weights` must be the same length")
  }
  na_mask <- if (na.rm && (anyNA(x) || anyNA(weights))) {
    stats::complete.cases(x, weights)
  }
  .gmean(x, weights, order, na_mask)
}

#' Nested generalized means
#'
#' Calculate a weighted (outer) generalized mean of two (inner) generalized
#' means (i.e., crossing means).
#'
#' @family math functions
#' @export
#'
#' @param x `[numeric > 0]` A strictly positive numeric vector.
#' @param weights `[list]` A list of positive numeric vector of
#'   weights, each
#'   the same length as `x`, for both of the inner generalized means. `NULL`
#'   elements of `weights` equally weight each element of `x`. The default
#'   uses equal weights for both inner generalized mean.
#' @param order `[numeric(2)]` A finite numeric vector giving the order of each
#'   of the inner
#'   generalized means. The default computes an arithmetic mean and a harmonic
#'   mean.
#' @param outer_weights `[numeric(2)]` A strictly positive numeric vector
#'   weights for each of
#'   the inner generalized means as used in the outer generalized mean. The
#'   default weights each inner generalized mean equally.
#' @param outer_order `[numeric(1)]` A finite number giving the order of the
#'   outer generalized mean. The default uses a geometric mean.
#' @param na.rm `[logical(1)]` Should missing values in `x` and `weights` be
#'   removed? By
#'   default missing values are not removed. Note that removal of missing values
#'   is balanced across `x` and both elements of `weights`.
#'
#' @returns
#' A numeric value for the nested generalized mean.
#'
#' @examples
#' x <- 1:3
#' w1 <- c(0.25, 0.25, 0.5)
#' w2 <- c(0.3, 0.3, 0.4)
#' # Calculate the geometric mean of the arithmetic and harmonic means
#' # to make a Fisher index.
#' nested_gmean(x, list(w1, w2))
nested_gmean <- function(
  x,
  weights = list(NULL, NULL),
  order = c(1, -1),
  outer_weights = NULL,
  outer_order = 0,
  na.rm = FALSE
) {
  if (different_length(x, weights[[1]], weights[[2]])) {
    stop("`x` and non-NULL components of `weights` must be the same length")
  }
  if (not_finite_pair(order)) {
    stop("`order` must be a pair of finite numbers")
  }
  if (length(weights) != length(order)) {
    stop("`weights` and `order` must be the same length")
  }
  na_mask <- if (na.rm && (anyNA(x) || anyNA(weights, recursive = TRUE))) {
    stats::complete.cases(x, weights[[1]], weights[[2]])
  }
  inner_mean <- c(
    .gmean(x, weights[[1]], order[1], na_mask),
    .gmean(x, weights[[2]], order[2], na_mask)
  )
  gmean(inner_mean, outer_weights, outer_order, na.rm = na.rm)
}

#' Internal generalized mean
#' @noRd
.gmean <- function(x, weights, r, na_mask) {
  if (!is.null(na_mask)) {
    x <- x[na_mask]
    weights <- weights[na_mask]
  }
  if (is.null(weights)) {
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
