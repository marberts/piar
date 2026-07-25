#' Extended mean
#'
#' Calculate the component-wise extended mean.
#'
#' The extended mean is also called the difference mean, Stolarsky mean, or
#' extended mean-value mean; see Bullen (2003, p. 393) for details.
#'
#' Both `x` and `y` should be strictly positive. This is not enforced, but the
#' results may not make sense when the extended mean is not defined. The usual
#' recycling rules apply when `x` and `y` are not the same length.
#'
#' By definition, the extended mean of `x`
#' and `y` is `x` when `x == y`. The `tol` argument is used
#' to test equality by checking if `abs(x - y) <= tol`.  In some cases it's
#' useful to multiply
#' `tol` by a scale factor, such as `max(abs(x), abs(y))`. This often
#' doesn't matter when making price indexes, however, as `x` and `y`
#' are usually around 1.
#'
#' @family math functions
#' @export
#'
#' @param x,y `[numeric > 0]` A strictly positive numeric vector.
#' @param order `[numeric(2)]` A pair of finite numbers giving the order of the
#'   extended mean.
#'   The default calculates the ordinary logarithmic mean. Setting either the
#'   first or second element to 1 gives the generalized logarithmic mean.
#' @param tol `[numeric > 0]` The tolerance used to determine if `x == y`.
#'   The default value is the same as [all.equal()].
#'
#' @returns
#' A numeric vector, the same length as
#' `max(length(x), length(y))`, giving the component-wise extended mean
#' of `x` and `y`.
#'
#' @references
#' Bullen, P. S. (2003). *Handbook of Means and Their Inequalities*.
#' Springer Science+Business Media.
#'
#' @examples
#' x <- 8:5
#' y <- 1:4
#'
#' # The arithmetic and geometric means are special cases of the
#' # generalized logarithmic mean.
#' all.equal(emean(x, y, c(2, 1)), (x + y) / 2)
#' all.equal(emean(x, y, c(-1, 1)), sqrt(x * y))
#'
#' # The harmonic mean cannot be expressed as a logarithmic mean, but can
#' # be expressed as an extended mean.
#' all.equal(emean(x, y, c(-2, -1)), 2 / (1 / x + 1 / y))
#'
#' # The quadratic mean is also a type of extended mean.
#' all.equal(emean(x, y, c(2, 4)), sqrt(x^2 / 2 + y^2 / 2))
#'
#' # As are heronian and centroidal means.
#' all.equal(
#'   emean(x, y, c(0.5, 1.5)),
#'   (x + sqrt(x * y) + y) / 3
#' )
#' all.equal(
#'   emean(x, y, c(2, 3)),
#'   2 / 3 * (x^2 + x * y + y^2) / (x + y)
#' )
emean <- function(
  x,
  y,
  order = c(0, 1),
  tol = .Machine$double.eps^0.5
) {
  if (not_finite_pair(order)) {
    stop("`order` must be a pair of finite numbers")
  }
  r <- order[[1]]
  s <- order[[2]]

  # Recycling x and y here avoids multiple warnings if one is not a multiple
  # length of the other.
  if (length(x) > length(y)) {
    if (length(y) > 0 && length(x) %% length(y) != 0) {
      warning("length of `x` is not a multiple of length of `y`")
      y <- rep_len(y, length(x))
    }
  } else if (length(x) < length(y)) {
    if (length(x) > 0 && length(y) %% length(x) != 0) {
      warning("length of `y` is not a multiple of length of `x`")
      x <- rep_len(x, length(y))
    }
  }

  max_len <- max(length(x), length(y))
  if (max_len > 0 && length(tol) > max_len) {
    stop("`tol` cannot be longer than `x` or `y`")
  }

  if (r == 0 && s == 0) {
    res <- sqrt(x * y)
  } else if (r == 0) {
    res <- ((x^s - y^s) / log(x / y) / s)^(1 / s)
  } else if (s == 0) {
    res <- ((x^r - y^r) / log(x / y) / r)^(1 / r)
  } else if (r == s) {
    res <- exp((x^r * log(x) - y^r * log(y)) / (x^r - y^r) - 1 / r)
  } else {
    res <- ((x^s - y^s) / (x^r - y^r) * (r / s))^(1 / (s - r))
  }
  # Set output to `x` when `x` == `y`.
  i <- which(abs(x - y) <= tol)
  res[i] <- x[(i - 1L) %% length(x) + 1L]
  res
}
