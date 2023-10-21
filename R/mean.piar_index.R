#' Aggregate a price index over subperiods
#'
#' Aggregate an index over subperiods by taking the (usually arithmetic) mean
#' of index values over consecutive windows of subperiods.
#'
#' The `mean()` method constructs a set of non-overlapping windows of
#' length `window`, starting in the first period of the index, and takes
#' the mean of each index value in these windows for each level of the index.
#' The last window is discarded if it is incomplete, so that index values are
#' always averaged over `window` periods. The names for the first time
#' period in each window form the new names for the aggregated time periods.
#' Note that percent-change contributions are discarded when aggregating over
#' subperiods.
#'
#' An optional vector of weights can be specified when aggregating index values
#' over subperiods, which is often useful when aggregating a Paasche index; see
#' section 4.3 of Balk (2008) for details.
#'
#' @param x A price index, as made by, e.g., [elemental_index()].
#' @param w A numeric vector of weights for the index values in `x`. The
#' default is equal weights. It is usually easiest to specify these weights as
#' a matrix with a row for each index value in `x` and a column for each
#' time period.
#' @param window The size of the window used to average index values across
#' subperiods. The default (3) turns a monthly index into into a quarterly one.
#' @param na.rm Should missing values be removed? By default, missing values
#' are not removed. Setting `na.rm = TRUE` is equivalent to overall mean
#' imputation.
#' @param r Order of the generalized mean to aggregate index values. 0 for a
#' geometric index (the default for making elemental indexes), 1 for an
#' arithmetic index (the default for aggregating elemental indexes and
#' averaging indexes over subperiods), or -1 for a harmonic index (usually for
#' a Paasche index). Other values are possible; see
#' [gpindex::generalized_mean()] for details.
#' @param ... Further arguments passed to or used by methods.
#' 
#' @returns
#' A price index with the same class as `x`.
#'
#' @references Balk, B. M. (2008). *Price and Quantity Index Numbers*.
#' Cambridge University Press.
#'
#' @examples
#' prices <- data.frame(
#'   rel = 1:8,
#'   period = rep(1:2, each = 4),
#'   ea = rep(letters[1:2], 4)
#' )
#'
#' epr <- with(prices, elemental_index(rel, period, ea))
#'
#' mean(epr, window = 2)
#'
#' @family index methods
#' @export
mean.piar_index <- function(x, w = NULL, window = 3, na.rm = FALSE,
                            r = 1, ...) {
  if (!is.null(w)) {
    if (length(w) != length(x$time) * length(x$levels)) {
      stop("'x' and 'w' must be the same length")
    }
    w <- split(as.numeric(w), gl(length(x$time), length(x$levels)))
  }
  gen_mean <- Vectorize(generalized_mean(r))
  len <- length(x$time) %/% window
  if (len == 0L) {
    stop("'x' must have at least 'window' time periods")
  }
  # get the starting location for each window
  loc <- seq.int(1L, by = window, length.out = len)
  periods <- x$time[loc]
  res <- index_skeleton(x$levels, periods)
  # loop over each window and calculate the mean for each level
  for (i in seq_along(loc)) {
    j <- seq(loc[i], length.out = window)
    index <- .mapply(c, x$index[j], list())
    weight <- if (is.null(w)) list(NULL) else .mapply(c, w[j], list())
    res[[i]][] <- gen_mean(index, weight, na.rm = na.rm)
  }
  x$index <- res
  x$contrib <- contrib_skeleton(x$levels, periods)
  x$time <- periods
  validate_piar_index(x)
}
