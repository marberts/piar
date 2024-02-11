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
#'
#' Percent-change contributions are aggregated if `contrib = TRUE` by treating
#' each product-subperiod pair as a unique product, then following the same
#' approach as [`aggregate()`][aggregate.piar_index]. The number of the
#' subperiod is appended to product names to make them unique across subperiods.
#'
#' An optional vector of weights can be specified when aggregating index values
#' over subperiods, which is often useful when aggregating a Paasche index; see
#' section 4.3 of Balk (2008) for details.
#'
#' @param x A price index, as made by, e.g., [elemental_index()].
#' @param weights A numeric vector of weights for the index values in `x`. The
#' default is equal weights. It is usually easiest to specify these weights as
#' a matrix with a row for each index value in `x` and a column for each
#' time period.
#' @param window A positive integer giving the size of the window used to
#' average index values across subperiods. The default (3) turns a monthly
#' index into into a quarterly one. Non-integers are truncated towards 0.
#' @param na.rm Should missing values be removed? By default, missing values
#' are not removed. Setting `na.rm = TRUE` is equivalent to overall mean
#' imputation.
#' @param r Order of the generalized mean to aggregate index values. 0 for a
#' geometric index (the default for making elemental indexes), 1 for an
#' arithmetic index (the default for aggregating elemental indexes and
#' averaging indexes over subperiods), or -1 for a harmonic index (usually for
#' a Paasche index). Other values are possible; see
#' [gpindex::generalized_mean()] for details.
#' @param contrib Aggregate percent-change contributions in `x` (if any)?
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
mean.piar_index <- function(x, weights = NULL, window = 3L, na.rm = FALSE,
                            r = 1, contrib = TRUE, ...) {
  if (!is.null(weights)) {
    if (length(weights) != length(x$time) * length(x$levels)) {
      stop("'weights' must have a value for each index value in 'x'")
    }
    w <- split(as.numeric(weights), gl(length(x$time), length(x$levels)))
  }

  window <- as.integer(window)
  if (length(window) > 1L || window < 1L) {
    stop("'window' must be a positive length 1 integer")
  }
  if (window > length(x$time)) {
    stop("'x' must have at least 'window' time periods")
  }

  # helpful functions
  gen_mean <- Vectorize(gpindex::generalized_mean(r), USE.NAMES = FALSE)
  agg_contrib <- Vectorize(aggregate_contrib(r),
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  )

  # get the starting location for each window
  len <- length(x$time) %/% window
  loc <- seq.int(1L, by = window, length.out = len)
  periods <- x$time[loc]

  has_contrib <- has_contrib(x) && contrib

  # loop over each window and calculate the mean for each level
  index <- index_skeleton(x$levels, periods)
  contrib <- contrib_skeleton(x$levels, periods)
  for (i in seq_along(loc)) {
    j <- seq(loc[i], length.out = window)
    rel <- .mapply(c, x$index[j], list())
    weight <- if (is.null(weights)) list(NULL) else .mapply(c, w[j], list())
    index[[i]][] <- gen_mean(rel, weight, na.rm = na.rm)
    if (has_contrib) {
      con <- .mapply(\(...) c(list(...)), x$contrib[j], list())
      contrib[[i]][] <- agg_contrib(con, rel, weight)
    }
  }
  
  x$index <- index
  x$contrib <- contrib
  x$time <- periods
  validate_piar_index(x)
}
