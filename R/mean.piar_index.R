#' Aggregate a price index over subperiods
#'
#' Aggregate an index over subperiods by taking the (usually arithmetic) mean
#' of index values over consecutive windows of subperiods.
#'
#' The `mean()` method constructs a set of non-overlapping windows of
#' length `window`, starting in the first period of the index, and takes
#' the mean of each index value in these windows for each level of the index.
#' The last window is discarded if it is incomplete (with a warning), so that
#' index values are
#' always averaged over `window` periods. The names for the first time
#' period in each window form the new names for the aggregated time periods.
#'
#' Percent-change contributions are aggregated if `contrib = TRUE` following the
#' same approach as [`aggregate()`][aggregate.piar_index].
#'
#' An optional vector of weights can be specified when aggregating index values
#' over subperiods, which is often useful when aggregating a Paasche index; see
#' section 4.3 of Balk (2008) for details.
#'
#' @name mean.piar_index
#' @aliases mean.piar_index
#'
#' @param x A price index, as made by, e.g., [elemental_index()].
#' @param weights A numeric vector of weights for the index values in `x`, or
#'   something that can be coerced into one. The
#'   default is equal weights. It is usually easiest to specify these weights as
#'   a matrix with a row for each index value in `x` and a column for each
#'   time period.
#' @param window A positive integer giving the size of the window used to
#'   average index values across subperiods. The default averages over all
#'   periods in `x`. Non-integers are truncated towards 0.
#' @param na.rm Should missing values be removed? By default, missing values
#'   are not removed. Setting `na.rm = TRUE` is equivalent to overall mean
#'   imputation.
#' @param r Order of the generalized mean to aggregate index values. 0 for a
#'   geometric index (the default for making elemental indexes), 1 for an
#'   arithmetic index (the default for aggregating elemental indexes and
#'   averaging indexes over subperiods), or -1 for a harmonic index (usually for
#'   a Paasche index). Other values are possible; see
#'   [gpindex::generalized_mean()] for details.
#' @param contrib Aggregate percent-change contributions in `x` (if any)?
#' @param ... Not currently used.
#' @param dup_products The method to deal with duplicate product contributions.
#'   Either 'make.unique' to make duplicate product names unique
#'   with [make.unique()] or 'sum' to add contributions for the same products
#'   across subperiods.
#'
#' @returns
#' A price index, averaged over subperiods, that inherits from the same
#' class as `x`.
#'
#' @references Balk, B. M. (2008). *Price and Quantity Index Numbers*.
#' Cambridge University Press.
#'
#' @examples
#' index <- as_index(matrix(c(1:12, 12:1), 2, byrow = TRUE))
#'
#' # Turn a monthly index into a quarterly index
#' mean(index, window = 3)
#'
#' @family index methods
#' @export
mean.chainable_piar_index <- function(x,
                                      ...,
                                      weights = NULL,
                                      window = ntime(x),
                                      na.rm = FALSE,
                                      contrib = TRUE,
                                      r = 1,
                                      dup_products = c("make.unique", "sum")) {
  chkDots(...)
  mean_index(
    x,
    weights,
    window = window,
    na.rm = na.rm,
    contrib = contrib,
    r = r,
    chainable = TRUE,
    dup_products = dup_products
  )
}

#' @rdname mean.piar_index
#' @export
mean.direct_piar_index <- function(x,
                                   ...,
                                   weights = NULL,
                                   window = ntime(x),
                                   na.rm = FALSE,
                                   contrib = TRUE,
                                   r = 1,
                                   dup_products = c("make.unique", "sum")) {
  chkDots(...)
  mean_index(
    x,
    weights,
    window = window,
    na.rm = na.rm,
    contrib = contrib,
    r = r,
    chainable = FALSE,
    dup_products = dup_products
  )
}

#' Internal function to aggregate over subperiods
#' @noRd
mean_index <- function(x,
                       weights,
                       window,
                       na.rm,
                       contrib,
                       r,
                       chainable,
                       dup_products) {
  if (!is.null(weights)) {
    weights <- as.numeric(weights)
    if (length(weights) != length(x$time) * length(x$levels)) {
      stop("'weights' must have a value for each index value in 'x'")
    }
    w <- split(weights, gl(length(x$time), length(x$levels)))
  }

  window <- as.integer(window)
  if (length(window) > 1L || window < 1L) {
    stop("'window' must be a positive length 1 integer")
  }
  if (window > length(x$time)) {
    stop("'x' must have at least 'window' time periods")
  }

  # Helpful functions.
  gen_mean <- Vectorize(gpindex::generalized_mean(r), USE.NAMES = FALSE)
  agg_contrib <- Vectorize(
    aggregate_contrib(r, dup_products),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )

  # Get the starting location for each window.
  if (length(x$time) %% window != 0) {
    warning("'window' is not a multiple of the number of time periods in 'x'")
  }
  len <- length(x$time) %/% window
  loc <- seq.int(1L, by = window, length.out = len)
  periods <- x$time[loc]

  has_contrib <- has_contrib(x) && contrib

  # Loop over each window and calculate the mean for each level.
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

  piar_index(index, contrib, x$levels, periods, chainable)
}
