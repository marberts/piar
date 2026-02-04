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
#' @param x A price index, as made by, e.g., [elementary_index()].
#' @param weights A numeric vector of weights for the index values in `x`, or
#'   something that can be coerced into one. The
#'   default is equal weights. It is usually easiest to specify these weights as
#'   a matrix with a row for each index value in `x` and a column for each
#'   time period.
#' @param window A positive integer giving the size of the window used to
#'   average index values across subperiods. The default averages over all
#'   periods in `x`. Non-integers are truncated towards 0.
#' @param na_action Approach for missing values be when aggregating the
#'   across subperiods. One of `"pass"`, `"omit"`, or `"fail"`. By default,
#'   missing values are passed over and not removed.
#' @param r Order of the generalized mean to aggregate index values. 0 for a
#'   geometric index (the default for making elementary indexes), 1 for an
#'   arithmetic index (the default for aggregating elementary indexes and
#'   averaging indexes over subperiods), or -1 for a harmonic index (usually for
#'   a Paasche index). Other values are possible; see
#'   [gpindex::generalized_mean()] for details.
#' @param contrib Aggregate percent-change contributions in `x` (if any)?
#' @param ... Not currently used.
#' @param duplicate_contrib The method to deal with duplicate product
#'   contributions. Either `"sum"` to add contributions for the same products
#'   across subperiods (the default) or `"make.unique"` to make duplicate
#'   product names unique with [make.unique()].
#'
#' @returns
#' A price index, averaged over subperiods, that inherits from the same
#' class as `x`.
#'
#' @references
#' Balk, B. M. (2008). *Price and Quantity Index Numbers*.
#' Cambridge University Press.
#'
#' @examples
#' index <- as_index(matrix(c(1:12, 12:1), 2, byrow = TRUE), chainable = FALSE)
#'
#' # Turn a monthly index into a quarterly index
#' mean(index, window = 3)
#'
#' @family index methods
#' @export
mean.chainable_piar_index <- function(
  x,
  ...,
  weights = NULL,
  window = ntime(x),
  na_action = c("pass", "omit", "fail"),
  contrib = TRUE,
  r = 1,
  duplicate_contrib = c("sum", "make.unique")
) {
  chkDots(...)
  mean_index(
    x,
    weights,
    window = window,
    na_action = na_action,
    contrib = contrib,
    r = r,
    chainable = TRUE,
    duplicate_contrib = duplicate_contrib
  )
}

#' @rdname mean.piar_index
#' @export
mean.direct_piar_index <- function(
  x,
  ...,
  weights = NULL,
  window = ntime(x),
  na_action = c("pass", "omit", "fail"),
  contrib = TRUE,
  r = 1,
  duplicate_contrib = c("make.unique", "sum")
) {
  chkDots(...)
  mean_index(
    x,
    weights,
    window = window,
    na_action = na_action,
    contrib = contrib,
    r = r,
    chainable = FALSE,
    duplicate_contrib = duplicate_contrib
  )
}

#' Internal function to aggregate over subperiods
#' @noRd
mean_index <- function(
  x,
  weights,
  window,
  na_action,
  contrib,
  r,
  chainable,
  duplicate_contrib
) {
  na_action <- match.arg(na_action)
  if (!is.null(weights)) {
    weights <- as.numeric(weights)
    if (any(weights < 0, na.rm = TRUE)) {
      stop("all elements of 'weights' must be non-negative")
    }
    if (length(weights) != ntime(x) * nlevels(x)) {
      stop("'weights' must have a value for each index value in 'x'")
    }
    dim(weights) <- c(nlevels(x), ntime(x))
  }

  window <- as.integer(window)
  if (length(window) > 1L || window < 1L) {
    stop("'window' must be a positive length 1 integer")
  }
  if (window > ntime(x)) {
    stop("'x' must have at least 'window' time periods")
  }

  # Helpful functions.
  gen_mean <- Vectorize(gpindex::generalized_mean(r), USE.NAMES = FALSE)
  agg_contrib <- Vectorize(
    aggregate_contrib(r, duplicate_contrib),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )

  # Get the starting location for each window.
  if (ntime(x) %% window != 0) {
    warning("'window' is not a multiple of the number of time periods in 'x'")
  }
  len <- ntime(x) %/% window
  loc <- seq.int(1L, by = window, length.out = len)
  periods <- x$time[loc]

  if (na_action == "fail") {
    if (anyNA(x$index)) {
      stop("'x' contains missing values")
    } else if (anyNA(weights)) {
      stop("'weights' contains missing values")
    }
  }

  has_contrib <- !is.null(x$contrib) && contrib
  # Loop over each window and calculate the mean for each level.
  res <- contrib <- vector("list", length(periods))
  rows <- seq_len(nlevels(x))
  w <- list(NULL)
  for (i in seq_along(loc)) {
    j <- seq(loc[i], length.out = window)
    rel <- split_rows(x$index[, j, drop = FALSE], rows)
    if (!is.null(weights)) {
      w <- split_rows(weights[, j, drop = FALSE], rows)
    }
    res[[i]] <- gen_mean(rel, w, na.rm = na_action == "omit")
    if (has_contrib) {
      con <- split_rows(x$contrib[, j, drop = FALSE], rows)
      contrib[[i]] <- agg_contrib(con, rel, w)
    }
  }

  piar_index(
    do.call(cbind, res),
    do.call(cbind, contrib),
    x$levels,
    periods,
    chainable
  )
}
