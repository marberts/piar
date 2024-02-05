dim_indices <- function(x, i) {
  if (!missing(i) && is.character(i)) {
    names(x) <- x
  }
  res <- match(x[i], x, incomparables = NA)
  if (anyNA(res) || length(res) == 0L) {
    stop("subscript out of bounds")
  }
  res
}

#' Extract and replace index values
#'
#' Methods to extract and replace index values like a matrix.
#'
#' The extraction methods treat `x` as a matrix of index values with
#' (named) rows for each `level` and columns for each `period` in
#' `x`. Unlike a matrix, dimensions are never dropped as subscripting
#' `x` always returns an index object. This means that subscripting with a
#' matrix is not possible, and only a submatrix can be extracted. As `x`
#' is not an atomic vector, subscripting with a single index like `x[1]`
#' extracts all time periods for that level.
#'
#' The replacement methods similarly treat `x` as a matrix, and behave the
#' same as replacing values in a matrix (except that `value` is coerced to
#' numeric). Note that replacing the values of an index will remove the
#' corresponding percent-change contributions (if any).
#'
#' Subscripting an aggregate index cannot generally preserve the aggregation
#' structure if any levels are removed or rearranged, and in this case the
#' resulting index is *not* an aggregate index. Similarly, replacing the
#' values for an aggregate index generally breaks consistency in aggregation,
#' and therefore the result is *not* an aggregate index.
#'
#' @param x A price index, as made by, e.g., [elemental_index()].
#' @param i,j Indices for the levels and time periods of a price index. See
#' details.
#' @param value A numeric vector.
#' @param ... Ignored.
#'
#' @returns
#' A price index that inherits from [`chainable_piar_index`] if `x` is a
#' period-over-period index, or [`direct_piar_index`] if `x` is a
#' fixed-base index. If `x` inherits from [`aggregate_piar_index`] then
#' `[` returns an aggregate index if the levels are unchanged.
#'
#' @examples
#' prices <- data.frame(
#'   rel = 1:8,
#'   period = rep(1:2, each = 4),
#'   ea = rep(letters[1:2], 4)
#' )
#'
#' # Calculate Jevons elemental indexes
#'
#' epr <- with(prices, elemental_index(rel, period, ea))
#'
#' # Extract the indexes like a matrix
#'
#' epr["a", ]
#'
#' epr[, 2]
#'
#' epr[1, ] <- 1 # can be useful for doing specific imputations
#' epr
#'
#' @family index methods
#' @export
`[.piar_index` <- function(x, i, j, ...) {
  if (!missing(i) && is.matrix(i)) {
    as.matrix(x)[i]
  } else {
    levels <- dim_indices(x$levels, i)
    periods <- dim_indices(x$time, j)
    x$index <- lapply(x$index[periods], `[`, levels)
    x$contrib <- lapply(x$contrib[periods], `[`, levels)
    x$levels <- x$levels[levels]
    x$time <- x$time[periods]
    validate_piar_index(x)
  }
}

#' @export
`[.aggregate_piar_index` <- function(x, i, j, ...) {
  res <- NextMethod("[")
  if (is_index(res) && !identical(res$levels, x$levels)) {
    new_piar_index(
      res$index, res$contrib, res$levels, res$time,
      is_chainable_index(res)
    )
  } else {
    res
  }
}

#' @rdname sub-.piar_index
#' @export
`[<-.piar_index` <- function(x, i, j, ..., value) {
  # do all the replacements with a matrix of x so that value is recycled
  # the same way as it would be for a matrix
  res <- as.matrix(x)
  if (!missing(i) && is.matrix(i)) {
    if (is.logical(i)) {
      dim <- c(length(x$levels), length(x$time))
      i <- arrayInd(which(rep_len(i, prod(dim))), dim)
    }
    levels <- dim_indices(x$levels, i[, 1])
    periods <- dim_indices(x$time, i[, 2])
    res[cbind(levels, periods)] <- as.numeric(value)
    # only loop over periods that have a value replaced
    for (i in seq_along(levels)) {
      x$index[[periods[i]]][levels[i]] <- res[levels[i], periods[i]]
      # drop contributions for replaced values
      x$contrib[[periods[i]]][levels[i]] <- list(numeric(0L))
    }
  } else {
    levels <- dim_indices(x$levels, i)
    periods <- dim_indices(x$time, j)
    if (is_index(value)) {
      if (length(value$time) != length(periods)) {
        stop("'x' and 'value' must have the same number of time periods")
      }
      if (length(levels) %% length(value$levels) != 0) {
        stop("number of items to replace is not a multiple of replacement length")
      }
      for (t in seq_along(periods)) {
        x$index[[periods[t]]][levels] <- value$index[[t]]
        x$contrib[[periods[t]]][levels] <- value$contrib[[t]]
      }
    } else {
      res[levels, periods] <- as.numeric(value)
      for (t in periods) {
        x$index[[t]][levels] <- res[levels, t]
        x$contrib[[t]][levels] <- list(numeric(0L))
      }
    }
  }
  validate_piar_index(x)
}

#' @export
`[<-.aggregate_piar_index` <- function(x, i, j, ..., value) {
  x <- new_piar_index(
    x$index, x$contrib, x$levels, x$time,
    is_chainable_index(x)
  )
  NextMethod("[<-")
}

#' @export
`[<-.chainable_piar_index` <- function(x, i, j, ..., value) {
  if (is_index(value) && !is_chainable_index(value)) {
    stop("'value' must be a period-over-period index")
  }
  NextMethod("[<-")
}

#' @export
`[<-.direct_piar_index` <- function(x, i, j, ..., value) {
  if (is_index(value) && !is_direct_index(value)) {
    stop("'value' must be a fixed-base index")
  }
  NextMethod("[<-")
}
