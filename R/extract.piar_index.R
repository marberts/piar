dim_indices <- function(x, i) {
  if (missing(i)) {
    return(seq_along(x))
  }
  if (is.character(i)) {
    res <- match(i, x, incomparables = NA)
  } else {
    res <- match(x[i], x, incomparables = NA)
  }
  if (length(res) == 0L || anyNA(res)) {
    stop("subscript out of bounds")
  }
  res
}

replace_matrix <- function(x, i, value) {
  if (is.logical(i)) {
    if (nrow(i) != length(x$levels) || ncol(i) != length(x$time)) {
      stop(
        "'i' must have a row for each level and a column for each ",
        "time period in 'x'"
      )
    }
    i <- which(i, arr.ind = TRUE)
    if (nrow(i) == 0L) {
      return(x)
    }
  }
  
  if (ncol(i) != 2L) {
    stop("'i' must have exactly two columns")
  }
  
  value <- as.numeric(value)
  n <- length(value)
  if (n == 0L) {
    stop("replacement has length zero")
  }
  
  levels <- dim_indices(x$levels, i[, 1L])
  periods <- dim_indices(x$time, i[, 2L])
  if (length(levels) %% n != 0L) {
    stop("number of items to replace is not a multiple of replacement length")
  }
  
  for (i in seq_along(levels)) {
    x$index[[periods[i]]][levels[i]] <- value[(i - 1L) %% n + 1]
    # drop contributions for replaced values
    x$contrib[[periods[i]]][levels[i]] <- list(numeric(0L))
  }
  x
}

replace_index <- function(x, i, j, value) {
  levels <- dim_indices(x$levels, i)
  periods <- dim_indices(x$time, j)
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
  x
}

replace_numeric <- function(x, i, j, value) {
  levels <- dim_indices(x$levels, i)
  periods <- dim_indices(x$time, j)
  
  value <- as.numeric(value)
  n <- length(value)
  if (n == 0L) {
    stop("replacement has length zero")
  }
  
  m <- length(levels)
  if ((m * length(periods)) %% n != 0) {
    stop("number of items to replace is not a multiple of replacement length")
  }
  
  s <- seq.int(0L, m - 1L)
  for (t in seq_along(periods)) {
    x$index[[periods[t]]][levels] <- value[(s + (t - 1L) * m) %% n + 1]
    x$contrib[[periods[t]]][levels] <- list(numeric(0L))
  }
  x
}

#' Extract and replace index values
#'
#' Methods to extract and replace index values like a matrix.
#'
#' The extraction method treat `x` like a matrix of index values with
#' (named) rows for each `level` and columns for each `period` in
#' `x`. Unlike a matrix, dimensions are never dropped as subscripting
#' `x` always returns an index object. This means that subscripting with a
#' matrix is not possible, and only a "submatrix" can be extracted. As `x`
#' is not an atomic vector, subscripting with a single index like `x[1]`
#' extracts all time periods for that level.
#'
#' The replacement method similarly treat `x` like a matrix. If `value` is
#' an index object with the same number of time periods as `x[i, j]` and
#' it inherits from the same class as `x`, then the index values and
#' percent-change contributions of `x[i, j]` are replaced with those for the
#' corresponding levels of `value`. If `value` is not an index, then it is
#' coerced to a numeric vector and behaves the same as replacing values in a 
#' matrix. Note that replacing the values of an index will remove the
#' corresponding percent-change contributions (if any). Unlike extraction, it
#' is possible to replace value in `x` using a logical matrix or a two-column
#' matrix of indices.
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
#' @param value A numeric vector or price index. See details.
#' @param ... Not currently used.
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
  periods <- dim_indices(x$time, j)
  # Optimize for extracting by time period.
  if (missing(i)) {
    x$index <- x$index[periods]
    x$contrib <- x$contrib[periods]
  } else {
    levels <- dim_indices(x$levels, i)
    x$index <- lapply(x$index[periods], `[`, levels)
    x$contrib <- lapply(x$contrib[periods], `[`, levels)
    x$levels <- x$levels[levels]
  }
  x$time <- x$time[periods]
  validate_piar_index(x)
}

#' @export
`[.aggregate_piar_index` <- function(x, i, j, ...) {
  res <- NextMethod("[")
  if (!identical(res$levels, x$levels)) {
    new_piar_index(
      res$index, res$contrib, res$levels, res$time,
      is_chainable_index(res)
    )
  } else {
    res
  }
}

# FIXME: Does it make sense to bundle the contributions with the index values
# as a list with a dim?
#' @rdname sub-.piar_index
#' @export
`[<-.piar_index` <- function(x, i, j, ..., value) {
  if (!missing(i) && is.matrix(i)) {
    if (!missing(j)) {
      warning(
        "indices for time periods do nothing when subscripting with a matrix"
      )
    }
    x <- replace_matrix(x, i, value)
  } else if (is_index(value)) {
    x <- replace_index(x, i, j, value)
  } else {
    x <- replace_numeric(x, i, j, value)
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
