#' Extract index values
#'
#' Methods to extract and replace index values like a matrix.
#'
#' The extraction method treats `x` like a matrix of index values with
#' (named) rows for each level and columns for each time period in
#' `x`. Unlike a matrix, dimensions are never dropped as subscripting
#' `x` always returns an index object. The one exception is when subscripting
#' with a matrix, in which case a list of index values is returned. As `x`
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
#' corresponding percent-change contributions (if any). When replacing with a
#' matrix, `value` can be a list of index objects.
#'
#' @param x A price index, as made by, e.g., [elementary_index()].
#' @param i,j Indices for the levels and time periods of a price index. See
#'   details.
#' @param value A numeric vector, price index, or list of price indexes.
#'   See details.
#' @param ... Not currently used.
#'
#' @returns
#' A price index that inherits from the same class as `x`.
#'
#' @examples
#' index <- as_index(matrix(1:6, 2))
#'
#' index["1", ]
#'
#' index[, 2]
#'
#' index[1, ] <- 1 # can be useful for doing specific imputations
#'
#' index
#'
#' @family index methods
#' @export
`[.piar_index` <- function(x, i, j, ...) {
  chkDots(...)
  if (!missing(i) && is.matrix(i)) {
    if (!missing(j)) {
      warning(
        "indices for time periods do nothing when subscripting with a matrix"
      )
    }
    i <- subscript_index_matrix(x, i)
    lapply(seq_len(nrow(i)), \(j) extract_index(x, i[j, 1L], i[j, 2L]))
  } else {
    levels <- subscript_index(x$levels, i)
    periods <- subscript_index(x$time, j)
    extract_index(x, levels, periods)
  }
}

#' Internal extraction function
#' @noRd
extract_index <- function(x, levels, periods) {
  x$index <- x$index[levels, periods, drop = FALSE]
  if (!is.null(x$contrib)) {
    x$contrib <- x$contrib[levels, periods, drop = FALSE]
  }
  x$levels <- x$levels[levels]
  x$time <- x$time[periods]
  validate_piar_index(x)
}

#' @rdname sub-.piar_index
#' @export
`[<-.piar_index` <- function(x, i, j, ..., value) {
  chkDots(...)
  if (!missing(i) && is.matrix(i)) {
    if (!missing(j)) {
      warning(
        "indices for time periods do nothing when subscripting with a matrix"
      )
    }
    i <- subscript_index_matrix(x, i)
    if (is_index(value)) {
      value <- list(value)
    }
    if (is.list(value)) {
      x <- replace_matrix_list(x, i, value)
    } else {
      x <- replace_matrix_numeric(x, i, value)
    }
  } else {
    levels <- subscript_index(x$levels, i)
    periods <- subscript_index(x$time, j)
    if (is_index(value)) {
      x <- replace_index(x, levels, periods, value)
    } else {
      x <- replace_numeric(x, levels, periods, value)
    }
  }
  # Replacement value should be validated; e.g., x[1] <- -1.
  validate_piar_index(x)
}

#' @export
`[<-.chainable_piar_index` <- function(x, i, j, ..., value) {
  if (is_index(value)) {
    if (!is_chainable_index(value)) {
      stop("'value' must be a period-over-period index")
    }
  } else if (is.list(value)) {
    if (!all(vapply(value, is_chainable_index, logical(1L)))) {
      stop("all elements of 'value' must be period-over-period indexes")
    }
  }
  NextMethod("[<-")
}

#' @export
`[<-.direct_piar_index` <- function(x, i, j, ..., value) {
  if (is_index(value)) {
    if (!is_direct_index(value)) {
      stop("'value' must be a fixed-base index")
    }
  } else if (is.list(value)) {
    if (!all(vapply(value, is_direct_index, logical(1L)))) {
      stop("all elements of 'value' must be fixed-base indexes")
    }
  }
  NextMethod("[<-")
}

#' Internal replacement functions
#' @noRd
replace_matrix_list <- function(x, i, value) {
  if (nrow(i) == 0L) {
    return(x)
  }
  n <- length(value)
  if (n == 0L) {
    stop("replacement has length zero")
  }
  if (nrow(i) %% n != 0) {
    warning(
      "number of items to replace is not a multiple of replacement length"
    )
  }
  if (any(vapply(value, \(x) ntime(x) * nlevels(x), integer(1L)) > 1L)) {
    stop("'value' must be a list of indexes with one level and time period")
  }
  value <- rep_len(value, nrow(i))
  index <- vapply(value, \(x) x$index, numeric(1L))
  contributions <- unlist(lapply(value, \(x) x$contrib), recursive = FALSE)
  has_contrib <- any(vapply(contributions, Negate(is.null), logical(1L)))
  # It's possible that only some elements of `value` have contributions.
  if (has_contrib || !is.null(x$contrib)) {
    contributions[lengths(contributions) == 0L] <- list(numeric(0L))
  }
  if (is.null(x$contrib) && has_contrib) {
    x$contrib <- contrib_skeleton(x$levels, x$time)
  }
  x$index[i] <- index
  if (!is.null(x$contrib)) {
    x$contrib[i] <- contributions
  }
  x
}

replace_matrix_numeric <- function(x, i, value) {
  x$index[i] <- as.numeric(value)
  if (!is.null(x$contrib)) {
    x$contrib[i] <- list(numeric(0L))
  }
  x
}

replace_index <- function(x, levels, periods, value) {
  if (length(levels) == 0L || length(periods) == 0L) {
    return(x)
  }

  if (ntime(value) != length(periods)) {
    stop("'x' and 'value' must have the same number of time periods")
  }
  if (length(levels) %% nlevels(value) != 0) {
    stop("number of items to replace is not a multiple of replacement length")
  }
  if (is.null(x$contrib) && !is.null(value$contrib)) {
    x$contrib <- contrib_skeleton(x$levels, x$time)
  } else if (!is.null(x$contrib) && is.null(value$contrib)) {
    value$contrib <- contrib_skeleton(value$levels, value$time)
  }
  for (t in seq_along(periods)) {
    x$index[levels, periods[t]] <- value$index[, t]
    if (!is.null(x$contrib)) {
      x$contrib[levels, periods[t]] <- value$contrib[, t]
    }
  }
  x
}

replace_numeric <- function(x, levels, periods, value) {
  x$index[levels, periods] <- as.numeric(value)
  if (!is.null(x$contrib)) {
    x$contrib[levels, periods] <- list(numeric(0L))
  }
  x
}
