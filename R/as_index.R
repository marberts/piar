#' Coerce to a price index
#'
#' Coerce pre-computed index values into an index object.
#'
#' Numeric matrices are coerced into an index object by treating each column as
#' a separate time period, and each row as a separate level of the index (e.g.,
#' an elemental aggregate). Column names
#' are used to denote time periods, and row names are used to denote levels
#' (so they must be unique). This essentially reverses calling
#' [`as.matrix()`][as.matrix.piar_index] on an index object. If a
#' dimension is unnamed, then it is given a sequential label from 1 to the size
#' of that dimension. The default method coerces `x` to a matrix prior to
#' using the matrix method.
#'
#' The data frame method for `as_index()` is best understood as reversing
#' the effect of [`as.data.frame()`][as.data.frame.piar_index] on an
#' index object. It constructs a matrix by taking the levels of
#' `x[[1]]` as columns and the levels of `x[[2]]` as rows
#' (coercing to a factor if necessary). It then populates this matrix with the
#' corresponding values in `x[[3]]`, and uses the matrix method for
#' `as_index()`.
#'
#' If `x` is a period-over-period index then it is returned unchanged when
#' `chainable = TRUE` and chained otherwise. Similarly, if `x` is a
#' fixed-base index then it is returned unchanged when
#' `chainable = FALSE` and unchain otherwise.
#'
#' @param x An object to coerce into a price index.
#' @param chainable Are the index values in `x` period-over-period
#' indexes, suitable for a chained calculation (the default)? This should be
#' `FALSE` when `x` is a fixed-base (direct) index.
#' @param ... Further arguments passed to or used by methods.
#'
#' @returns
#' `as_index()` returns a price index that inherits from
#' [`piar_index`]. If `chainable = TRUE` then this is a
#' period-over-period price index that also inherits from
#' [`chainable_piar_index`]; otherwise, it is a fixed-base index that
#' inherits from [`direct_piar_index`].
#'
#' @seealso
#' [`as.matrix()`][as.matrix.piar_index] and
#' [`as.data.frame()`][as.data.frame.piar_index] for coercing an index
#' into a tabular form.
#'
#' @examples
#' prices <- data.frame(
#'   rel = 1:8,
#'   period = rep(1:2, each = 4),
#'   ea = rep(letters[1:2], 4)
#' )
#'
#' index <- with(prices, elemental_index(rel, period, ea))
#'
#' all.equal(as_index(as.data.frame(index)), index)
#' all.equal(as_index(as.matrix(index)), index)
#'
#' @export
as_index <- function(x, ...) {
  UseMethod("as_index")
}

#' @rdname as_index
#' @export
as_index.default <- function(x, ..., chainable = TRUE) {
  as_index(as.matrix(x), ..., chainable = chainable)
}

#' @rdname as_index
#' @export
as_index.matrix <- function(x, ..., chainable = TRUE) {
  storage.mode(x) <- "numeric"
  levels <- if (is.null(rownames(x))) seq_len(nrow(x)) else rownames(x)
  periods <- if (is.null(colnames(x))) seq_len(ncol(x)) else colnames(x)
  if (any(x <= 0, na.rm = TRUE)) {
    warning("some elements of 'x' are less than or equal to 0")
  }

  index <- index_skeleton(levels, periods)
  for (t in seq_along(periods)) {
    index[[t]][] <- x[, t]
  }
  contrib <- contrib_skeleton(levels, periods)
  piar_index(index, contrib, levels, periods, chainable)
}

#' @rdname as_index
#' @export
as_index.data.frame <- function(x, ..., chainable = TRUE) {
  if (length(x) != 3L) {
    stop(
      "'x' must have a column of time periods, index levels, and index values"
    )
  }
  x[1:2] <- lapply(x[1:2], as.factor)
  time <- levels(x[[1L]])
  levels <- levels(x[[2L]])
  # elemental_index() usually gives NaN for missing cells.
  res <- matrix(NA_real_,
    nrow = length(levels), ncol = length(time),
    dimnames = list(levels, time)
  )
  res[as.matrix(x[2:1])] <- as.numeric(x[[3L]])
  as_index(res, ..., chainable = chainable)
}

#' @rdname as_index
#' @export
as_index.chainable_piar_index <- function(x, ..., chainable = TRUE) {
  if (chainable) x else chain(x)
}

#' @rdname as_index
#' @export
as_index.direct_piar_index <- function(x, ..., chainable = FALSE) {
  if (chainable) unchain(x) else x
}
