#' Coerce an index into a tabular form
#'
#' Turn an index into a data frame or a matrix.
#'
#' @param x A price index, as made by, e.g., [elemental_index()].
#' @param row.names,stringsAsFactors See [as.data.frame()].
#' @param optional Not currently used.
#' @param ... Not currently used.
#' @param contrib Include percent-change contributions (the default does not
#'   include them).
#'
#' @returns
#' `as.data.frame()` returns the index values in `x` as a data frame with three
#' columns: `period`, `level`, and `value`. If `contrib = TRUE` then there is
#' a fourth (list) column `contrib` containing percent-change contributions.
#'
#' `as.matrix()` returns the index values in `x` as a matrix with a row for
#' each level and a column for each time period in `x`.
#'
#' @seealso
#' [as_index()] to coerce a matrix/data frame of index values into an index
#' object.
#'
#' @examples
#' index <- as_index(matrix(1:6, 2))
#'
#' as.data.frame(index)
#'
#' as.matrix(index)
#'
#' @family index methods
#' @export
as.data.frame.piar_index <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  contrib = FALSE,
  stringsAsFactors = FALSE
) {
  chkDots(...)
  value <- unlist(x$index, use.names = FALSE)
  period <- rep(x$time, each = nlevels(x))
  if (stringsAsFactors) {
    res <- data.frame(
      period = factor(period, x$time),
      level = factor(x$levels, x$levels),
      value,
      row.names = row.names
    )
  } else {
    res <- data.frame(
      period,
      level = x$levels,
      value,
      row.names = row.names,
      stringsAsFactors = FALSE
    )
  }
  if (contrib) {
    res$contrib <- unlist(x$contrib, use.names = FALSE, recursive = FALSE)
  }
  res
}

#' @rdname as.data.frame.piar_index
#' @export
as.matrix.piar_index <- function(x, ...) {
  chkDots(...)
  res <- do.call(cbind, x$index)
  dimnames(res) <- list(levels = x$levels, time = x$time)
  res
}

#' @export
as.double.piar_index <- function(x, ...) {
  chkDots(...)
  as.double(as.matrix(x))
}

#' Coerce an index into a time series
#'
#' Turn an index into a regular time series, represented as a [`ts`] object.
#'
#' @param x A price index, as made by, e.g., [elemental_index()].
#' @param ... Additional arguments passed to [`ts()`].
#'
#' @returns A time series object.
#' @examples
#' as.ts(as_index(matrix(1:9, 3)))
#'
#' @importFrom stats as.ts ts
#' @family index methods
#' @export
as.ts.piar_index <- function(x, ...) {
  ts(data = do.call(rbind, x$index), ..., names = x$levels)
}
