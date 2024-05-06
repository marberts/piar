#' Coerce an index into a tabular form
#'
#' Turn an index into a data frame or a matrix.
#'
#' @param x A price index, as made by, e.g., [elemental_index()].
#' @param stringsAsFactors See [as.data.frame()].
#' @param ... Not currently used.
#'
#' @returns
#' `as.data.frame()` returns the index values in `x` as a data frame with three
#' columns: `period`, `level`, and `value`.
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
as.data.frame.piar_index <- function(x, ..., stringsAsFactors = FALSE) {
  value <- unlist(x$index, use.names = FALSE)
  period <- rep(x$time, each = length(x$levels))
  if (stringsAsFactors) {
    data.frame(period = factor(period, x$time),
      level = factor(x$levels, x$levels), value
    )
  } else {
    data.frame(period,
      level = x$levels, value,
      stringsAsFactors = FALSE
    )
  }
}

#' @rdname as.data.frame.piar_index
#' @export
as.matrix.piar_index <- function(x, ...) {
  res <- do.call(cbind, x$index)
  dimnames(res) <- list(x$levels, x$time)
  res
}

#' @export
as.double.piar_index <- function(x, ...) {
  as.double(as.matrix(x))
}
