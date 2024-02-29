#' Coerce an index into a tabular form
#'
#' Turn an index into a data frame or a matrix.
#'
#' @param x A price index, as made by, e.g., [elemental_index()].
#' @param stringsAsFactors See [as.data.frame()].
#' @param ... Not used.
#'
#' @returns
#' `as.data.frame()` returns a data frame with three columns: `period`, `level`,
#' and `value`.
#'
#' `as.matrix()` returns a matrix with a row for each level and a column
#' for each period.
#'
#' @seealso
#' [as_index()] to coerce a matrix/data frame of index values into an index
#' object.
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
#' as.data.frame(epr)
#' as.matrix(epr)
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
