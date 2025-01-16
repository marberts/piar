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
  ts(do.call(rbind, x$index), ..., names = x$levels)
}
