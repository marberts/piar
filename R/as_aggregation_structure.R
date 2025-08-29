#' Coerce to an aggregation structure
#'
#' Coerce an object into an aggregation structure object.
#'
#' The default method attempts to coerce `x` into a list prior to calling
#' [aggregation_structure()].
#'
#' The data frame and matrix methods treat `x` as a table with a row for
#' each elemental aggregate, a column of labels for each level in the
#' aggregation structure, and a column of weights for the elemental aggregates.
#'
#' @param x An object to coerce into an aggregation structure.
#' @param weights A numeric vector of aggregation weights for the elemental
#'   aggregates. The default is to give each elemental aggregate the same
#'   weight.
#' @param ... Further arguments passed to or used by methods.
#'
#' @returns
#' A price index aggregation structure that inherits from
#' [`piar_aggregation_structure`].
#'
#' @seealso
#' [`as.matrix()`][as.matrix.piar_aggregation_structure] and
#' [`as.data.frame()`][as.data.frame.piar_aggregation_structure] for
#' coercing an aggregation structure into a tabular form.
#'
#' @examples
#' # A simple aggregation structure
#' #            1
#' #      |-----+-----|
#' #      11          12
#' #  |---+---|       |
#' #  111     112     121
#' #  (1)     (3)     (4)
#'
#' aggregation_weights <- data.frame(
#'   level1 = c("1", "1", "1"),
#'   level2 = c("11", "11", "12"),
#'   ea     = c("111", "112", "121"),
#'   weight = c(1, 3, 4)
#' )
#'
#' pias <- aggregation_structure(
#'   aggregation_weights[1:3],
#'   weights = aggregation_weights[[4]]
#' )
#'
#' all.equal(
#'   pias,
#'   as_aggregation_structure(aggregation_weights)
#' )
#'
#' all.equal(
#'   pias,
#'   as_aggregation_structure(as.matrix(aggregation_weights))
#' )
#'
#' @export
as_aggregation_structure <- function(x, ...) {
  UseMethod("as_aggregation_structure")
}

#' @rdname as_aggregation_structure
#' @export
as_aggregation_structure.default <- function(x, ..., weights = NULL) {
  chkDots(...)
  aggregation_structure(as.list(x), weights)
}

#' @rdname as_aggregation_structure
#' @export
as_aggregation_structure.data.frame <- function(x, ...) {
  chkDots(...)
  x <- as.list(x)
  aggregation_structure(drop_last(x), last(x))
}

#' @rdname as_aggregation_structure
#' @export
as_aggregation_structure.matrix <- function(x, ...) {
  as_aggregation_structure(as.data.frame(x), ...)
}

#' @export
as_aggregation_structure.piar_aggregation_structure <- function(x, ...) {
  chkDots(...)
  x
}
