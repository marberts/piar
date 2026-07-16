#' Get the levels for an aggregation structure
#'
#' Get the hierarchical list of levels for an aggregation structure. It is
#' an error to try and replace these values.
#'
#' @family aggregation structure methods
#' @export
#'
#' @param x `[piar_aggregation_structure]` A price index aggregation structure,
#'   as made by [aggregation_structure()].
#'
#' @returns
#' A list of character vectors giving the levels for each position in the
#' aggregation structure.
levels.piar_aggregation_structure <- function(x) {
  x$levels
}

#' @export
`levels<-.piar_aggregation_structure` <- function(x, value) {
  stop("cannot replace levels attribute for an aggregation structure")
}
