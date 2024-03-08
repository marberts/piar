#' @export
levels.piar_aggregation_structure <- function(x) {
  x$levels
}

#' @export
`levels<-.piar_aggregation_structure` <- function(x, value) {
  stop("cannot replace levels attribute for an aggregation structure")
}
