#' Update an aggregation structure
#'
#' Price update the weights in a price index aggregation structure.
#'
#' @importFrom stats update
#' @family aggregation structure methods
#' @export
#'
#' @param object `[piar_aggregation_structure]` A price index aggregation
#'   structure, as made by [aggregation_structure()].
#' @param index `[piar_index]` A fixed-base (direct) price index, or something
#'   that can be coerced into one. Usually an aggregate price index as made by
#'   [`aggregate()`][aggregate.piar_index].
#' @param period `[character(1)]` The time period used to price update the
#'   weights. The default uses the last period in `index`.
#' @param order `[numeric(1)]` Order of the generalized mean to update the
#'   weights. The default is 1 for an arithmetic index.
#' @param ... Not currently used.
#' @param r Deprecated.
#'
#' @returns
#' A copy of `object` with price-updated weights using the index
#' values in `index`.
#'
#' @seealso
#' [`aggregate()`][aggregate.piar_index] to make an aggregated price index.
#'
#' @examples
#' # A simple aggregation structure.
#' #            1
#' #      |-----+-----|
#' #      11          12
#' #  |---+---|       |
#' #  111     112     121
#' #  (1)     (3)     (4)
#' aggregation_weights <- data.frame(
#'   level1 = c("1", "1", "1"),
#'   level2 = c("11", "11", "12"),
#'   ea     = c("111", "112", "121"),
#'   weight = c(1, 3, 4)
#' )
#'
#' pias <- as_aggregation_structure(aggregation_weights)
#'
#' index <- as_index(
#'   matrix(1:9, 3, dimnames = list(c("111", "112", "121"), NULL))
#' )
#'
#' weights(pias, ea_only = FALSE)
#'
#' weights(update(pias, index), ea_only = FALSE)
update.piar_aggregation_structure <- function(
  object,
  index,
  ...,
  period = NULL,
  order = 1,
  r = order
) {
  if ("r" %in% names(sys.call())) {
    warning("`r` is deprecated and will be removed; use `order` instead")
  }
  chkDots(...)
  index <- as_index(index, chainable = FALSE)
  r <- as.numeric(r)
  period <- if (!is.null(period)) {
    match_time(as.character(period), index)
  } else {
    ntime(index)
  }
  eas <- match_eas(object, index)
  if (anyNA(eas)) {
    warning("not all weights in `object` have a corresponding index value")
  }
  weights(object) <- update_weights(
    index$index[, period][eas],
    object$weights,
    order = r
  )
  object
}
