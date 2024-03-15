#' Update an aggregation structure
#'
#' Price update the weights in a price index aggregation structure.
#'
#' @param object A price index aggregation structure, as made by
#' [aggregation_structure()].
#' @param index A price index, or something that can be coerced into one.
#' Usually an aggregate price index as made by
#' [`aggregate()`][aggregate.piar_index].
#' @param period The time period used to price update the weights. The default
#' uses the last period in `index`.
#' @param r Order of the generalized mean to update the weights. The default
#' uses the order used to aggregate `index` if it's an aggregate index;
#' otherwise, the default is 1 for an arithmetic index.
#' @param ... Not currently used.
#'
#' @returns
#' A copy of `object` with price-updated weights using the index
#' values in `index`.
#'
#' @seealso
#' [`aggregate()`][aggregate.piar_index] to make an aggregated price index.
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
#' pias <- as_aggregation_structure(aggregation_weights)
#'
#' index <- as_index(
#'   matrix(1:9, 3, dimnames = list(c("111", "112", "121"), NULL))
#' )
#'
#' weights(pias, ea_only = FALSE)
#'
#' weights(update(pias, index), ea_only = FALSE)
#'
#' @importFrom stats update
#' @family aggregation structure methods
#' @export
update.piar_aggregation_structure <- function(object, index,
                                              period = end(index), ...,
                                              r = 1) {
  index <- as_index(index)
  period <- match.arg(as.character(period), index$time)
  price_update <- gpindex::factor_weights(r)
  eas <- match(object$levels[[length(object$levels)]], index$levels)
  if (anyNA(eas)) {
    warning("not all weights in 'object' have a corresponding index value")
  }
  epr <- chain(index)$index[[match(period, index$time)]]
  weights(object) <- price_update(epr[eas], object$weights)
  object
}
