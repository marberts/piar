#' Get the weights for an aggregation structure
#'
#' Get and set the weights for a price index aggregation structure.
#'
#' @param object A price index aggregation structure, as made by
#'   [aggregation_structure()].
#' @param ea_only Should weights be returned for only the elemental aggregates
#'   (the default)? Setting to `FALSE` gives the weights for the entire
#'   aggregation structure.
#' @param na.rm Should missing values be removed when aggregating the
#'   weights (i.e., when `ea_only = FALSE`)? By default, missing values are
#'   not removed.
#' @param value A numeric vector of weights for the elemental aggregates of
#'   `object`.
#' @param ... Not currently used.
#'
#' @returns
#' `weights()` returns a named vector of weights for the elemental aggregates.
#' The replacement method replaces these values without changing the
#' aggregation structure. (`set_weights()` is an alias that's easier to use with
#' pipes.)
#'
#' If `ea_only = FALSE` then the return value is a list
#' with a named vector of weights for each level in the aggregation structure.
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
#' # Extract the weights
#'
#' weights(pias)
#'
#' # ... or update them
#'
#' weights(pias) <- 1:3
#' weights(pias)
#'
#' @importFrom stats weights
#' @family aggregation structure methods
#' @export
weights.piar_aggregation_structure <- function(object,
                                               ...,
                                               ea_only = TRUE,
                                               na.rm = FALSE) {
  chkDots(...)
  names(object$weights) <- object$levels[[length(object$levels)]]
  if (ea_only) {
    return(object$weights)
  }
  res <- vector("list", length(object$levels))
  names(res) <- rev(names(object$levels))
  res[[1L]] <- object$weights

  for (i in seq_along(res)[-1L]) {
    res[[i]] <- vapply(
      object$child[[i - 1L]],
      \(z) sum(res[[i - 1L]][z], na.rm = na.rm),
      numeric(1L)
    )
  }
  rev(res)
}

#' @rdname weights.piar_aggregation_structure
#' @export
`weights<-` <- function(object, value) {
  UseMethod("weights<-")
}

#' @rdname weights.piar_aggregation_structure
#' @export
`weights<-.piar_aggregation_structure` <- function(object, value) {
  object$weights[] <- as.numeric(value)
  object
}

#' @rdname weights.piar_aggregation_structure
#' @export
set_weights <- `weights<-`
