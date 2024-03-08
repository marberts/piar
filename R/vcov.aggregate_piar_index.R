#' Bootstrap variance for a price index with replicate weights
#'
#' Estimate the sampling variance for an aggregate price index when using a
#' sample of elemental aggregates.
#'
#' This function is a simple wrapper that reaggregates the elemental indexes in
#' `object` using the bootstrap replicate weights in `repweights` to
#' get a collection of aggregate indexes from which the variance is calculated.
#'
#' This approach is usually applicable when elemental aggregates are sampled
#' with a stratified design that follows the aggregation structure, so that
#' there is no correlation between the index values for different levels of the
#' index. It ignores any variation from the elemental indexes (which often use
#' judgmental sampling), and ultimately depends on the method of generating
#' replicate weights. (Chapters 3 and 4 of Selvanathan and Rao (1994),
#' especially section 4.7, provide analytic variance estimators for some common
#' price indexes that are applicable with simple random sampling.)
#'
#' Note that any missing elemental indexes need to be explicitly imputed prior
#' to using this method, otherwise they will propagate throughout the variance
#' calculation.
#'
#' @param object An aggregate price index, as made by
#' [`aggregate()`][aggregate.piar_index].
#' @param repweights A matrix, or something that can be coerced into one, of
#' bootstrap replicate weights with a row for each elemental aggregate and a
#' column for each replicate.
#' @param mse Should variance be centered off the value of the index in
#' `object` (the default), or the mean of the replicates?
#' @param sparse Use sparse matrices from \pkg{Matrix} when aggregating the
#' index. Faster for indexes with large aggregation structures. The default
#' uses regular dense matrices.
#' @param ... Not currently used.
#'
#' @returns
#' A matrix of variances with a row for each upper-level index and a
#' column for each time period.
#'
#' @seealso
#' The `sps_repweights()` function in the \pkg{sps} package to
#' generate replicates weights when elemental aggregates are sampled using
#' sequential Poisson sampling.
#'
#' @references
#' Selvanathan, E. A., and Rao, D. S. P. (1994).
#' *Index Numbers: A Stochastic Approach*. MacMillan.
#'
#' @source
#' The `vcov()` method was influenced by a SAS routine by Justin
#' Francis that was first ported to R by Ambuj Dewan, and subsequently
#' rewritten by Steve Martin.
#'
#' @examples
#' prices <- data.frame(
#'   rel = 1:8,
#'   period = rep(1:2, each = 4),
#'   ea = rep(letters[1:2], 4)
#' )
#'
#' # A two-level aggregation structure
#'
#' pias <- aggregation_structure(
#'   list(c("top", "top", "top"), c("a", "b", "c")), 1:3
#' )
#'
#' repweights <- matrix(c(0, 2, 3, 1, 2, 4, 2, 3, 3), 3)
#'
#' # Calculate Jevons elemental indexes
#'
#' elemental <- with(prices, elemental_index(rel, period, ea))
#'
#' # Aggregate
#'
#' index <- aggregate(elemental, pias, na.rm = TRUE)
#'
#' # Calculate variance
#'
#' vcov(index, repweights)
#'
#' @family index methods
#' @importFrom stats vcov
#' @export
vcov.aggregate_piar_index <- function(object, repweights, ...,
                                      mse = TRUE, sparse = FALSE) {
  repweights <- as.matrix(repweights)
  eas <- object$pias$levels[[length(object$pias$levels)]]
  if (nrow(repweights) != length(eas)) {
    stop("'repweights' must have a row for each weight in 'pias'")
  }
  upper <- setdiff(object$levels, eas)
  n <- ncol(repweights)
  r <- object$r
  # template aggregation structure with no weights for each bootstrap replicate
  pias <- as_aggregation_structure(object)
  # matrix aggregation is much faster than aggregate(), but needs to be
  # done with a chained index
  elem <- as.matrix(chain(object[eas, ]))
  repindex <- lapply(seq_len(n), function(i) {
    weights(pias) <- repweights[, i]
    res <- (as.matrix(pias, sparse = sparse) %*% elem^r)^(1 / r)
    # undo chaining for a period-over-period index
    if (is_chainable_index(object)) {
      res[, -1] <- res[, -1] / res[, -ncol(res)]
    }
    # use regular matrices, not Matrix ones
    as.matrix(res)
  })
  # it's easier to calculate the variance with an array of indexes
  dimnm <- list(upper, object$time, seq_len(n))
  repindex <- array(
    unlist(repindex, use.names = FALSE),
    dim = lengths(dimnm), dimnames = dimnm
  )
  # mse = TRUE is the default for variance estimation in SAS,
  # but not the survey package
  if (mse) {
    centre <- as.matrix(object)[upper, , drop = FALSE]
  } else {
    centre <- apply(repindex, 2L, rowMeans)
  }
  apply(sweep(repindex, 1:2, centre), 1:2, crossprod) / n
}
