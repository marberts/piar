aggregate_contrib <- function(r) {
  arithmetic_weights <- gpindex::transmute_weights(r, 1)
  function(x, rel, w) {
    w <- gpindex::scale_weights(arithmetic_weights(rel, w))
    res <- unlist(Map("*", x, w))
    names(res) <- make.unique(as.character(names(res)))
    res
  }
}

#' Aggregate elemental price indexes
#'
#' Aggregate elemental price indexes with a price index aggregation structure.
#'
#' The `aggregate()` method loops over each time period in `x` and
#' 1. aggregates the elemental indexes with
#' [`gpindex::generalized_mean(r)()`][gpindex::generalized_mean] for each level
#' of `pias`;
#' 2. aggregates percent-change contributions for each level of
#' `pias` (if there are any and `contrib = TRUE`);
#' 3. price updates the weights in `pias` with
#' [`gpindex::factor_weights(r)()`][gpindex::factor_weights] (only for
#' period-over-period elemental indexes).
#'
#' The result is a collection of aggregated period-over-period indexes that
#' can be chained together to get a fixed-base index when `x` are
#' period-over-period elemental indexes. Otherwise, when `x` are fixed-base
#' elemental indexes, the result is a collection of aggregated fixed-base
#' (direct) indexes.
#'
#' By default, missing elemental indexes will propagate when aggregating the
#' index. Missing elemental indexes can be due to both missingness of these
#' values in `x`, and the presence of elemental aggregates in `pias`
#' that are not part of `x`. Setting `na.rm = TRUE` ignores missing
#' values, and is equivalent to parental (or overall mean) imputation. As an
#' aggregated price index generally cannot have missing values (for otherwise
#' it can't be chained over time and weights can't be price updated), any
#' missing values for a level of `pias` are removed and recursively replaced
#' by the value of its immediate parent.
#'
#' In most cases aggregation is done with an arithmetic mean (the default), and
#' this is detailed in chapter 8 (pp. 190--198) of the CPI manual (2020).
#' Aggregating with a non-arithmetic mean follows the same steps, except that
#' the elemental indexes are aggregated with a mean of a different order (e.g.,
#' harmonic for a Paasche index), and the method for price updating the weights
#' is slightly different. Note that, because aggregation is done with a
#' generalized mean, the resulting index is consistent-in-aggregation at each
#' point in time.
#'
#' Aggregating percent-change contributions uses the method in chapter 9 of the
#' CPI manual (equations 9.26 and 9.28) when aggregating with an arithmetic
#' mean. With a non-arithmetic mean, arithmetic weights are constructed using
#' [`gpindex::transmute_weights(r, 1)()`][gpindex::transmute_weights] in order
#' to apply this method.
#'
#' There may not be contributions for all prices relatives in an elemental
#' aggregate if the elemental indexes are built from several sources (as with
#' [`merge()`][merge.piar_index]). In this case the contribution for
#' a price relative in the aggregated index will be correct, but the sum of all
#' contributions will not equal the change in the value of the index. This can
#' also happen when aggregating an already aggregated index in which missing
#' index values have been imputed (i.e., when `na.rm = TRUE`).
#'
#' @name aggregate.piar_index
#' @aliases aggregate.piar_index
#' @param x A price index, usually made by [elemental_index()].
#' @param pias A price index aggregation structure or something that can be
#' coerced into one. This can be made with [aggregation_structure()].
#' @param na.rm Should missing values be removed? By default, missing values
#' are not removed. Setting `na.rm = TRUE` is equivalent to overall mean
#' imputation.
#' @param r Order of the generalized mean to aggregate index values. 0 for a
#' geometric index (the default for making elemental indexes), 1 for an
#' arithmetic index (the default for aggregating elemental indexes and
#' averaging indexes over subperiods), or -1 for a harmonic index (usually for
#' a Paasche index). Other values are possible; see
#' [gpindex::generalized_mean()] for details.
#' @param contrib Aggregate percent-change contributions in `x` (if any)?
#' @param ... Further arguments passed to or used by methods.
#'
#' @returns
#' An aggregate price index that inherits from [`aggregate_piar_index`] and
#' the class of `x`.
#'
#' @note
#' For large indexes it can be much faster to turn the aggregation structure
#' into an aggregation matrix with
#' [`as.matrix()`][as.matrix.piar_aggregation_structure], then aggregate
#' elemental indexes as a matrix operation when there are no missing
#' values---see the examples for details.
#'
#' @references
#' Balk, B. M. (2008). *Price and Quantity Index Numbers*.
#' Cambridge University Press.
#'
#' ILO, IMF, OECD, Eurostat, UN, and World Bank. (2020).
#' *Consumer Price Index Manual: Theory and Practice*.
#' International Monetary Fund.
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
#' # Calculate Jevons elemental indexes
#'
#' (epr <- with(prices, elemental_index(rel, period, ea)))
#'
#' # Aggregate (note the imputation for elemental index 'c')
#'
#' (index <- aggregate(epr, pias, na.rm = TRUE))
#'
#' # Aggregation can equivalently be done as matrix multiplication
#'
#' as.matrix(pias) %*% as.matrix(chain(index[letters[1:3]]))
#'
#' @importFrom stats aggregate
#' @family index methods
#' @export
aggregate.chainable_piar_index <- function(x, pias, na.rm = FALSE, r = 1,
                                           contrib = TRUE, ...) {
  NextMethod("aggregate", chainable = TRUE)
}

#' @rdname aggregate.piar_index
#' @export
aggregate.direct_piar_index <- function(x, pias, na.rm = FALSE, r = 1,
                                        contrib = TRUE, ...) {
  NextMethod("aggregate", chainable = FALSE)
}

#' @export
aggregate.piar_index <- function(x, pias, na.rm = FALSE, r = 1, contrib = TRUE,
                                 ..., chainable) {
  pias <- as_aggregation_structure(pias)
  r <- as.numeric(r)
  
  # helpful functions
  price_update <- gpindex::factor_weights(r)
  gen_mean <- gpindex::generalized_mean(r)
  agg_contrib <- aggregate_contrib(r)
  
  # put the aggregation weights upside down to line up with pias
  w <- rev(weights(pias, ea_only = FALSE, na.rm = na.rm))
  
  has_contrib <- has_contrib(x) && contrib
  pias_eas <- match(pias$eas, pias$levels)
  eas <- match(pias$eas, x$levels)
  
  # loop over each time period
  index <- contrib <- vector("list", length(x$time))
  for (t in seq_along(x$time)) {
    rel <- con <- vector("list", pias$height)
    # align epr with weights so that positional indexing works
    rel[[1L]] <- x$index[[t]][eas]
    con[[1L]] <- x$contrib[[t]][eas]
    
    # get rid of any NULL contributions
    con[[1L]][lengths(con[[1L]]) == 0L] <- list(numeric(0L))
    for (i in which(is.na(rel[[1L]]))) {
      con[[1L]][[i]][] <- NA
    }
    
    # loop over each level in pias from the bottom up and aggregate
    for (i in seq_along(rel)[-1L]) {
      nodes <- unname(pias$child[[i - 1L]])
      rel[[i]] <- vapply(
        nodes,
        \(z) gen_mean(rel[[i - 1L]][z], w[[i - 1L]][z], na.rm = na.rm),
        numeric(1L)
      )
      if (has_contrib) {
        con[[i]] <- lapply(
          nodes,
          \(z) agg_contrib(con[[i - 1L]][z], rel[[i - 1L]][z], w[[i - 1L]][z])
        )
      } else {
        con[i] <- empty_contrib(nodes)
      }
    }
    
    # parental imputation
    if (na.rm) {
      for (i in rev(seq_along(rel))[-1L]) {
        impute <- which(is.na(rel[[i]]))
        rel[[i]][impute] <- rel[[i + 1L]][pias$parent[[i]][impute]]
      }
    }
    
    # return index and contributions
    index[[t]] <- unlist(rev(rel), use.names = FALSE)
    contrib[[t]] <- unlist(rev(con), recursive = FALSE, use.names = FALSE)
    # price update weights for all periods after the first
    if (chainable) {
      weights(pias) <- price_update(index[[t]][pias_eas], w[[1L]])
      w <- rev(weights(pias, ea_only = FALSE, na.rm = na.rm))
    }
  }
  
  aggregate_piar_index(
    index, contrib, pias$levels, x$time, r,
    pias[c("child", "parent", "eas", "height")],
    chainable
  )
}
