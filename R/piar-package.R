

#' Methods to get the weights and levels for a price index aggregation
#' structure
#' 
#' Get and set the weights for a price index aggregation structure, or get the
#' levels.
#' 
#' 
#' @aliases weights.piar_aggregation_structure weights<-
#' weights<-.piar_aggregation_structure levels.piar_aggregation_structure
#' @param object,x A price index aggregation structure, as made by
#' [`aggregation_structure()`][aggregation_structure].
#' @param ea_only Should weights be returned for only the elemental aggregates?
#' The default gives the weights for the entire aggregation structure.
#' @param na.rm Should missing values be removed from when aggregating the
#' weights (i.e., when `ea_only = FALSE`)? By default, missing values are
#' not removed.
#' @param value A numeric vector of weights for the elemental aggregates of
#' `object`.
#' @param ... Further arguments passed to or used by methods.
#' @return `weights()` returns a list with a named vector of weights for
#' each level in the aggregation structure. If `ea_only = TRUE` then the
#' return value is just a named vector of weights for the elemental aggregates.
#' The replacement method replaces these values without changing the
#' aggregation structure.
#' 
#' `levels()` returns a character vector with the levels for the
#' aggregation structure.
#' @examples
#' 
#' # A simple aggregation structure
#' #            1
#' #      |-----+-----|
#' #      11          12
#' #  |---+---|       |
#' #  111     112     121
#' #  (1)     (3)     (4)
#' 
#' aggregation_weights <- data.frame(
#'   level1 = c("1",   "1",   "1"),
#'   level2 = c("11",  "11",  "12"),
#'   ea     = c("111", "112", "121"),
#'   weight = c(1,     3,     4)
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
NULL





#' Coerce a price index aggregation structure into a tabular form
#' 
#' Coerce a price index aggregation structure into an aggregation matrix, or a
#' data frame.
#' 
#' 
#' @aliases as.matrix.piar_aggregation_structure
#' as.data.frame.piar_aggregation_structure
#' @param x A price index aggregation structure, as made by
#' [`aggregation_structure()`][aggregation_structure].
#' @param stringsAsFactors See [as.data.frame()].
#' @param ... Further arguments passed to or used by methods.
#' @return `as.matrix()` represents an aggregation structure as a matrix,
#' such that multiplying with a (column) vector of elemental indexes gives the
#' aggregated index.
#' 
#' `as.data.frame()` takes an aggregation structure and returns a data
#' frame that could have generated it, with columns `level1`,
#' `level2`, ..., `ea`, and `weight`.
#' @seealso [as_aggregation_structure()] for coercing into an
#' aggregation structure.
#' @examples
#' 
#' # A simple aggregation structure
#' #            1
#' #      |-----+-----|
#' #      11          12
#' #  |---+---|       |
#' #  111     112     121
#' #  (1)     (3)     (4)
#' 
#' aggregation_weights <- data.frame(
#'   level1 = c("1",   "1",   "1"),
#'   level2 = c("11",  "11",  "12"),
#'   ea     = c("111", "112", "121"),
#'   weight = c(1,     3,     4)
#' )
#' 
#' pias <- as_aggregation_structure(aggregation_weights)
#' 
#' as.matrix(pias)
#' 
#' all.equal(as.data.frame(pias), aggregation_weights)
#' 
NULL





#' Coerce an index into a tabular form
#' 
#' Turn an index into a matrix with a row for each level and a column for each
#' period, or a data frame with three columns: period, level, and value.
#' 
#' 
#' @aliases as.matrix.piar_index as.data.frame.piar_index
#' @param x A price index, as made by, e.g.,
#' [`elemental_index()`][elemental_index].
#' @param stringsAsFactors See [as.data.frame()].
#' @param ... Further arguments passed to or used by methods.
#' @return `as.data.frame()` returns a data frame.
#' 
#' `as.matrix()` returns a matrix.
#' @seealso [as_index()] to coerce a matrix/data frame of index
#' values into an index object.
#' @examples
#' 
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
NULL





#' Extract and replace index values
#' 
#' Methods to extract and replace index values like a matrix.
#' 
#' The extraction methods treat `x` as a matrix of index values with
#' (named) rows for each `level` and columns for each `period` in
#' `x`. Unlike a matrix, dimensions are never dropped as subscripting
#' `x` always returns an index object. This means that subscripting with a
#' matrix is not possible, and only a submatrix can be extracted. As `x`
#' is not an atomic vector, subscripting with a single index like `x[1]`
#' extracts all time periods for that level.
#' 
#' The replacement methods similarly treat `x` as a matrix, and behave the
#' same as replacing values in a matrix (except that `value` is coerced to
#' numeric). Note that replacing the values of an index will remove the
#' corresponding percent-change contributions (if any).
#' 
#' Subscripting an aggregate index cannot generally preserve the aggregation
#' structure if any levels are removed or rearranged, and in this case the
#' resulting index is *not* an aggregate index. Similarly, replacing the
#' values for an aggregate index generally breaks consistency in aggregation,
#' and therefore the result is *not* an aggregate index.
#' 
#' @aliases [.aggregate_piar_index [.piar_index [<-.aggregate_piar_index
#' [<-.piar_index
#' @param x A price index, as made by, e.g.,
#' [`elemental_index()`][elemental_index].
#' @param i,j Indices for the levels and time periods of a price index. See
#' details.
#' @param value A numeric vector.
#' @param ... Ignored.
#' @return A price index that inherits from [chainable_piar_index()]
#' if `x` is a period-over-period index, or
#' [direct_piar_index()] if `x` is a fixed-base index. If
#' `x` inherits from [aggregate_piar_index()] then ``[``
#' returns an aggregate index if the levels are unchanged.
#' @examples
#' 
#' prices <- data.frame(
#'   rel = 1:8,
#'   period = rep(1:2, each = 4),
#'   ea = rep(letters[1:2], 4)
#' )
#' 
#' # Calculate Jevons elemental indexes
#' 
#' epr <- with(prices, elemental_index(rel, period, ea))
#' 
#' # Extract the indexes like a matrix
#' 
#' epr["a", ]
#' 
#' epr[, 2]
#' 
#' epr[1, ] <- 1 # can be useful for doing specific imputations
#' epr
#' 
NULL





#' Impute missing prices
#' 
#' Impute missing prices using the carry forward or shadow price method.
#' 
#' The carry forward method replaces a missing price for a product by the price
#' for the same product in the previous period. It tends to push an index value
#' towards 1, and is usually avoided; see paragraph 6.61 in the CPI manual
#' (2020).
#' 
#' The shadow price method recursively imputes a missing price by the value of
#' the price for the same product in the previous period multiplied by the
#' value of the period-over-period elemental index for the elemental aggregate
#' to which that product belongs. This requires computing and aggregating an
#' index (according to `pias`, unless `pias` is not supplied) for
#' each `period`, and so these imputations can take a while. The index
#' values used to do the imputations are not returned because the index needs
#' to be recalculated to get correct percent-change contributions.
#' 
#' Shadow price imputation is referred to as self-correcting overall mean
#' imputation in chapter 6 of the CPI manual (2020). It is identical to simply
#' excluding missing price relatives in the index calculation, except in the
#' period that a missing product returns. For this reason care is needed when
#' using this method. It is sensitive to the assumption that a product does not
#' change over time, and in some cases it is safer to simply omit the missing
#' price relatives instead of imputing the missing prices.
#' 
#' @aliases carry_forward shadow_price
#' @param x A numeric vector of prices.
#' @param period A factor, or something that can be coerced into one, giving
#' the time period associated with each price in `x`. The ordering of time
#' periods follows of the levels of `period`, to agree with
#' [`cut()`][cut.Date].
#' @param product A factor, or something that can be coerced into one, giving
#' the product associated with each price in `x`.
#' @param ea A factor, or something that can be coerced into one, giving the
#' elemental aggregate associated with each price in `x`.
#' @param pias A price index aggregation structure, or something that can be
#' coerced into one, as made with
#' [`aggregation_structure()`][aggregation_structure]. The default
#' imputes from elemental indexes only (i.e., not recursively).
#' @param w A numeric vector of weights for the prices in `x` (i.e.,
#' product weights). The default is to give each price equal weight.
#' @param r1 Order of the price index used to calculate the elemental price
#' indexes: 0 for a geometric index (the default), 1 for an arithmetic index,
#' or -1 for a harmonic index. Other values are possible; see
#' [`generalized_mean()`][generalized_mean] for details.
#' @param r2 Order of the price index used to aggregate the elemental price
#' indexes: 0 for a geometric index, 1 for an arithmetic index (the default),
#' or -1 for a harmonic index. Other values are possible; see
#' [`generalized_mean()`][generalized_mean] for details.
#' @return A copy of `x` with missing values replaced (where possible).
#' @seealso [price_relative()] for making price relatives for the
#' same products over time.
#' @references ILO, IMF, OECD, Eurostat, UN, and World Bank. (2020).
#' *Consumer Price Index Manual: Theory and Practice*. International
#' Monetary Fund.
#' @examples
#' 
#' prices <- data.frame(
#'   price = c(1:7, NA),
#'   period = rep(1:2, each = 4), 
#'   product = 1:4,
#'   ea = rep(letters[1:2], 4)
#' )
#'                      
#' with(prices, carry_forward(price, period, product))
#' 
#' with(prices, shadow_price(price, period, product, ea))
#' 
NULL





#' Get the attributes for a price index
#' 
#' Methods to get and set the levels and time periods for a price index.
#' 
#' 
#' @aliases levels.piar_index levels<-.piar_index levels<-.aggregate_piar_index
#' time.piar_index time<- time<-.piar_index start.piar_index end.piar_index
#' @param x A price index, as made by, e.g.,
#' [`elemental_index()`][elemental_index].
#' @param value A character vector, or something that can be coerced into one,
#' giving the replacement levels/time periods for `x`.
#' @param ... Further arguments passed to or used by methods.
#' @return `levels()` and `time()` return a character vector with the
#' levels and time periods for a price index. `start()` and `end()`
#' return the first and last time period.
#' 
#' The replacement methods return a copy of `x` with the levels/time
#' periods in `value`. It's not generally possible to change the levels of
#' an aggregate price index, and in this case replacing the levels does not
#' return an aggregate index.
#' @examples
#' 
#' prices <- data.frame(
#'   rel = 1:8,
#'   period = rep(1:2, each = 4),
#'   ea = rep(letters[1:2], 4)
#' )
#' 
#' epr <- with(prices, elemental_index(rel, period, ea))
#' 
#' levels(epr)
#' time(epr)
#' start(epr)
#' end(epr)
#' 
NULL





#' Price Index Aggregation
#' 
#' Most price indexes are made with a two-step procedure, where
#' period-over-period *elemental indexes* are first calculated for a
#' collection of *elemental aggregates* at each point in time, and then
#' aggregated according to a *price index aggregation structure*. These
#' indexes can then be chained together to form a time series that gives the
#' evolution of prices with respect to a fixed base period. This package
#' contains a collections of functions that revolve around this work flow,
#' making it easy to build standard price indexes, and implement the methods
#' described by Balk (2008), von der Lippe (2001), and the CPI manual (2020)
#' for bilateral price indexes.
#' 
#' 
#' @name piar-package
#' @aliases piar-package piar
#' @docType package
#' @section Usage: The vignette *Making price indexes* gives several
#' extended examples of how to use the functions in this package to make
#' different types of price indexes. Run `vignette("making_price_indexes",
#' "piar")` to view it. But the basic work flow is fairly simple.
#' 
#' The starting point is to make period-over-period elemental price indexes
#' with the [`elemental_index()`][elemental_index] function and an
#' aggregation structure with the
#' [`aggregation_structure()`][aggregation_structure] function. The
#' [`aggregate()`][aggregate.piar_index] method can then be used to
#' aggregate the elemental indexes according to the aggregation structure.
#' There are a variety of methods to work with these index objects, such as
#' chaining them over time.
#' 
#' The two-step workflow is described in chapter 8 of the CPI manual (2020) and
#' chapter 5 of Balk (2008). A practical overview is given by Chiru et al.
#' (2015) for the Canadian CPI, and a detailed discussion of chaining indexes
#' is given by von der Lippe (2001).
#' @author **Maintainer**: Steve Martin \email{stevemartin041@@gmail.com}
#' @seealso <https://github.com/marberts/piar>
#' @references Balk, B. M. (2008). *Price and Quantity Index Numbers*.
#' Cambridge University Press.
#' 
#' Chiru, R., Huang, N., Lequain, M. Smith, P., and Wright, A. (2015).
#' *The Canadian Consumer Price Index Reference Paper*, Statistics Canada
#' catalogue 62-553-X. Statistics Canada.
#' 
#' ILO, IMF, OECD, Eurostat, UN, and World Bank. (2020). *Consumer Price
#' Index Manual: Theory and Practice*. International Monetary Fund.
#' 
#' von der Lippe, P. (2001). *Chain Indices: A Study in Price Index
#' Theory*, Spectrum of Federal Statistics vol. 16. Federal Statistical Office,
#' Wiesbaden.
NULL





#' Price data
#' 
#' Sample price and weight data for both a match sample and fixed sample type
#' index.
#' 
#' 
#' @name price_data
#' @aliases ms_prices ms_weights fs_prices fs_weights
#' @docType data
NULL



