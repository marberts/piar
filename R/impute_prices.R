#' Impute missing prices
#'
#' Impute missing prices using the carry forward or shadow price method.
#'
#' The carry forward method replaces a missing price for a product by the price
#' for the same product in the previous period. It tends to push an index value
#' towards 1, and is usually avoided; see paragraph 6.61 in the CPI manual
#' (2020). The carry backwards method does the opposite, but this is rarely
#' used in practice.
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
#' @name impute_prices
#' @aliases impute_prices
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
#' coerced into one, as made with [aggregation_structure()]. The default
#' imputes from elemental indexes only (i.e., not recursively).
#' @param weights A numeric vector of weights for the prices in `x` (i.e.,
#' product weights). The default is to give each price equal weight.
#' @param r1 Order of the generalized-mean price index used to calculate the
#' elemental price indexes: 0 for a geometric index (the default), 1 for an
#' arithmetic index, or -1 for a harmonic index. Other values are possible; see
#' [gpindex::generalized_mean()] for details.
#' @param r2 Order of the generalized-mean price index used to aggregate the
#' elemental price indexes: 0 for a geometric index, 1 for an arithmetic index
#' (the default), or -1 for a harmonic index. Other values are possible; see
#' [gpindex::generalized_mean()] for details.
#'
#' @returns
#' A copy of `x` with missing values replaced (where possible).
#'
#' @seealso
#' [price_relative()] for making price relatives for the
#' same products over time.
#'
#' @references
#' ILO, IMF, OECD, Eurostat, UN, and World Bank. (2020).
#' *Consumer Price Index Manual: Theory and Practice*. International
#' Monetary Fund.
#'
#' @examples
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
#' @export
shadow_price <- function(x, period, product, ea,
                         pias = NULL, weights = NULL, r1 = 0, r2 = 1) {
  if (different_length(x, period, product, ea, weights)) {
    stop("input vectors must be the same length")
  }
  # this is mostly a combination of gpindex::back_period() and aggregate()
  # it just does it period-by-period and keeps track of prices to impute
  period <- as.factor(period)
  if (nlevels(period) == 0L) {
    return(rep.int(NA_integer_, length(period)))
  }

  res <- split(x, period)
  product <- as.factor(product)
  attributes(product) <- NULL
  product <- split(product, period)
  if (duplicate_products(product)) {
    warning("there are duplicated period-product pairs")
  }
  ea <- split(as.factor(ea), period)
  if (is.null(weights)) {
    w <- rep.int(list(NULL), nlevels(period))
  } else {
    w <- split(as.numeric(weights), period)
  }
  if (!is.null(pias)) {
    pias <- as_aggregation_structure(pias)
  }
  for (t in seq_along(res)[-1L]) {
    # calculate relatives
    matches <- match(product[[t]], product[[t - 1L]], incomparables = NA)
    back_price <- res[[t - 1L]][matches]
    price <- res[[t]]
    # calculate indexes
    epr <- elemental_index(price / back_price,
      ea = ea[[t]], weights = w[[t]], na.rm = TRUE, r = r1
    )
    if (!is.null(pias)) {
      epr <- aggregate(epr, pias, na.rm = TRUE, r = r2)
      pias <- update(pias, epr)
    }
    # add shadow prices to 'x'
    impute <- is.na(price)
    eas <- match(as.character(ea[[t]][impute]), epr$levels)
    res[[t]][impute] <- epr$index[[1L]][eas] * back_price[impute]
  }
  res <- unsplit(res, period)
  attributes(res) <- attributes(x)
  res
}

#' @rdname impute_prices
#' @export
carry_forward <- function(x, period, product) {
  if (different_length(x, period, product)) {
    stop("input vectors must be the same length")
  }
  period <- as.factor(period)
  if (nlevels(period) == 0L) {
    return(rep.int(NA_integer_, length(period)))
  }

  res <- split(x, period)
  product <- as.factor(product)
  attributes(product) <- NULL
  product <- split(product, period)
  if (duplicate_products(product)) {
    warning("there are duplicated period-product pairs")
  }
  for (t in seq_along(res)[-1L]) {
    impute <- is.na(res[[t]])
    matches <- match(product[[t]][impute],
      product[[t - 1L]],
      incomparables = NA
    )
    res[[t]][impute] <- res[[t - 1L]][matches]
  }
  res <- unsplit(res, period)
  attributes(res) <- attributes(x)
  res
}

#' @rdname impute_prices
#' @export
carry_backwards <- function(x, period, product) {
  period <- as.factor(period)
  carry_forward(x, factor(period, rev(levels(period))), product)
}
