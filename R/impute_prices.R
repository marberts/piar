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
#' value of the period-over-period elementary index for the elementary aggregate
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
#' @param x Either a numeric vector (or something that can be coerced into one)
#'   or data frame of prices.
#' @param period A factor, or something that can be coerced into one, giving
#'   the time period associated with each price in `x`. The ordering of time
#'   periods follows of the levels of `period`, to agree with
#'   [`cut()`][cut.Date].
#' @param product A factor, or something that can be coerced into one, giving
#'   the product associated with each price in `x`.
#' @param ea A factor, or something that can be coerced into one, giving the
#'   elementary aggregate associated with each price in `x`.
#' @param pias A price index aggregation structure, or something that can be
#'   coerced into one, as made with [aggregation_structure()]. The default
#'   imputes from elementary indexes only (i.e., not recursively).
#' @param weights A numeric vector of weights for the prices in `x` (i.e.,
#'   product weights), or something that can be coerced into one. The default is
#'   to give each price equal weight. This is evaluated in `x` for the data
#'   frame method.
#' @param r1 Order of the generalized-mean price index used to calculate the
#'   elementary price indexes: 0 for a geometric index (the default), 1 for an
#'   arithmetic index, or -1 for a harmonic index. Other values are possible;
#'   see [gpindex::generalized_mean()] for details.
#' @param r2 Order of the generalized-mean price index used to aggregate the
#'   elementary price indexes: 0 for a geometric index, 1 for an arithmetic
#'   index (the default), or -1 for a harmonic index. Other values are possible;
#'   see [gpindex::generalized_mean()] for details.
#' @param formula A two-sided formula with prices on the left-hand
#'   side. For `carry_forward()` and `carry_backward()`, the right-hand side
#'   should have time periods and products (in that order); for
#'   `shadow_price()`, the right-hand side should have time period, products,
#'   and elementary aggregates (in that order).
#' @param ... Further arguments passed to or used by methods.
#'
#' @returns
#' A numeric vector of prices with missing values replaced (where possible).
#'
#' @seealso
#' [price_relative()] for making price relatives for the
#' same products over time.
#'
#' @references
#' IMF, ILO, OECD, Eurostat, UNECE, and World Bank. (2020).
#' *Consumer Price Index Manual: Concepts and Methods*.
#' International Monetary Fund.
#'
#' @examples
#' prices <- data.frame(
#'   price = c(1:7, NA),
#'   period = rep(1:2, each = 4),
#'   product = 1:4,
#'   ea = rep(letters[1:2], 4)
#' )
#'
#' carry_forward(prices, price ~ period + product)
#'
#' shadow_price(prices, price ~ period + product + ea)
#'
#' @export
shadow_price <- function(x, ...) {
  UseMethod("shadow_price")
}

#' @rdname impute_prices
#' @export
shadow_price.default <- function(
  x,
  period,
  product,
  ea,
  ...,
  pias = NULL,
  weights = NULL,
  r1 = 0,
  r2 = 1
) {
  # This is mostly a combination of gpindex::back_period() and aggregate()
  # it just does it period-by-period and keeps track of prices to impute.
  chkDots(...)
  x <- as.numeric(x)
  period <- as.factor(period)
  product <- as.factor(product)
  attributes(product) <- NULL
  ea <- as.factor(ea)
  if (!is.null(weights)) {
    weights <- as.numeric(weights)
  }

  if (different_length(x, period, product, ea, weights)) {
    stop("input vectors must be the same length")
  }
  if (nlevels(period) == 0L) {
    return(rep.int(NA_integer_, length(period)))
  }

  res <- split(x, period)
  product <- split(product, period)
  if (duplicate_products(product)) {
    warning("there are duplicated period-product pairs")
  }
  ea <- split(ea, period)
  if (is.null(weights)) {
    w <- rep.int(list(NULL), nlevels(period))
  } else {
    w <- split(weights, period)
  }
  if (!is.null(pias)) {
    pias <- as_aggregation_structure(pias)
  }
  for (t in seq_along(res)[-1L]) {
    # Calculate relatives.
    matches <- match(product[[t]], product[[t - 1L]], incomparables = NA)
    back_price <- res[[t - 1L]][matches]
    price <- res[[t]]
    # Calculate indexes.
    epr <- elementary_index(
      price / back_price,
      period = gl(1, length(price)),
      ea = ea[[t]],
      weights = w[[t]],
      na.rm = TRUE,
      r = r1
    )
    if (!is.null(pias)) {
      epr <- aggregate(epr, pias, na.rm = TRUE, r = r2)
      pias <- update(pias, epr, r = r2)
    }
    # Add shadow prices to 'x'.
    impute <- is.na(price)
    eas <- match(as.character(ea[[t]][impute]), epr$levels)
    res[[t]][impute] <- epr$index[, 1L][eas] * back_price[impute]
  }
  unsplit(res, period)
}

#' @rdname impute_prices
#' @export
shadow_price.data.frame <- function(x, formula, ..., weights = NULL) {
  vars <- formula_vars(formula, x, 3L)
  weights <- eval(substitute(weights), x, parent.frame())

  shadow_price(
    vars[[1L]],
    period = vars[[2L]],
    product = vars[[3L]],
    ea = vars[[4L]],
    weights = weights,
    ...
  )
}

#' @rdname impute_prices
#' @export
carry_forward <- function(x, ...) {
  UseMethod("carry_forward")
}

#' @rdname impute_prices
#' @export
carry_forward.default <- function(x, period, product, ...) {
  chkDots(...)
  x <- as.numeric(x)
  period <- as.factor(period)
  product <- as.factor(product)
  attributes(product) <- NULL

  if (different_length(x, period, product)) {
    stop("input vectors must be the same length")
  }
  if (nlevels(period) == 0L) {
    return(rep.int(NA_integer_, length(period)))
  }

  res <- split(x, period)
  product <- split(product, period)
  if (duplicate_products(product)) {
    warning("there are duplicated period-product pairs")
  }
  for (t in seq_along(res)[-1L]) {
    impute <- is.na(res[[t]])
    matches <- match(
      product[[t]][impute],
      product[[t - 1L]],
      incomparables = NA
    )
    res[[t]][impute] <- res[[t - 1L]][matches]
  }
  unsplit(res, period)
}

#' @rdname impute_prices
#' @export
carry_forward.data.frame <- function(x, formula, ...) {
  chkDots(...)
  vars <- formula_vars(formula, x)

  carry_forward(vars[[1L]], period = vars[[2L]], product = vars[[3L]])
}

#' @rdname impute_prices
#' @export
carry_backward <- function(x, ...) {
  UseMethod("carry_backward")
}

#' @rdname impute_prices
#' @export
carry_backward.default <- function(x, period, product, ...) {
  chkDots(...)
  period <- as.factor(period)
  levels <- rev(levels(period))
  carry_forward(x, period = factor(period, levels), product = product)
}

#' @rdname impute_prices
#' @export
carry_backward.data.frame <- function(x, formula, ...) {
  chkDots(...)
  vars <- formula_vars(formula, x)

  carry_backward(vars[[1L]], period = vars[[2L]], product = vars[[3L]])
}
