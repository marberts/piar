#' Impute missing prices
#'
#' Impute missing prices using the carry forward/backward or the
#' self-correcting overall mean method.
#'
#' The carry forward method replaces a missing price for a product by the price
#' for the same product in the previous period. It tends to push an index value
#' towards 1, and is usually avoided; see paragraph 6.61 in the CPI manual
#' (2020). The carry backwards method does the opposite, but this is rarely
#' used in practice.
#'
#' The self-correcting overall mean method recursively imputes a missing price
#' by the value of
#' the price for the same product in the previous period multiplied by the
#' value of the period-over-period elementary index for the elementary aggregate
#' to which that product belongs. This requires computing and aggregating an
#' index (according to `pias`, unless `pias` is not supplied) for
#' each `period`. The index
#' values used to do the imputations are not returned because the index needs
#' to be recalculated to get correct percent-change contributions. It is
#' identical to simply
#' excluding missing price relatives in the index calculation, except in the
#' period that a missing product returns. For this reason care is needed when
#' using this method. It is sensitive to the assumption that a product does not
#' change over time, and in some cases it is safer to simply omit the missing
#' price relatives instead of imputing the missing prices.
#'
#' Imputation works slightly differently depending on whether data are in a long
#' or wide format. When `x` is a two-column of matrix of current and back prices
#' (in that order), then imputation is done separately on the current price
#' at a point in time and the back price at the next point in time. When `x` is
#' a numeric vector then these two prices are necessarily the same.
#'
#' @name impute_prices
#' @param x Either a numeric vector (or something that can be coerced into one),
#'   a data frame of prices, or a two-column matrix of current prices
#'   and back prices (in that order).
#' @param period A factor, or something that can be coerced into one, giving
#'   the time period associated with each price in `x`. The ordering of time
#'   periods follows of the levels of `period`, to agree with
#'   [`cut()`][cut.Date].
#' @param product A factor, or something that can be coerced into one, giving
#'   the product associated with each price in `x`.
#' @param ea A factor, or something that can be coerced into one, giving the
#'   elementary aggregate associated with each price in `x`. This is evaluated
#'   in `x` for the data frame method. The default pools all data into one
#'   elementary aggregate.
#' @param pias A price index aggregation structure, or something that can be
#'   coerced into one, as made with [aggregation_structure()]. The default
#'   imputes from elementary indexes only (i.e., not recursively).
#' @param weights A numeric vector of weights for the prices in `x` (i.e.,
#'   product weights), or something that can be coerced into one. The default is
#'   to give each price equal weight. This is evaluated in `x` for the data
#'   frame method.
#' @param r A pair of numeric values. The first gives the order of the
#'   generalized-mean price index used to calculate the
#'   elementary price indexes, defaulting to a geometric index. The second
#'   gives the order of the generalized-mean price index used to aggregate the
#'   elementary price indexes, defaulting to an arithmetic index. Other values
#'   are possible;
#'   see [gpindex::generalized_mean()] for details.
#' @param formula A two-sided formula with prices on the left-hand
#'   side and time periods and products on the right-hand side (in that order).
#' @param method Name of the imputation method, one of `"overall-mean"`,
#'   `"carry-forward"`, or `"carry-backward"`.
#' @param ... Further arguments passed to or used by methods.
#'
#' @returns
#' A numeric vector or matrix of prices with missing values replaced
#' (where possible).
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
#' impute_prices(prices, price ~ period + product, method = "carry-forward")
#'
#' impute_prices(
#'   prices,
#'   price ~ period + product,
#'   ea = ea,
#'   method = "overall-mean"
#' )
#'
#' # Can also be done with current price-back price formulation.
#' prices$back_price <- with(
#'   prices,
#'   price[gpindex::back_period(period, product)]
#' )
#'
#' impute_prices(
#'   prices,
#'   cbind(price, back_price) ~ period + product,
#'   ea = ea,
#'   method = "overall-mean"
#' )
#'
#' @export
impute_prices <- function(x, ...) {
  UseMethod("impute_prices")
}

#' @rdname impute_prices
#' @export
impute_prices.default <- function(x, ...) {
  impute_prices(as.numeric(x), ...)
}

#' @rdname impute_prices
#' @export
impute_prices.matrix <- function(
  x,
  period,
  product,
  ...,
  ea = NULL,
  weights = NULL,
  pias = NULL,
  r = c(0, 1),
  method = c("overall-mean", "carry-forward")
) {
  # This is mostly a combination of gpindex::back_period() and aggregate()
  # it just does it period-by-period and keeps track of prices to impute.
  chkDots(...)
  method <- match.arg(method)
  period <- as.factor(period)
  product <- as.factor(product)
  attributes(product) <- NULL
  if (!is.null(ea)) {
    ea <- as.factor(ea)
  }
  if (!is.null(weights)) {
    weights <- as.numeric(weights)
  }

  if (different_length(x[, 1L], period, product, ea, weights)) {
    stop("input vectors must be the same length")
  }
  if (nlevels(period) == 0L) {
    return(matrix(NA_real_, nrow = length(period), ncol = 2))
  }

  res <- split.data.frame(x, period)
  product <- split(product, period)
  if (duplicate_products(product)) {
    warning("there are duplicated period-product pairs")
  }
  if (!is.null(ea)) {
    ea <- split(ea, period)
  }
  if (!is.null(weights)) {
    weights <- split(weights, period)
  }
  if (!is.null(pias)) {
    pias <- as_aggregation_structure(pias)
  }
  for (t in seq_along(res)) {
    impute <- which(is.na(res[[t]][, 1L]))
    if (method == "overall-mean") {
      index <- elementary_index(
        res[[t]][, 1L] / res[[t]][, 2L],
        ea = ea[[t]],
        weights = weights[[t]],
        na.rm = TRUE,
        r = r[1L]
      )
      if (!is.null(pias)) {
        index <- aggregate(index, pias, na.rm = TRUE, r = r[2L])
        pias <- update(pias, index, r = r[2L])
      }
      eas <- if (!is.null(ea)) {
        match(as.character(ea[[t]][impute]), index$levels)
      } else {
        1L
      }
      res[[t]][impute, 1L] <- as.numeric(index)[eas] * res[[t]][impute, 2L]
    } else {
      res[[t]][impute, 1L] <- res[[t]][impute, 2L]
    }
    if (t < length(res)) {
      impute2 <- which(is.na(res[[t + 1L]][, 2L]))
      matches <- match(product[[t + 1L]][impute2], product[[t]][impute])
      res[[t + 1L]][impute2, 2L] <- res[[t]][impute, 1L][matches]
    }
  }
  # unsplit() doesn't work nicely with matrices.
  split(x, period) <- res
  x
}

#' @rdname impute_prices
#' @export
impute_prices.numeric <- function(
  x,
  period,
  product,
  ...,
  ea = NULL,
  weights = NULL,
  pias = NULL,
  r = c(0, 1),
  method = c("overall-mean", "carry-forward", "carry-backward")
) {
  # This is mostly a combination of gpindex::back_period() and aggregate()
  # it just does it period-by-period and keeps track of prices to impute.
  chkDots(...)
  method <- match.arg(method)
  period <- as.factor(period)
  if (method == "carry-backward") {
    period <- factor(period, rev(levels(period)))
  }
  product <- as.factor(product)
  attributes(product) <- NULL
  if (!is.null(ea)) {
    ea <- as.factor(ea)
  }
  if (!is.null(weights)) {
    weights <- as.numeric(weights)
  }

  if (different_length(x, period, product, ea, weights)) {
    stop("input vectors must be the same length")
  }
  if (nlevels(period) == 0L) {
    return(rep.int(NA_real_, length(period)))
  }

  res <- split(x, period)
  product <- split(product, period)
  if (duplicate_products(product)) {
    warning("there are duplicated period-product pairs")
  }
  if (!is.null(ea)) {
    ea <- split(ea, period)
  }
  if (!is.null(weights)) {
    weights <- split(weights, period)
  }
  if (!is.null(pias)) {
    pias <- as_aggregation_structure(pias)
  }
  for (t in seq_along(res)[-1L]) {
    impute <- which(is.na(res[[t]]))
    if (method == "overall-mean") {
      matches <- match(product[[t]], product[[t - 1L]], incomparables = NA)
      back_price <- res[[t - 1L]][matches]

      index <- elementary_index(
        res[[t]] / back_price,
        ea = ea[[t]],
        weights = weights[[t]],
        na.rm = TRUE,
        r = r[1L]
      )
      if (!is.null(pias)) {
        index <- aggregate(index, pias, na.rm = TRUE, r = r[2L])
        pias <- update(pias, index, r = r[2L])
      }
      eas <- if (!is.null(ea)) {
        match(as.character(ea[[t]][impute]), index$levels)
      } else {
        1L
      }
      res[[t]][impute] <- as.numeric(index)[eas] * back_price[impute]
    } else {
      matches <- match(
        product[[t]][impute],
        product[[t - 1L]],
        incomparables = NA
      )
      res[[t]][impute] <- res[[t - 1L]][matches]
    }
  }
  unsplit(res, period)
}

#' @rdname impute_prices
#' @export
impute_prices.data.frame <- function(
  x,
  formula,
  ...,
  ea = NULL,
  weights = NULL
) {
  vars <- formula_vars(formula, x, 2L)
  ea <- eval(substitute(ea), x, parent.frame())
  weights <- eval(substitute(weights), x, parent.frame())

  impute_prices(
    vars[[1L]],
    period = vars[[2L]],
    product = vars[[3L]],
    ea = ea,
    weights = weights,
    ...
  )
}

#' @rdname impute_prices
#' @export
carry_forward <- function(x, ...) {
  warning("'carry_forward() is deprecated; use 'impute_prices()' instead")
  impute_prices(x, method = "carry-forward", ...)
}

#' @rdname impute_prices
#' @export
carry_backward <- function(x, ...) {
  warning("'carry_backward() is deprecated; use 'impute_prices()' instead")
  impute_prices(x, method = "carry-backward", ...)
}

#' @rdname impute_prices
#' @export
shadow_price <- function(x, ...) {
  warning("'shadow_price() is deprecated; use 'impute_prices()' instead")
  impute_prices(x, method = "overall-mean", ...)
}
