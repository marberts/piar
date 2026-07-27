#' Back period
#'
#' Offset a vector prices or quantities by computing the position one (or more)
#' period backwards for each product.
#'
#' @export
#'
#' @param period `[factor]` A factor, or something that can be coerced into one,
#'   that
#'   gives the time period for each transaction. The ordering of time periods
#'   follows the levels of `period` to agree with
#'   [`cut()`][cut.Date].
#' @param product `[factor]` A factor, or something that can be coerced into
#'   one, that
#'   gives the product identifier for each transaction. The default is to
#'   assume that all transactions are for the same product.
#' @param match_first `[logical(1)]` Should products in the first period match
#'   with themselves (the default)?
#' @param offset `[integer(1)]` The number of periods to offset. The default
#'   offsets by one period (back period). Setting to `nlevels(period)` gives
#'   the the base period.
#'
#' @returns
#' A numeric vector of indices giving the position of the the back periods.
#'
#' @note
#' By definition, there must be at most one transaction for each product
#' in each time period to determine a back period. If multiple transactions
#' correspond to a period-product pair, then the back period at a point in
#' time is always the first position for that product in the previous period.
#'
#' @seealso
#' [`outliers()`] for common methods to detect outliers for price relatives.
#'
#' `rs_pairs` in the \pkg{rsmatrix} package for making sales pairs.
#'
#' @examples
#' prices <- data.frame(
#'   price = 1:6,
#'   product = factor(c("a", "b")),
#'   period = factor(c(1, 1, 2, 2, 3, 3))
#' )
#'
#' with(prices, back_period(period, product))
#'
#' # Make fixed-base price relatives.
#' with(
#'   prices,
#'   price / price[back_period(period, product, offset = nlevels(period))]
#' )
#'
#' # Change the base period with relevel().
#' with(
#'   prices,
#'   price / price[
#'     back_period(relevel(period, "2"), product, offset = nlevels(period))
#'   ]
#' )
back_period <- function(
  period,
  product = NULL,
  match_first = TRUE,
  offset = 1L
) {
  f <- function(x, offset) {
    x[c(rep.int(1L, offset), seq_len(length(x) - offset))]
  }
  offset <- as.integer(offset)
  period <- as.factor(period)
  if (!is.null(product)) {
    product <- as.factor(product)
    attributes(product) <- NULL # matching is faster on factor codes
  } else {
    product <- rep.int(1L, length(period))
  }

  if (length(period) != length(product)) {
    stop("`period` and `product` must be the same length")
  }
  if (offset < 0L || offset > nlevels(period)) {
    stop("`offset` must be a positive integer less than `nlevels(period)`")
  }

  product <- split(product, period)
  if (duplicate_products(product)) {
    warning("there are duplicated period-product pairs")
  }
  if (offset == 0L) {
    return(seq_along(period))
  }
  m <- Map(match, product, f(product, offset), incomparables = NA)
  if (!match_first) {
    m[seq_len(offset)][] <- NA_integer_
  }
  res <- split(seq_along(period), period)
  unsplit(Map(`[`, f(res, offset), m), period)
}
