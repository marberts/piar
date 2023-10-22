# new_price_relative <- function(relative, period, product, chainable) {
#   structure(relative, period = period, product = product, chainable = chainable,
#             class = "price_relative")
# }
#
# price_relative <- function(x, ...) {
#   UseMethod("price_relative")
# }
#
# price_relative.default <- function(x, ...) {
#   price_relative(as.numeric(x), ...)
# }
#
# price_relative.numeric <- function(x, period, product, chainable = TRUE, ...) {
#   res <- x / x[back_period(period, product)]
#   new_price_relative(res, period, product, chainable)
# }

#' Calculate period-over-period price relatives
#'
#' Construct period-over-period price relatives from information on prices and
#' products over time.
#'
#' @param x A numeric vector of prices.
#' @param period A factor, or something that can be coerced into one, that
#' gives the corresponding time period for each element in `x`. The
#' ordering of time periods follows the levels of `period` to agree with
#' [`cut()`][cut.Date].
#' @param product A factor, or something that can be coerced into one, that
#' gives the corresponding product identifier for each element in `x`.
#'
#' @returns
#' A numeric vector of price relatives, with `product` as names.
#'
#' @seealso
#' [gpindex::back_period()] to get only the back price.
#'
#' [gpindex::base_period()] for making fixed-base price relatives.
#'
#' [carry_forward()] and [shadow_price()] to impute missing prices.
#'
#' [`gpindex::outliers`] for methods to identify outliers with price relatives.
#'
#' @examples
#' price_relative(1:6, rep(1:2, each = 3), rep(letters[1:3], 2))
#'
#' @export
price_relative <- function(x, period, product) {
  x <- as.numeric(x)
  res <- x / x[gpindex::back_period(period, product)]
  names(res) <- product
  res
}
