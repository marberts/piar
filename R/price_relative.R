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

price_relative <- function(x, period, product) {
  x <- as.numeric(x)
  res <- x / x[back_period(period, product)]
  names(res) <- product
  res
}
