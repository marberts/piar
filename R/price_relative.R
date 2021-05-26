price_relative <- function(x, period, product) {
  x <- as.numeric(x)
  product <- as.factor(product) # can be removed with version 0.2.6 of gpindex
  res <- x / back_price(x, period, product)
  structure(res, names = as.character(product))
}
