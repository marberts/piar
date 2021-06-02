price_relative <- function(x, period, product) {
  x <- as.numeric(x)
  res <- x / back_price(x, period, product)
  structure(res, names = as.character(product))
}
