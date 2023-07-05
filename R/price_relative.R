# TODO: Should this be a class generator?
price_relative <- function(x, period, product) {
  x <- as.numeric(x)
  res <- x / x[back_period(period, product)]
  structure(res, names = as.character(product))
}
