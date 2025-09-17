#---- Make some data ----
big_prices <- data.frame(
  period = sprintf("%d%02d", 2020, 1:12),
  business = rep(paste0("B", 1:5000), each = 1200),
  product = rep(1:500000, each = 12),
  price = runif(6e6, 0.5, 2)
)

big_weights <- data.frame(
  business = paste0("B", 1:5000),
  classification = paste0(
    1,
    rep(1:2, each = 2500),
    rep(1:5, each = 500),
    rep(1:5, each = 100),
    rep(1:5, each = 20)
  ),
  weight = runif(5000, 10, 1000)
)

big_prices2 <- data.frame(
  period = sprintf("%d%02d", rep(2011:2020, each = 12), 1:12),
  business = rep(paste0("B", 1:2500), each = 120 * 10),
  product = rep(1:25000, each = 120),
  price = runif(3e6, 0.5, 2)
)

big_weights2 <- data.frame(
  business = paste0("B", 1:2500),
  classification = paste0(
    1,
    rep(1:2, each = 1250),
    rep(1:5, each = 250),
    rep(1:5, each = 50),
    rep(1:5, each = 10)
  ),
  weight = runif(2500, 10, 1000)
)

#---- Make the aggregation structures ----
pias <- with(
  big_weights,
  aggregation_structure(
    c(expand_classification(classification), list(business)),
    weight
  )
)

pias2 <- with(
  big_weights2,
  aggregation_structure(
    c(expand_classification(classification), list(business)),
    weight
  )
)

#---- Benchmarks ----
big_prices$rel <- price_relative(big_prices, price ~ period + product)

elem <- with(
  big_prices,
  elemental_index(rel, period = period, ea = business, contrib = TRUE)
)

index <- aggregate(elem, pias, na.rm = TRUE)

bench::mark(
  aggregate(index, pias),
  aggregate(index, pias, dup_products = "sum")
)
