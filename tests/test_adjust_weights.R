library(piar)

# Reproduce example for CPI
rel <- c(1.1, 0.82, 1.33, 1, 1, 1.08, 0.77, 0.9, 1.11, 1.1, 0.95, 1.05)
epr <- elemental_index(rel, period = 1:12)
pias <- aggregation_structure(list("b", 1), 155)
weights(adjust_weights(pias, aggregate(epr, pias)))

# Do it again with a fixed-base index
epr2 <- elemental_index(cumprod(rel), period = 1:12)
weights(adjust_weights(pias, aggregate(epr, pias, chained = FALSE)))

# Adjusting weights does nothing if prices don't change
ms_prices$price <- with(ms_prices, ifelse(is.na(price), NA, 1))
epr <- with(ms_prices, elemental_index(price, period, business, na.rm = TRUE))
pias <- with(
  ms_weights,
  aggregation_structure(c(expand_classification(classification), list(business)), weight)
)
index <- aggregate(epr, pias, na.rm = TRUE)
all.equal(weights(pias), weights(adjust_weights(pias, index)))
