library(piar)

# Reproduce example for CPI
rel <- c(1.1, 0.82, 1.33, 1, 1, 1.08, 0.77, 0.9, 1.11, 1.1, 0.95, 1.05)
epr <- elemental_index(rel, period = 1:12)
pias <- aggregation_structure(list("b", 1), 155)
weights(adjust_weights(pias, aggregate(epr, pias)))

# Do it again with a fixed-base index
epr2 <- elemental_index(cumprod(rel), period = 1:12)
weights(adjust_weights(pias, aggregate(epr, pias, chained = FALSE)))
