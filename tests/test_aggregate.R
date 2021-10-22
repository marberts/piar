#---- Tests for aggregate.index() method ----
library(piar)

set.seed(12345)

# Tests for corner cases
unclass(aggregate(elemental_index(numeric(0)), aggregation_structure(list())))

all.equal(as_elemental_index(aggregate(elemental_index(numeric(0)), aggregation_structure(list()))),
          elemental_index(numeric(0)))

all.equal(update(aggregate(elemental_index(numeric(0)), aggregation_structure(list()))),
          aggregation_structure(list()))

unclass(aggregate(elemental_index(1:5), aggregation_structure(list())))

unclass(as_elemental_index(aggregate(elemental_index(1:5), aggregation_structure(list()))))

all.equal(update(aggregate(elemental_index(1:5), aggregation_structure(list()))),
          aggregation_structure(list()))

unclass(aggregate(elemental_index(1:5), aggregation_structure(list("2"))))

is.na(weights(update(aggregate(elemental_index(1:5), aggregation_structure(list("2"))))))

all.equal(update(aggregate(elemental_index(rep(1, 10)), aggregation_structure(list(1)))),
          aggregation_structure(list(1)))

# Tests with a matched-sample index
ms_epr <- with(
  ms_prices, 
  elemental_index(price_relative(price, period, product),
                  period, business, contrib = TRUE, na.rm = TRUE)
)

ms_pias <- with(
  ms_weights,
  aggregation_structure(c(expand_classification(classification), list(business)), weight)
)

(ms_index <- aggregate(ms_epr, ms_pias, na.rm = TRUE))

unclass(ms_index)

as_elemental_index(ms_index)

# Check adding up of lower-level indexes
all.equal(apply(cumprod(ms_index)[4:8, ], 2, weighted.mean, weights(ms_pias)[[3]]),
          cumprod(ms_index)[1, ])

all.equal(apply(cumprod(ms_index)[2:3, ], 2, weighted.mean, weights(ms_pias)[[2]]),
          cumprod(ms_index)[1, ])

# Re-aggregating the index shouldn't do anything
all.equal(aggregate(ms_index, ms_pias)[], ms_index[])

all.equal(aggregate(ms_index, ms_pias, na.rm = TRUE)[], ms_index[])

# Re-arranging the index shouldn't do anything
ms_epr <- with(
  ms_prices[sample(nrow(ms_prices)), ], 
  elemental_index(price_relative(price, period, product),
                  period, business, contrib = TRUE, na.rm = TRUE)
)

ms_pias <- with(
  ms_weights[sample(nrow(ms_weights)), ],
  aggregation_structure(c(expand_classification(classification), list(business)), weight)
)

all.equal(aggregate(ms_epr, ms_pias, na.rm = TRUE)[rownames(ms_index[]), ], ms_index[])

# Stacking shouldn't do anything
all.equal(Reduce(stack, unstack(ms_index)), ms_index)

# Aggregated contributions should add up
all.equal(ms_index[1, ], 
          sapply(ms_index$contributions, function(x) sum(x[[1]], na.rm = TRUE) + 1))

# Check that weights are getting price updated correctly
apply(cumprod(ms_index)[4:8, ], 2, `*`, ms_weights$weight)

weights(update(ms_index), ea_only = TRUE)

weights(update(ms_index, "202003"), ea_only = TRUE)

# Do the same tests but with a weird index
ms_epr <- with(
  ms_prices,
  elemental_index(price_relative(price, period, product), 
                  period, business, contrib = TRUE, r = 0.2)
)

ms_index <- aggregate(ms_epr, ms_pias, r = -1.7, na.rm = TRUE)

all.equal(aggregate(ms_index, ms_pias, r = -1.7, na.rm = TRUE)[], ms_index[])

all.equal(ms_index[1, ], 
          sapply(ms_index$contributions, function(x) sum(x[[1]], na.rm = TRUE) + 1))

all.equal(apply(cumprod(ms_index)[2:3, ], 2, gpindex::generalized_mean(-1.7), weights(ms_pias)[[2]]),
          cumprod(ms_index)[1, ])

ms_index <- aggregate(ms_epr, ms_pias, r = -1.7)

all.equal(aggregate(ms_index, ms_pias, r = -1.7)[], ms_index[])

all.equal(ms_index[1, ], sapply(ms_index$contributions, function(x) sum(x[[1]]) + 1))

# Tests with a fixed-sample index
fs_epr <- with(
  fs_prices, 
  elemental_index(price_relative(price, period, business),
                  period, classification, w = weight, contrib = TRUE)
)

fs_pias <- with(
  fs_weights,
  aggregation_structure(expand_classification(classification), weight)
)

(fs_index <- aggregate(fs_epr, fs_pias, na.rm = TRUE))

unclass(fs_index)

# Re-aggregating the index shouldn't do anything
all.equal(aggregate(fs_index, fs_pias)[], fs_index[])

# Contributions should add up
all.equal(fs_index[1, ], 
          sapply(fs_index$contributions, function(x) sum(x[[1]], na.rm = TRUE) + 1))

# Check adding up of lower level indexes
all.equal(apply(cumprod(fs_index)[5:9, ], 2, weighted.mean, weights(fs_pias)[[3]]),
          cumprod(fs_index)[1, ])

all.equal(apply(cumprod(fs_index)[2:4, ], 2, weighted.mean, weights(fs_pias)[[2]]),
          cumprod(fs_index)[1, ])

# Non-missing indexes should be the same when missing values are not remove
fs_index2 <- aggregate(fs_epr, fs_pias)
fs_index2[] - fs_index[]

all.equal(fs_index2["121", ], 
          sapply(fs_index2$contributions, function(x) sum(x[["121"]], na.rm = TRUE) + 1))

all.equal(fs_index2["13", 1:3], 
          sapply(fs_index2$contributions[1:3], function(x) sum(x[["13"]], na.rm = TRUE) + 1))

# Tests with a fixed-base index
prices <- data.frame(price = 1:15, 
                     period = letters[1:3], 
                     product = rep(1:5, each = 3), 
                     ea = rep(c("f1", "f2"), c(6, 9)))
prices$pop_rel <- with(prices, price_relative(price, period, product))
prices$fx_rel <- with(prices, price / gpindex::base_price(price, period, product))

pias <- aggregation_structure(list(c("1", "1"), c("f1", "f2")), 1:2)

epr_pop <- with(prices, elemental_index(pop_rel, period, ea))
epr_fx <- with(prices, elemental_index(fx_rel, period, ea))

index_pop <- aggregate(epr_pop, pias)
index_fx <- aggregate(epr_fx, pias, chained = FALSE)

# Chained calculation and fixed-base calculation should be the same
all.equal(as.matrix(index_fx), cumprod(index_pop))
