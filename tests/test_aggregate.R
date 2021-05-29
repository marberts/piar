library(piar)
set.seed(12345)

# length 0 inputs
unclass(aggregate(elemental_index(numeric(0)), aggregation_structure(list())))

all.equal(update(aggregation_structure(list()), 
                 aggregate(elemental_index(numeric(0)), aggregation_structure(list()))),
          aggregation_structure(list()))

unclass(aggregate(elemental_index(1:5), aggregation_structure(list())))

all.equal(update(aggregation_structure(list()),
                 aggregate(elemental_index(1:5), aggregation_structure(list()))),
          aggregation_structure(list()))

unclass(aggregate(elemental_index(1:5), aggregation_structure(list("2"))))

is.na(weights(update(aggregation_structure(list("2")), 
               aggregate(elemental_index(1:5), aggregation_structure(list("2"))))))

all.equal(update(aggregation_structure(list(1)), 
                 aggregate(elemental_index(rep(1, 10)), aggregation_structure(list(1)))),
          aggregation_structure(list(1)))

#---- Matched sample ----
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

# adding up
all.equal(apply(cumprod(ms_index)[4:8, ], 2, weighted.mean, weights(ms_pias)[[1]]),
          cumprod(ms_index)[1, ])

all.equal(apply(cumprod(ms_index)[2:3, ], 2, weighted.mean, weights(ms_pias)[[2]]),
          cumprod(ms_index)[1, ])

# re-aggregating the index shouldn't do anything
all.equal(aggregate(ms_index, ms_pias)[], ms_index[])

all.equal(aggregate(ms_index, ms_pias, na.rm = TRUE)[], ms_index[])

# re-arranging the index shouldn't do anything
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

# are contributions getting aggregated correctly?
all.equal(ms_index[1, ], sapply(ms_index$contributions, function(x) sum(x[[1]], na.rm = TRUE) + 1))

# are weights updated correctly?
apply(cumprod(ms_index)[4:8, ], 2, `*`, ms_weights$weight)

weights(update(ms_pias, ms_index), ea_only = TRUE)

# try a weird index
ms_epr <- with(
  ms_prices,
  elemental_index(price_relative(price, period, product), 
                  period, business, contrib = TRUE, r = 0.2)
)

ms_index <- aggregate(ms_epr, ms_pias, r = -1.7, na.rm = TRUE)

all.equal(aggregate(ms_index, ms_pias, r = -1.7, na.rm = TRUE)[], ms_index[])

all.equal(ms_index[1, ], sapply(ms_index$contributions, function(x) sum(x[[1]], na.rm = TRUE) + 1))

all.equal(apply(cumprod(ms_index)[2:3, ], 2, gpindex::mean_generalized(-1.7), weights(ms_pias)[[2]]),
          cumprod(ms_index)[1, ])

ms_index <- aggregate(ms_epr, ms_pias, r = -1.7)

all.equal(aggregate(ms_index, ms_pias, r = -1.7)[], ms_index[])

all.equal(ms_index[1, ], sapply(ms_index$contributions, function(x) sum(x[[1]]) + 1))

#---- Fixed sample ----
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

# re-aggregating the index shouldn't do anything
all.equal(aggregate(fs_index, fs_pias)[], fs_index[])

# are contributions getting aggregated correctly?
all.equal(fs_index[1, ], sapply(fs_index$contributions, function(x) sum(x[[1]], na.rm = TRUE) + 1))

# adding up
all.equal(apply(cumprod(fs_index)[5:9, ], 2, weighted.mean, weights(fs_pias)[[1]]),
          cumprod(fs_index)[1, ])

all.equal(apply(cumprod(fs_index)[2:4, ], 2, weighted.mean, weights(fs_pias)[[2]]),
          cumprod(fs_index)[1, ])

# non-missing indexes should be the same
fs_index2 <- aggregate(fs_epr, fs_pias)
fs_index2[] - fs_index[]

all.equal(fs_index2["121", ], sapply(fs_index2$contributions, function(x) sum(x[["121"]], na.rm = TRUE) + 1))
