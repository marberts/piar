library(piar)

pias <- with(
  ms_weights, 
  aggregation_structure(c(expand_classification(classification), list(business)), weight)
)

shadow_price(integer(0), integer(0), integer(0), integer(0), pias)

# Imputing shadow prices does nothing
(sp <- with(ms_prices, shadow_price(price, period, product, business, pias)))

all.equal(sp, with(ms_prices, shadow_price(sp, period, product, business, pias)))

# First period with missing values gives the same result as ignoring them 
epr <- with(ms_prices, 
            elemental_index(price_relative(price, period, product), period, business, na.rm = TRUE))
epr2 <- with(ms_prices, 
             elemental_index(price_relative(sp, period, product), period, business, na.rm = TRUE))
all.equal(epr[c(1, 3:4), 1:3], epr2[c(1, 3:4), 1:3])

all.equal(aggregate(epr, pias, na.rm = TRUE)[, 1:2], aggregate(epr2, pias, na.rm = TRUE)[, 1:2])

# No imputation should happen if the pias doesn't line up with the elemental aggregates
pias2 <- with(
  ms_weights, 
  aggregation_structure(c(expand_classification(classification), list(paste0(business, 1))), weight)
)

all.equal(ms_prices$price,
          with(ms_prices, shadow_price(price, period, product, business, pias2)))
