## -----------------------------------------------------------------------------
library(piar)

head(ms_prices)

ms_weights

## -----------------------------------------------------------------------------
relative <- with(ms_prices, price_relative(price, period, product))

(ms_epr <- with(ms_prices, elemental_index(relative, period, business, na.rm = TRUE)))

## -----------------------------------------------------------------------------
ms_epr[, "202004"]
ms_epr["B1", ]

## -----------------------------------------------------------------------------
pias <- with(
  ms_weights, 
  aggregation_structure(c(expand_classification(classification), list(business)), weight)
)

## -----------------------------------------------------------------------------
(ms_index <- aggregate(ms_epr, pias, na.rm = TRUE))

## -----------------------------------------------------------------------------
as.matrix(ms_index)

## -----------------------------------------------------------------------------
as.data.frame(ms_index)

## -----------------------------------------------------------------------------
ms_index$index$`202004`

## -----------------------------------------------------------------------------
(ms_index_chained <- cumprod(ms_index))

## -----------------------------------------------------------------------------
apply(as.matrix(ms_index), 1, cumprod)

## -----------------------------------------------------------------------------
ms_index_chained  / ms_index_chained[, "202004"]

## -----------------------------------------------------------------------------
ms_index_chained  / rowMeans(ms_index_chained[, c("202003", "202004")])

## -----------------------------------------------------------------------------
(ms_weights <- transform(ms_weights, stratum = c("TS", "TA", "TS", "TS", "TS")))

## -----------------------------------------------------------------------------
(classification_sps <- with(ms_weights, paste0(classification, stratum)))

## -----------------------------------------------------------------------------
(classification_sps <- expand_classification(classification_sps, width = c(1, 1, 2)))
pias_sps <- with(
  ms_weights, 
  aggregation_structure(c(classification_sps, list(business)), weight)
)

## -----------------------------------------------------------------------------
aggregate(ms_epr, pias_sps, na.rm = TRUE)

## -----------------------------------------------------------------------------
ms_weights$impute <- c("C1", "C1", "C3", "C4", "C5")
ms_weights

## -----------------------------------------------------------------------------
pias <- with(
  ms_weights, 
  aggregation_structure(c(expand_classification(classification), 
                          list(impute), 
                          list(business)), 
                        weight)
)

## -----------------------------------------------------------------------------
aggregate(ms_epr, pias, na.rm = TRUE)

## -----------------------------------------------------------------------------
with(ms_prices, elemental_index(relative, period, business, na.rm = TRUE, r = 1))

## -----------------------------------------------------------------------------
with(ms_prices, elemental_index(relative, period, business, na.rm = TRUE, r = -1))

## -----------------------------------------------------------------------------
ms_prices_sup <- transform(ms_prices, quantity = 10 - price)

## -----------------------------------------------------------------------------
cols <- c("price", "quantity")
back_cols <- paste("back", cols, sep = "_")
ms_prices_sup[back_cols] <- lapply(ms_prices_sup[cols], gpindex::back_price,
                                   period = ms_prices_sup$period, 
                                   product = ms_prices_sup$product)
ms_prices_sup <- na.omit(ms_prices_sup)

f <- interaction(ms_prices_sup$period, ms_prices_sup$business)

ms_prices_sup$weight <- with(
  lapply(ms_prices_sup, split, f),
  unsplit(Map(gpindex::index_weights("Tornqvist"), 
              price, back_price, quantity, back_quantity), 
          f)
)

## -----------------------------------------------------------------------------
with(ms_prices_sup, elemental_index(price / back_price, period, business, weight))

## -----------------------------------------------------------------------------
ms_epr <- with(ms_prices, elemental_index(relative, period, business, contrib = TRUE))

## -----------------------------------------------------------------------------
contrib(ms_epr)

## -----------------------------------------------------------------------------
ms_epr$contrib$`202004`$B3

## -----------------------------------------------------------------------------
ms_prices1 <- subset(ms_prices, period <= "202003")
ms_prices2 <- subset(ms_prices, period >= "202003")

## -----------------------------------------------------------------------------
ms_epr1 <- with(
  ms_prices1, 
  elemental_index(price_relative(price, period, product), period, business, na.rm = TRUE)
)

pias1 <- with(
  ms_weights, 
  aggregation_structure(c(expand_classification(classification), list(business)), weight)
)

(ms_index1 <- aggregate(ms_epr1, pias1, na.rm = TRUE))

## -----------------------------------------------------------------------------
ms_epr2 <- with(
  ms_prices2, 
  elemental_index(price_relative(price, period, product), period, business, na.rm = TRUE)
)

## -----------------------------------------------------------------------------
pias2 <- update(ms_index1)
(ms_index2 <- aggregate(ms_epr2, pias2, na.rm = TRUE))

## -----------------------------------------------------------------------------
(ms_index <- cbind(ms_index1[], ms_index2[, -1]))
apply(ms_index, 1, cumprod)

## -----------------------------------------------------------------------------
ms_prices$relative <- with(
  ms_prices, 
  price_relative(carry_forward(price, period, product), period, product)
)

(ms_epr <- with(ms_prices, elemental_index(relative, period, business, na.rm = TRUE)))

## -----------------------------------------------------------------------------
(ms_index <- aggregate(ms_epr, pias, na.rm = TRUE))

## -----------------------------------------------------------------------------
ms_prices1 <- subset(ms_prices, business %in% c("B1", "B2", "B3"))
ms_prices2 <- subset(ms_prices, business == "B4")

## -----------------------------------------------------------------------------
ms_epr1 <- with(
  ms_prices1, 
  elemental_index(price_relative(price, period, product), period, business, na.rm = TRUE)
)
ms_epr1
ms_epr2 <- with(
  transform(ms_prices2, period = factor(period, levels = ms_epr1$time)), 
  elemental_index(price_relative(price, period, product), period, business, na.rm = TRUE)
)
ms_epr2

## -----------------------------------------------------------------------------
ms_epr <- merge(ms_epr1, ms_epr2)
aggregate(ms_epr, pias, na.rm = TRUE)

## -----------------------------------------------------------------------------
ms_prices2 <- subset(
  as.data.frame(aggregate(ms_epr, pias, na.rm = TRUE)),
  level %in% c("B4", "B5")
)
ms_prices2

## -----------------------------------------------------------------------------
ms_epr2 <- with(ms_prices2, elemental_index(value, period, level))
ms_epr <- merge(ms_epr1, ms_epr2)
aggregate(ms_epr, pias, na.rm = TRUE)

## -----------------------------------------------------------------------------
head(fs_prices)

fs_weights

## -----------------------------------------------------------------------------
fs_prices$relative <- with(fs_prices, price_relative(price, period, business))

fs_epr <- with(
  fs_prices, 
  elemental_index(relative, period, classification, weight, na.rm = TRUE)
)

fs_epr

## -----------------------------------------------------------------------------
pias <- with(
  fs_weights, 
  aggregation_structure(expand_classification(classification), weight)
)

aggregate(fs_epr, pias, na.rm = TRUE)

## -----------------------------------------------------------------------------
weights <- data.frame(period = rep(c("202001", "202002", "202003", "202004"), each = 5),
                      classification = fs_weights$classification,
                      weight = 1:20)
head(weights)

## -----------------------------------------------------------------------------
(fs_epr <- unstack(fs_epr))

## -----------------------------------------------------------------------------
pias <- with(
  weights, 
  Map(aggregation_structure, 
      lapply(split(classification, period), expand_classification), 
      split(weight, period))
)

## -----------------------------------------------------------------------------
(paasche <- Reduce(stack, Map(aggregate, fs_epr, pias, na.rm = TRUE, r = -1)))

## -----------------------------------------------------------------------------
laspeyres <- Reduce(stack, Map(aggregate, fs_epr, pias[c(1, 1, 2, 3)], na.rm = TRUE))
sqrt(as.matrix(laspeyres) * as.matrix(paasche))

