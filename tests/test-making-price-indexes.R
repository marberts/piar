## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(piar)

head(ms_prices)

ms_weights

## -----------------------------------------------------------------------------
relatives <- with(
  ms_prices,
  price_relative(price, period = period, product = product)
)

ms_elemental <- with(
  ms_prices,
  elemental_index(relatives, period = period, ea = business, na.rm = TRUE)
)

ms_elemental

## -----------------------------------------------------------------------------
ms_elemental[, "202004"]

ms_elemental["B1", ]

## -----------------------------------------------------------------------------
hierarchy <- with(
  ms_weights, 
  c(expand_classification(classification), list(business))
)

pias <- aggregation_structure(hierarchy, weights = ms_weights$weight)

## -----------------------------------------------------------------------------
ms_index <- aggregate(ms_elemental, pias, na.rm = TRUE)

ms_index

## -----------------------------------------------------------------------------
ms_index_chained <- chain(ms_index)

ms_index_chained

## -----------------------------------------------------------------------------
t(apply(as.matrix(ms_index), 1, cumprod))

## -----------------------------------------------------------------------------
rebase(ms_index_chained, ms_index_chained[, "202004"])

## -----------------------------------------------------------------------------
rebase(
  ms_index_chained,
  mean(window(ms_index_chained, "202003"))
)

## -----------------------------------------------------------------------------
ms_weights$stratum <- c("TS", "TA", "TS", "TS", "TS")

ms_weights

## -----------------------------------------------------------------------------
classification_sps <- with(ms_weights, paste0(classification, stratum))

classification_sps

## -----------------------------------------------------------------------------
classification_sps <- expand_classification(
  classification_sps,
  width = c(1, 1, 2)
)

pias_sps <- with(
  ms_weights, 
  aggregation_structure(c(classification_sps, list(business)), weight)
)

## -----------------------------------------------------------------------------
index_sps <- aggregate(ms_elemental, pias_sps, na.rm = TRUE)
index_sps

## -----------------------------------------------------------------------------
interacted_hierarchy <- with(
  ms_weights,
  interact_classifications(
    expand_classification(classification),
    expand_classification(stratum)
  )
)

pias_sps2 <- lapply(
  interacted_hierarchy,
  \(x) aggregation_structure(c(x, ms_weights["business"]), ms_weights$weight)
)

index_sps2 <- lapply(pias_sps2, \(x) aggregate(index_sps, x, include_ea = FALSE))

## -----------------------------------------------------------------------------
Reduce(merge, index_sps2)

## -----------------------------------------------------------------------------
pias_matrix <- as.matrix(pias)
pias_matrix

## -----------------------------------------------------------------------------
pias_matrix %*% as.matrix(chain(ms_index[ms_weights$business]))

## -----------------------------------------------------------------------------
ms_elemental2 <- with(
  ms_prices,
  elemental_index(
    relatives, period, factor(business, ms_weights$business),
    na.rm = TRUE
  )
)

## -----------------------------------------------------------------------------
pias_matrix <- as.matrix(pias) > 0 
pias_matrix %*% is.na(ms_elemental2) / rowSums(pias_matrix)

## -----------------------------------------------------------------------------
ms_elemental2 <- ms_elemental
ms_elemental2["B2", 2:3] <- 1
ms_elemental2

## -----------------------------------------------------------------------------
aggregate(ms_elemental2, pias, na.rm = TRUE)

## -----------------------------------------------------------------------------
with(
  ms_prices, 
  elemental_index(relatives, period, business, na.rm = TRUE, r = 1)
)

## -----------------------------------------------------------------------------
with(
  ms_prices,
  elemental_index(relatives, period, business, na.rm = TRUE, r = -1)
)

## -----------------------------------------------------------------------------
ms_prices2 <- transform(ms_prices, quantity = 10 - price)

## -----------------------------------------------------------------------------
library(gpindex)

tw <- grouped(index_weights("Tornqvist"))

ms_prices2[c("back_price", "back_quantity")] <- 
  ms_prices2[back_period(ms_prices2$period, ms_prices2$product),
             c("price", "quantity")]

ms_prices2 <- na.omit(ms_prices2) # can't have NAs for Tornqvist weights

ms_prices2$weight <- with(
  ms_prices2,
  tw(
    price, back_price, quantity, back_quantity,
    group = interaction(period, business)
  )
)

## -----------------------------------------------------------------------------
with(
  ms_prices2,
  elemental_index(price / back_price, period, business, weight)
)

## -----------------------------------------------------------------------------
ms_elemental <- with(
  ms_prices,
  elemental_index(relatives, period, business, contrib = TRUE, na.rm = TRUE)
)

## -----------------------------------------------------------------------------
contrib(ms_elemental)

## -----------------------------------------------------------------------------
contrib(aggregate(ms_elemental, pias, na.rm = TRUE))

## -----------------------------------------------------------------------------
ms_prices1 <- subset(ms_prices, period <= "202003")
ms_prices2 <- subset(ms_prices, period >= "202003")

## -----------------------------------------------------------------------------
ms_elemental1 <- with(
  ms_prices1, 
  elemental_index(
    price_relative(price, period, product),
    period, business, na.rm = TRUE
  )
)

ms_index1 <- aggregate(ms_elemental1, pias, na.rm = TRUE)

ms_index1

## -----------------------------------------------------------------------------
ms_elemental2 <- ms_prices2 |>
  transform(rel = price_relative(price, period, product)) |>
  subset(period > "202003") |>
  with(elemental_index(rel, period, business, na.rm = TRUE))

## -----------------------------------------------------------------------------
ms_index2 <- aggregate(ms_elemental2, update(pias, ms_index1), na.rm = TRUE)

ms_index2

## -----------------------------------------------------------------------------
chain(stack(ms_index1, ms_index2))

## -----------------------------------------------------------------------------
ms_elemental2 <- with(
  ms_prices, 
  elemental_index(
    price_relative(carry_forward(price, period, product), period, product),
    period, business, na.rm = TRUE
  )
)

ms_elemental2

## -----------------------------------------------------------------------------
ms_index <- aggregate(ms_elemental2, pias, na.rm = TRUE)
 
ms_index

## -----------------------------------------------------------------------------
ms_prices1 <- subset(ms_prices, business %in% c("B1", "B2", "B3"))
ms_prices2 <- subset(ms_prices, business == "B4")

## -----------------------------------------------------------------------------
ms_elemental1 <- with(
  ms_prices1, 
  elemental_index(
    price_relative(price, period, product),
    period, business, na.rm = TRUE
  )
)

ms_elemental1

ms_elemental2 <- ms_prices2 |>
  transform(period = factor(period, levels = time(ms_elemental1))) |>
  with(
    elemental_index(
      price_relative(price, period, product),
      period, business, na.rm = TRUE
    )
  )

ms_elemental2

## -----------------------------------------------------------------------------
aggregate(merge(ms_elemental1, ms_elemental2), pias, na.rm = TRUE)

## -----------------------------------------------------------------------------
ms_prices2 <- subset(
  as.data.frame(aggregate(ms_elemental, pias, na.rm = TRUE)),
  level %in% c("B4", "B5")
)

ms_prices2

## -----------------------------------------------------------------------------
ms_elemental2 <- as_index(ms_prices2)

aggregate(merge(ms_elemental1, ms_elemental2), pias, na.rm = TRUE)

## -----------------------------------------------------------------------------
weights <- data.frame(
  period = rep(c("202001", "202002", "202003", "202004"), each = 5),
  classification = ms_weights$classification,
  weight = 1:20
)

head(weights)

## -----------------------------------------------------------------------------
ms_elemental <- unstack(ms_elemental)

ms_elemental

## -----------------------------------------------------------------------------
pias <- with(
  weights, 
  Map(aggregation_structure, list(hierarchy), split(weight, period))
)

## -----------------------------------------------------------------------------
paasche <- Reduce(
  stack,
  Map(aggregate, ms_elemental, pias, na.rm = TRUE, r = -1)
)

paasche

## -----------------------------------------------------------------------------
laspeyres <- Reduce(
  stack,
  Map(aggregate, ms_elemental, pias[c(1, 1, 2, 3)], na.rm = TRUE)
)

fisher <- sqrt(as.matrix(laspeyres) * as.matrix(paasche))

fisher

## -----------------------------------------------------------------------------
geometric_weights <- transmute_weights(0, 1)

w <- mapply(
  \(x, y) scale_weights(geometric_weights(c(x, y))),
  as.numeric(laspeyres[1]),
  as.numeric(paasche[1])
)

laspeyres_contrib <- contrib(laspeyres)
paasche_contrib <- contrib(paasche)

fisher_contrib <- w[1, col(laspeyres_contrib)] * laspeyres_contrib +
  w[2, col(paasche_contrib)] * paasche_contrib

fisher_contrib

## -----------------------------------------------------------------------------
chain(fisher)

## -----------------------------------------------------------------------------
sqrt(as.matrix(chain(laspeyres)) * as.matrix(chain(paasche)))

