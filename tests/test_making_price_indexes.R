### R code from vignette source 'making_price_indexes.Rnw'

###################################################
### code chunk number 1: making_price_indexes.Rnw:29-34
###################################################
library(piar)

head(ms_prices)

ms_weights


###################################################
### code chunk number 2: making_price_indexes.Rnw:39-42
###################################################
relative <- with(ms_prices, price_relative(price, period, product))

ms_epr <- with(ms_prices, elemental_index(relative, period, business, na.rm = TRUE))


###################################################
### code chunk number 3: making_price_indexes.Rnw:49-52
###################################################
ms_epr[]
ms_epr[, "202004"]
ms_epr["B1", ]


###################################################
### code chunk number 4: making_price_indexes.Rnw:57-61
###################################################
pias <- with(
  ms_weights, 
  aggregation_structure(c(expand_classification(classification), list(business)), weight)
)


###################################################
### code chunk number 5: making_price_indexes.Rnw:66-68
###################################################
ms_index <- aggregate(ms_epr, pias, na.rm = TRUE)
ms_index[]


###################################################
### code chunk number 6: making_price_indexes.Rnw:75-76
###################################################
as.matrix(ms_index)


###################################################
### code chunk number 7: making_price_indexes.Rnw:81-82
###################################################
as.data.frame(ms_index)


###################################################
### code chunk number 8: making_price_indexes.Rnw:86-87
###################################################
ms_index$index$`202004`


###################################################
### code chunk number 9: making_price_indexes.Rnw:96-97
###################################################
cumprod(ms_index)


###################################################
### code chunk number 10: making_price_indexes.Rnw:101-102
###################################################
apply(as.matrix(ms_index), 1, cumprod)


###################################################
### code chunk number 11: making_price_indexes.Rnw:110-111
###################################################
(ms_weights <- transform(ms_weights, stratum = c("TS", "TA", "TS", "TS", "TS")))


###################################################
### code chunk number 12: making_price_indexes.Rnw:116-117
###################################################
(classification_sps <- with(ms_weights, paste0(classification, stratum)))


###################################################
### code chunk number 13: making_price_indexes.Rnw:122-127
###################################################
(classification_sps <- expand_classification(classification_sps, width = c(1, 1, 2)))
pias_sps <- with(
  ms_weights, 
  aggregation_structure(c(classification_sps, list(business)), weight)
)


###################################################
### code chunk number 14: making_price_indexes.Rnw:131-132
###################################################
aggregate(ms_epr, pias_sps, na.rm = TRUE)[]


###################################################
### code chunk number 15: making_price_indexes.Rnw:141-143
###################################################
ms_weights$impute <- c("C1", "C1", "C3", "C4", "C5")
ms_weights


###################################################
### code chunk number 16: making_price_indexes.Rnw:148-155
###################################################
pias <- with(
  ms_weights, 
  aggregation_structure(c(expand_classification(classification), 
                          list(impute), 
                          list(business)), 
                        weight)
)


###################################################
### code chunk number 17: making_price_indexes.Rnw:159-161
###################################################
ms_index <- aggregate(ms_epr, pias, na.rm = TRUE)
ms_index[]


###################################################
### code chunk number 18: making_price_indexes.Rnw:168-169
###################################################
with(ms_prices, elemental_index(relative, period, business, na.rm = TRUE, r = 1))[]


###################################################
### code chunk number 19: making_price_indexes.Rnw:174-175
###################################################
with(ms_prices, elemental_index(relative, period, business, na.rm = TRUE, r = -1))[]


###################################################
### code chunk number 20: making_price_indexes.Rnw:181-182
###################################################
ms_prices_sup <- transform(ms_prices, quantity = 10 - price)


###################################################
### code chunk number 21: making_price_indexes.Rnw:187-202
###################################################
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


###################################################
### code chunk number 22: making_price_indexes.Rnw:206-207
###################################################
with(ms_prices_sup, elemental_index(price / back_price, period, business, weight))[]


###################################################
### code chunk number 23: making_price_indexes.Rnw:214-215
###################################################
ms_epr <- with(ms_prices, elemental_index(relative, period, business, contrib = TRUE))


###################################################
### code chunk number 24: making_price_indexes.Rnw:219-222
###################################################
as.matrix(ms_epr, type = "contributions")

as.data.frame(ms_epr, type = "contributions")


###################################################
### code chunk number 25: making_price_indexes.Rnw:226-227
###################################################
ms_epr$contributions$`202004`$B3


###################################################
### code chunk number 26: making_price_indexes.Rnw:237-239
###################################################
ms_prices1 <- subset(ms_prices, period <= "202003")
ms_prices2 <- subset(ms_prices, period >= "202003")


###################################################
### code chunk number 27: making_price_indexes.Rnw:243-255
###################################################
ms_epr1 <- with(
  ms_prices1, 
  elemental_index(price_relative(price, period, product), period, business, na.rm = TRUE)
)

pias1 <- with(
  ms_weights, 
  aggregation_structure(c(expand_classification(classification), list(business)), weight)
)

ms_index1 <- aggregate(ms_epr1, pias1, na.rm = TRUE)
ms_index1[]


###################################################
### code chunk number 28: making_price_indexes.Rnw:260-264
###################################################
ms_epr2 <- with(
  ms_prices2, 
  elemental_index(price_relative(price, period, product), period, business, na.rm = TRUE)
)


###################################################
### code chunk number 29: making_price_indexes.Rnw:268-278
###################################################
ms_weights2 <- transform(
  ms_weights, 
  weight = gpindex::weights_update(cumprod(ms_index1)[business, "202003"], weight)
)
pias2 <- with(
  ms_weights2, 
  aggregation_structure(c(expand_classification(classification), list(business)), weight)
)
ms_index2 <- aggregate(ms_epr2, pias2, na.rm = TRUE)
ms_index2[]


###################################################
### code chunk number 30: making_price_indexes.Rnw:282-284
###################################################
(ms_index <- cbind(ms_index1[], ms_index2[, -1, drop = FALSE]))
apply(ms_index, 1, cumprod)


###################################################
### code chunk number 31: making_price_indexes.Rnw:293-298
###################################################
ms_prices$relative <- with(ms_prices, replace(relative, is.na(relative), 1))

ms_epr <- with(ms_prices, elemental_index(relative, period, business))

ms_epr[]


###################################################
### code chunk number 32: making_price_indexes.Rnw:302-304
###################################################
ms_index <- aggregate(ms_epr, pias, na.rm = TRUE)
ms_index[]


###################################################
### code chunk number 33: making_price_indexes.Rnw:313-315
###################################################
ms_prices1 <- subset(ms_prices, business %in% c("B1", "B2", "B3"))
ms_prices2 <- subset(ms_prices, business == "B4")


###################################################
### code chunk number 34: making_price_indexes.Rnw:320-330
###################################################
ms_epr1 <- with(
  ms_prices1, 
  elemental_index(price_relative(price, period, product), period, business, na.rm = TRUE)
)
ms_epr1[]
ms_epr2 <- with(
  transform(ms_prices2, period = factor(period, levels = ms_epr1$period)), 
  elemental_index(price_relative(price, period, product), period, business, na.rm = TRUE)
)
ms_epr2[drop = FALSE]


###################################################
### code chunk number 35: making_price_indexes.Rnw:334-336
###################################################
ms_epr <- merge(ms_epr1, ms_epr2)
aggregate(ms_epr, pias, na.rm = TRUE)[]


###################################################
### code chunk number 36: making_price_indexes.Rnw:341-346
###################################################
ms_prices2 <- subset(
  as.data.frame(aggregate(ms_epr, pias, na.rm = TRUE)),
  level %in% c("B4", "B5")
)
ms_prices2


###################################################
### code chunk number 37: making_price_indexes.Rnw:350-353
###################################################
ms_epr2 <- with(ms_prices2, elemental_index(value, period, level))
ms_epr <- merge(ms_epr1, ms_epr2)
aggregate(ms_epr, pias, na.rm = TRUE)[]


###################################################
### code chunk number 38: making_price_indexes.Rnw:360-363
###################################################
head(fs_prices)

fs_weights


###################################################
### code chunk number 39: making_price_indexes.Rnw:368-376
###################################################
fs_prices$relative <- with(fs_prices, price_relative(price, period, business))

fs_epr <- with(
  fs_prices, 
  elemental_index(relative, period, classification, weight, na.rm = TRUE)
)

fs_epr[]


###################################################
### code chunk number 40: making_price_indexes.Rnw:380-387
###################################################
pias <- with(
  fs_weights, 
  aggregation_structure(expand_classification(classification), weight)
)

fs_index <- aggregate(fs_epr, pias, na.rm = TRUE)
fs_index[]


