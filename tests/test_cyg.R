library(piar)

wd <- getwd()

# Matched sample

ms_cyg <- read.csv(file.path(wd, "ms_cyg.csv"))

pias <- with(
  ms_weights, 
  aggregation_structure(c(expand_classification(classification), list(business)), weight)
)

sp <- with(ms_prices, shadow_price(price, period, product, business, pias))

rel <- with(ms_prices, price_relative(sp, period, product))

epr <- with(ms_prices, elemental_index(rel, period, business, na.rm = TRUE))

index <- aggregate(epr, pias, na.rm = TRUE)

all.equal(as.numeric(as.matrix(cumprod(index))), ms_cyg$index / 100)

# Fixed sample

# fs_cyg <- read.csv(file.path(wd, "tests", "fs_cyg.csv"))
# 
# pias <- with(
#   fs_weights, 
#   aggregation_structure(expand_classification(classification), weight)
# )
# 
# rel <- with(fs_prices, price_relative(price, period, business))
# 
# epr <- with(fs_prices, elemental_index(rel, period, classification, weight, na.rm = TRUE, r = 1))
# 
# index <- aggregate(epr, pias, na.rm = TRUE)
# 
# all.equal(as.numeric(as.matrix(cumprod(index))), fs_cyg$index / 100)
