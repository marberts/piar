pias <- with(
  ms_weights, 
  aggregation_structure(c(expand_classification(classification), list(business)),
                        weight)
)

sp <- with(ms_prices, shadow_price(price, period, product, business, pias))

test_that("a length 0 inputs returns a length 0 output", {
  expect_length(carry_forward(integer(0), factor(integer(0), 1:5), integer(0)),
                0)
  expect_length(
    shadow_price(integer(0), integer(0), integer(0), integer(0), pias), 0
  )
})

test_that("imputing shadow prices does noting", {
  expect_equal(
    sp, 
    with(ms_prices, shadow_price(sp, period, product, business, pias))
  )
})

test_that("sp imputation is the same are regular parental in periods 1, 2", {
  epr <- with(
    ms_prices, 
    elemental_index(price_relative(price, period, product), period, business,
                    na.rm = TRUE)
  )

  epr2 <- with(
    ms_prices, 
    elemental_index(price_relative(sp, period, product), period, business,
                    na.rm = TRUE)
  )
  
  # B2 is imputed from above the EA level
  expect_equal(epr[c(1, 3:4), 1:3], epr2[c(1, 3:4), 1:3]) 
  
  expect_equal(aggregate(epr, pias, na.rm = TRUE)[, 1:2], 
            aggregate(epr2, pias, na.rm = TRUE)[, 1:2])
})

test_that("imputing with an improper pias does nothing", {
  # Append a '1' to each business label to make a garbage pias
  pias2 <- with(
    ms_weights, 
    aggregation_structure(c(expand_classification(classification),
                            list(paste0(business, 1))), 
                          weight)
  )
  
  expect_equal(
    ms_prices$price,
    with(ms_prices, shadow_price(price, period, product, business, pias2))
  )
})

test_that("jumbling prices does nothing", {
  jumble <- c(27, 29, 10, 15, 24, 5, 38, 34, 40, 13, 4, 30, 19, 28, 22, 23, 2,
              35, 6, 16, 17, 25, 3, 39, 11, 20, 1, 33, 9, 18, 14, 8, 32, 26, 31,
              12, 21, 36, 7, 37)
  ms_prices <- ms_prices[jumble, ]
  expect_equal(
    with(ms_prices, shadow_price(price, period, product, business, pias)),
    sp[jumble]
  )
})
