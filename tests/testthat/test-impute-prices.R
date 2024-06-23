pias <- with(
  ms_weights,
  aggregation_structure(c(expand_classification(classification),
                          list(business)),
                        weight)
)

sp <- shadow_price(ms_prices, price ~ period + product + business, pias = pias)

test_that("a length 0 inputs returns a length 0 output", {
  expect_length(
    carry_forward(integer(0), period = factor(integer(0), 1:5), product = integer(0)),
    0
  )
  expect_length(
    shadow_price(integer(0), period = integer(0), product = integer(0), ea = integer(0), pias = pias),
    0
  )
})

test_that("imputing shadow prices does noting", {
  expect_equal(
    sp,
    shadow_price(ms_prices, sp ~ period + product + business, pias = pias)
  )
  
  expect_equal(
    sp,
    shadow_price(ms_prices, sp ~ period + product + business, pias = pias, weights = price)
  )
})

test_that("sp imputation is the same are regular parental in periods 1, 2", {
  epr <- elemental_index(
    ms_prices,
    price_relative(price, period = period, product = product) ~ period + business,
    na.rm = TRUE
  )

  epr2 <- elemental_index(
    ms_prices,
    price_relative(sp, period = period, product = product) ~ period + business,
    na.rm = TRUE
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
    shadow_price(ms_prices, price ~ period + product + business, pias = pias2)
  )
})

test_that("jumbling prices does nothing", {
  jumble <- c(27, 29, 10, 15, 24, 5, 38, 34, 40, 13, 4, 30, 19, 28, 22, 23, 2,
              35, 6, 16, 17, 25, 3, 39, 11, 20, 1, 33, 9, 18, 14, 8, 32, 26, 31,
              12, 21, 36, 7, 37)
  ms_prices <- ms_prices[jumble, ]
  expect_equal(
    shadow_price(ms_prices, price ~ period + product + business, pias = pias),
    sp[jumble]
  )
})

test_that("carrying forward/backwards imputation works", {
  df <- data.frame(price = c(NA, 1, 2, NA, 3), period = gl(5, 1), product = gl(1, 5))
  expect_equal(carry_forward(df, price ~ period + product),
               c(NA, 1, 2, 2, 3))
  expect_equal(carry_backward(df, price ~ period + product),
               c(1, 1, 2, 3, 3))
})
