pias <- with(
  ms_weights,
  aggregation_structure(
    c(expand_classification(classification), list(business)),
    weight
  )
)

sp <- impute_prices(
  ms_prices,
  price ~ period + product,
  ea = business,
  pias = pias,
  method = "overall-mean"
)

test_that("a length 0 inputs returns a length 0 output", {
  expect_length(
    impute_prices(
      integer(0),
      period = factor(integer(0), 1:5),
      product = integer(0),
      method = "carry-forward"
    ),
    0
  )
  expect_length(
    impute_prices(
      integer(0),
      period = integer(0),
      product = integer(0),
      ea = integer(0),
      pias = pias,
      method = "overall-mean"
    ),
    0
  )
})

test_that("imputing shadow prices does noting", {
  expect_equal(
    sp,
    impute_prices(
      ms_prices,
      sp ~ period + product,
      ea = business,
      pias = pias,
      method = "overall-mean"
    )
  )

  expect_equal(
    sp,
    impute_prices(
      ms_prices,
      sp ~ period + product,
      ea = business,
      pias = pias,
      weights = price,
      method = "overall-mean"
    )
  )
})

test_that("sp imputation is the same are regular parental in periods 1, 2", {
  epr <- elementary_index(
    ms_prices,
    price_relative(price, period = period, product = product) ~
      period + business,
    na.rm = TRUE
  )

  epr2 <- elementary_index(
    ms_prices,
    price_relative(sp, period = period, product = product) ~ period + business,
    na.rm = TRUE
  )

  # B2 is imputed from above the EA level
  expect_equal(epr[c(1, 3:4), 1:3], epr2[c(1, 3:4), 1:3])

  expect_equal(
    aggregate(epr, pias, na.rm = TRUE)[, 1:2],
    aggregate(epr2, pias, na.rm = TRUE)[, 1:2]
  )
})

test_that("imputing with an improper pias does nothing", {
  # Append a '1' to each business label to make a garbage pias
  pias2 <- with(
    ms_weights,
    aggregation_structure(
      c(expand_classification(classification), list(paste0(business, 1))),
      weight
    )
  )

  expect_equal(
    ms_prices$price,
    impute_prices(
      ms_prices,
      price ~ period + product,
      ea = business,
      pias = pias2,
      method = "overall-mean"
    )
  )
})

test_that("jumbling prices does nothing", {
  jumble <- c(
    27,
    29,
    10,
    15,
    24,
    5,
    38,
    34,
    40,
    13,
    4,
    30,
    19,
    28,
    22,
    23,
    2,
    35,
    6,
    16,
    17,
    25,
    3,
    39,
    11,
    20,
    1,
    33,
    9,
    18,
    14,
    8,
    32,
    26,
    31,
    12,
    21,
    36,
    7,
    37
  )
  ms_prices <- ms_prices[jumble, ]
  expect_equal(
    impute_prices(
      ms_prices,
      price ~ period + product,
      ea = business,
      pias = pias,
      method = "overall-mean"
    ),
    sp[jumble]
  )
})

test_that("carrying forward/backwards imputation works", {
  df <- data.frame(
    price = c(NA, 1, 2, NA, 3),
    period = gl(5, 1),
    product = gl(1, 5)
  )
  expect_equal(
    impute_prices(df, price ~ period + product, method = "carry-forward"),
    c(NA, 1, 2, 2, 3)
  )
  expect_equal(
    impute_prices(df, price ~ period + product, method = "carry-backward"),
    c(1, 1, 2, 3, 3)
  )
})

test_that("imputing with a matrix works", {
  ms_prices$back_price <- with(
    ms_prices,
    price[gpindex::back_period(period, product)]
  )
  sp2 <- impute_prices(
    ms_prices,
    cbind(price, back_price) ~ period + product,
    ea = business,
    pias = pias,
    method = "overall-mean"
  )
  expect_equal(sp2[, 1L], sp)
  expect_equal(
    impute_prices(
      matrix(c(1, NA, NA, 4, 5, 6, 3, 2, 1, 1, NA, 3), ncol = 2),
      rep(1:2, each = 3),
      rep(1:3, 2),
      method = "carry-forward"
    ),
    matrix(c(1, 2, 1, 4, 5, 6, 3, 2, 1, 1, 2, 3), ncol = 2)
  )
})
