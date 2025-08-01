library(gpindex)

ms_prices$rel <- price_relative(ms_prices, price ~ period + product)
ms_prices$w1 <- 1:40
ms_prices$w2 <- 40:1

epr1 <- elemental_index(ms_prices, rel ~ period + business, contrib = TRUE)

epr2 <- elemental_index(
  ms_prices, rel ~ period + business,
  r = -1, contrib = TRUE, na.rm = TRUE
)

# Test a Fisher calculation
fw <- function(x, w1, w2) {
  v1 <- scale_weights(transmute_weights(1, 0)(x, w1))
  v2 <- scale_weights(transmute_weights(-1, 0)(x, w2))
  v1 + v2
}

ms_prices2 <- na.omit(ms_prices)

w <- with(
  ms_prices2,
  grouped(fw)(rel, w1, w2, group = interaction(period, business))
)

epr3 <- elemental_index(
  ms_prices2,
  rel ~ period + business,
  weights = w, contrib = TRUE
)

test_that("results agree with an alternate implementation", {
  epr11 <- aggregate(rel ~ as.character(business) + period, ms_prices,
    function(x) exp(weighted.mean(log(x))),
    na.action = na.pass, drop = FALSE
  )
  epr22 <- aggregate(rel ~ as.character(business) + period, ms_prices,
    function(x) 1 / weighted.mean(1 / x),
    na.action = na.omit, drop = FALSE
  )
  names(epr11) <- names(epr22) <- c("level", "period", "value")

  expect_equal(as.data.frame(epr1), epr11[c(2, 1, 3)])
  expect_equal(as.data.frame(epr2), epr22[c(2, 1, 3)])
})

test_that("Fisher calculation agrees with manual calculation", {
  l <- elemental_index(ms_prices2, rel ~ period + business, weights = w1, r = 1)
  p <- elemental_index(ms_prices2, rel ~ period + business, weights = w2, r = -1)
  expect_equal(sqrt(as.matrix(l) * as.matrix(p)), as.matrix(epr3))

  # Should work for other kinds of superlative indexes
  fw <- function(x, w1, w2) {
    v1 <- scale_weights(transmute_weights(1.5, 0)(x))
    v2 <- scale_weights(transmute_weights(-1.5, 0)(x, w2))
    v1 + v2
  }

  w <- with(
    ms_prices2,
    grouped(fw)(rel, w1, w2, group = interaction(period, business))
  )

  sepr <- elemental_index(ms_prices2, rel ~ period + business, weights = w)

  l <- elemental_index(ms_prices2, rel ~ period + business, r = 1.5)
  p <- elemental_index(ms_prices2, rel ~ period + business, weights = w2, r = -1.5)
  expect_equal(sqrt(as.matrix(l) * as.matrix(p)), as.matrix(sepr))
})

test_that("contributions add up", {
  sum_contrib <- \(x, ...) sum(x, ...) / (sum(!is.na(x)) > 0)
  expect_equal(
    epr1$index,
    lapply(epr1$contrib, \(x) sapply(x, sum_contrib) + 1)
  )

  expect_equal(
    epr2$index,
    lapply(epr2$contrib, function(x) sapply(x, sum_contrib, na.rm = TRUE) + 1)
  )

  expect_equal(
    epr3$index,
    lapply(epr3$contrib, function(x) sapply(x, sum_contrib) + 1)
  )
})

test_that("argument checking works", {
  expect_error(elemental_index(1:3, period = 1:2, ea = 1:3))
  expect_error(elemental_index(1:3, period = 1:3, ea = 1:4))
  expect_error(elemental_index(1:3, period = 1:3, ea = 1:3, weights = 1:2))
  expect_error(elemental_index(1:3, period = factor(1:3, levels = numeric(0))))
  expect_error(elemental_index(1:3, ea = factor(1:3, levels = numeric(0))))
  expect_error(elemental_index(setNames(1:3, c("", 1, 2)), contrib = TRUE))
  expect_error(elemental_index(-1:1, period = 1:3, ea = 1:3, r = 1))
  expect_warning(elemental_index(
    setNames(1:3, rep(1, 3)),
    period = gl(1, 3), ea = gl(1, 3), contrib = TRUE
  ))
})

test_that("nse works", {
  expect_equal(
    elemental_index(ms_prices, rel ~ period:business, contrib = TRUE),
    epr1
  )
  expect_equal(
    elemental_index(ms_prices[c(1:2, 5)], rel ~ ., contrib = TRUE),
    epr1
  )
  expect_error(elemental_index(ms_prices, rel ~ .))
  expect_error(elemental_index(ms_prices, ~ period + business))
})
