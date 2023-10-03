test_that("aggregating over subperiods works", {
  ms_epr <- with(
    ms_prices,
    elemental_index(price_relative(price, period, product),
                    period, business, contrib = TRUE, na.rm = TRUE)
  )

  epr2 <- mean(ms_epr, window = 2)
  expect_identical(levels(epr2), levels(ms_epr))
  expect_identical(time(epr2), c("202001", "202003"))

  expect_equal(as.matrix(epr2)[, 1], rowMeans(as.matrix(ms_epr)[, 1:2]))
  expect_equal(as.matrix(epr2)[, 2], rowMeans(as.matrix(ms_epr)[, 3:4]))

  expect_true(is_chainable_index(epr2))
  expect_false(is_chainable_index(mean(chain(ms_epr))))
  expect_equal(
    contrib(epr2),
    matrix(numeric(0), 0, 2, dimnames = list(NULL, c("202001", "202003")))
  )

  w <- matrix(seq_len(4 * 4), 4)
  expect_equal(
    as.matrix(mean(ms_epr, w, window = 2))[, 1],
    diag(as.matrix(ms_epr)[, 1:2] %*% apply(w[, 1:2], 1, scale_weights)),
    ignore_attr = TRUE
  )
  expect_equal(
    as.matrix(mean(ms_epr, w, window = 2))[, 2],
    diag(as.matrix(ms_epr)[, 3:4] %*% apply(w[, 3:4], 1, scale_weights)),
    ignore_attr = TRUE
  )
})

test_that("aggregating with a window of 1 does nothing", {
  x <- as_index(matrix(1:9, 3))
  expect_equal(mean(x, window = 1), x)
  expect_equal(mean(x, 9:1, window = 1, r = 0), x)
})

test_that("mean requires a suitable window", {
  index <- as_index(matrix(1:9, 3))
  expect_error(mean(index, window = 4))
  expect_error(mean(index, window = 0))
})
