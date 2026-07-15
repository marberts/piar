local({
  x <- 1:5
  w <- 5:1

  # No NAs.
  expect_equal(update_weights(x, w), w * x)
  expect_equal(update_weights(x, w, order = -1), w / x)
  expect_equal(update_weights(x, w, order = 0), w)

  expect_equal(update_weights(x), x)
  expect_equal(update_weights(x, order = -1), 1 / x)
  expect_equal(update_weights(x, order = 0), rep(1, 5))

  # With NAs.
  x[1] <- NA
  w[2] <- NA

  expect_equal(update_weights(x, w), w * x)
  expect_equal(update_weights(x, w, order = -1), w / x)
  expect_equal(update_weights(x, w, order = 0), replace(w, 1, NA))

  expect_equal(update_weights(x), x)
  expect_equal(update_weights(x, order = -1), 1 / x)
  expect_equal(update_weights(x, order = 0), c(NA, rep(1, 4)))

  # Errors.
  expect_error(update_weights(x, order = NA), "`order` must be a finite number")
  expect_error(
    update_weights(x, weights = 1),
    "`x` and `weights` must be the same length"
  )
})
