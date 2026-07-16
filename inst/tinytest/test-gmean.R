# Generalized mean works.
local({
  # Corner cases.
  expect_equal(gmean(c(0, 1, 2), order = -1), 0)
  expect_equal(gmean(c(Inf, 1, 2), order = 2), Inf)

  # No NAs.
  x <- 1:5
  w <- 5:1

  expect_equal(gmean(x, w), weighted.mean(x, w))
  expect_equal(gmean(x, w, order = -1), 1 / weighted.mean(1 / x, w))
  expect_equal(gmean(x, w, order = 0), exp(weighted.mean(log(x), w)))
  expect_equal(gmean(x, w, order = 2), sqrt(weighted.mean(x^2, w)))

  expect_equal(gmean(x), weighted.mean(x))
  expect_equal(gmean(x, order = -1), 1 / weighted.mean(1 / x))
  expect_equal(gmean(x, order = 0), exp(weighted.mean(log(x))))
  expect_equal(gmean(x, order = 2), sqrt(weighted.mean(x^2)))

  # With NAs.
  x[1] <- NA
  w[2] <- NA

  xx <- x[3:5]
  ww <- w[3:5]

  expect_equal(gmean(x, w, na.rm = TRUE), weighted.mean(xx, ww))
  expect_equal(
    gmean(x, w, order = -1, na.rm = TRUE),
    1 / weighted.mean(1 / xx, ww)
  )
  expect_equal(
    gmean(x, w, order = 0, na.rm = TRUE),
    exp(weighted.mean(log(xx), ww))
  )
  expect_equal(
    gmean(x, w, order = 2, na.rm = TRUE),
    sqrt(weighted.mean(xx^2, ww))
  )

  expect_equal(gmean(x, na.rm = TRUE), weighted.mean(x, na.rm = TRUE))
  expect_equal(
    gmean(x, order = -1, na.rm = TRUE),
    1 / weighted.mean(1 / x, na.rm = TRUE)
  )
  expect_equal(
    gmean(x, order = 0, na.rm = TRUE),
    exp(weighted.mean(log(x), na.rm = TRUE))
  )
  expect_equal(
    gmean(x, order = 2, na.rm = TRUE),
    sqrt(weighted.mean(x^2, na.rm = TRUE))
  )

  # Errors.
  expect_error(gmean(x, order = NA), "`order` must be a finite number")
  expect_error(
    gmean(x, weights = 1),
    "`x` and `weights` must be the same length"
  )
})

# Nested generalized means works.
local({
  # No NAs.
  x <- 5:1
  w1 <- c(1, 2, 1, 2, 1)
  w2 <- c(1, 1, 2, 2, 3)

  expect_equal(nested_gmean(x), sqrt(weighted.mean(x) / weighted.mean(1 / x)))
  expect_equal(
    nested_gmean(x, list(w1, NULL)),
    sqrt(weighted.mean(x, w1) / weighted.mean(1 / x))
  )
  expect_equal(
    nested_gmean(x, list(NULL, w2)),
    sqrt(weighted.mean(x) / weighted.mean(1 / x, w2))
  )
  expect_equal(
    nested_gmean(x, list(w1, w2)),
    sqrt(weighted.mean(x, w1) / weighted.mean(1 / x, w2))
  )
  expect_equal(
    nested_gmean(x, outer_weights = 1:2, outer_order = 1),
    weighted.mean(c(weighted.mean(x), 1 / weighted.mean(1 / x)), 1:2)
  )

  # With NAs.
  x[1] <- NA
  w1[2] <- NA

  expect_equal(
    nested_gmean(x, list(NULL, w2), na.rm = TRUE),
    sqrt(weighted.mean(x[-1]) / weighted.mean(1 / x[-1], w2[-1]))
  )
  expect_equal(
    nested_gmean(x, list(w1, w2), na.rm = TRUE),
    sqrt(
      weighted.mean(x[-(1:2)], w1[-(1:2)]) /
        weighted.mean(1 / x[-(1:2)], w2[-(1:2)])
    )
  )
  expect_equal(
    nested_gmean(x, list(w1, w2), outer_weights = c(NA, 1), na.rm = TRUE),
    gmean(x[-(1:2)], w2[-(1:2)], order = -1)
  )

  # Errors.
  expect_error(
    nested_gmean(1:5, list(NULL, 1:4)),
    "`x` and non-NULL components of `weights` must be the same length"
  )

  expect_error(
    nested_gmean(1:5, order = NA),
    "`order` must be a pair of finite numbers"
  )
})
