# Transmuting weights works.
local({
  # No NAs.
  x <- 1:5
  w <- 5:1
  expect_equal(transmute_weights(x, order = 2, to = 2), rep(0.2, 5))
  expect_equal(
    gmean(x, transmute_weights(x, mean = gmean(x, order = 0))),
    gmean(x, order = 0)
  )
  expect_equal(
    gmean(x, transmute_weights(x, order = -2.5, to = 3), 3),
    gmean(x, order = -2.5)
  )

  expect_equal(transmute_weights(x, w, order = -1, to = -1), scale_weights(w))
  expect_equal(gmean(x, transmute_weights(x, w)), gmean(x, w, order = 0))
  expect_equal(
    gmean(x, transmute_weights(x, w, order = 3.25, to = -1), -1),
    gmean(x, w, order = 3.25)
  )

  expect_equal(
    transmute_weights(c(1, 1, 1), 1:3, order = 2, to = 0),
    scale_weights(1:3)
  )

  # With NAs.
  x[1] <- NA
  w[2] <- NA

  expect_equal(transmute_weights(x, order = 2, to = 2), c(NA, rep(0.25, 4)))
  expect_equal(
    gmean(x, transmute_weights(x), na.rm = TRUE),
    gmean(x, order = 0, na.rm = TRUE)
  )
  expect_equal(
    gmean(x, transmute_weights(x, order = -2.5, to = 3), 3, na.rm = TRUE),
    gmean(x, order = -2.5, na.rm = TRUE)
  )

  expect_equal(
    transmute_weights(x, w, order = -1, to = -1),
    scale_weights(replace(w, 1, NA))
  )
  expect_equal(
    gmean(x, transmute_weights(x, w), na.rm = TRUE),
    gmean(x, w, order = 0, na.rm = TRUE)
  )
  expect_equal(
    gmean(x, transmute_weights(x, w, order = 3.25, to = -1), -1, na.rm = TRUE),
    gmean(x, w, order = 3.25, na.rm = TRUE)
  )

  # Errors.
  expect_error(
    transmute_weights(x, 1:4),
    "`x` and `weights` must be the same length"
  )
})

# Nested transmute works.
local({
  # No NAs.
  x <- 1:5
  w1 <- 1:5
  w2 <- c(5, 1, 4, 2, 3)

  expect_equal(
    gmean(x, transmute_weights2(x)),
    nested_gmean(x)
  )
  expect_equal(
    gmean(x, transmute_weights2(x, list(w1, NULL))),
    nested_gmean(x, list(w1, NULL))
  )
  expect_equal(
    gmean(x, transmute_weights2(x, list(NULL, w2))),
    nested_gmean(x, list(NULL, w2))
  )
  expect_equal(
    gmean(x, transmute_weights2(x, list(w1, w2))),
    nested_gmean(x, list(w1, w2))
  )
  expect_equal(
    gmean(x, transmute_weights2(x, list(w1, w2), pivot = 3)),
    nested_gmean(x, list(w1, w2))
  )

  expect_equal(
    transmute_weights2(x, list(w1, w2), outer_weights = c(NA, 1)),
    transmute_weights(x, w2, -1, 1)
  )
  expect_equal(
    transmute_weights2(x, list(w1, w1), order = c(2, 2), to = -1),
    transmute_weights(x, w1, 2, -1)
  )

  # With NAs.
  x[1] <- NA
  w1[2] <- NA

  expect_equal(
    gmean(x, transmute_weights2(x), na.rm = TRUE),
    nested_gmean(x, na.rm = TRUE)
  )
  expect_equal(
    gmean(x, transmute_weights2(x, list(w1, NULL)), na.rm = TRUE),
    nested_gmean(x, list(w1, NULL), na.rm = TRUE)
  )
  expect_equal(
    gmean(x, transmute_weights2(x, list(NULL, w2)), na.rm = TRUE),
    nested_gmean(x, list(NULL, w2), na.rm = TRUE)
  )
  expect_equal(
    gmean(x, transmute_weights2(x, list(w1, w2)), na.rm = TRUE),
    nested_gmean(x, list(w1, w2), na.rm = TRUE)
  )
  expect_equal(
    gmean(x, transmute_weights2(x, list(w1, w2), pivot = 3), na.rm = TRUE),
    nested_gmean(x, list(w1, w2), na.rm = TRUE)
  )

  expect_equal(
    transmute_weights2(x, list(w1, w2), outer_weights = c(NA, 1)),
    transmute_weights(x, replace(w2, 2, NA), -1, 1)
  )
  expect_equal(
    transmute_weights2(x, list(w1, w1), order = c(2, 2), to = -1),
    transmute_weights(x, w1, 2, -1)
  )

  # Errors.
  expect_error(
    transmute_weights2(x, list(1:4, NULL)),
    "`x` and non-NULL components of `weights` must be the same length"
  )
})
