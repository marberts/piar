id <- letters[c(5:1, 1:5)]
period <- rep(1:2, each = 5)

# Offsetting periods works.
local({
  # Length 0 inputs
  expect_identical(back_period(NULL, NULL, offset = 0), integer(0))
  expect_identical(
    back_period(numeric(0), factor(numeric(0), 1:3), offset = 0),
    integer(0)
  )
  expect_identical(
    back_period(factor(numeric(0), 1:3), numeric(0), offset = 0),
    integer(0)
  )
  expect_identical(
    back_period(factor(numeric(0), 1:3), factor(numeric(0), 1:3), offset = 0),
    integer(0)
  )

  # Simple cases
  expect_identical(back_period(1:4), c(1L, 1L, 2L, 3L))
  expect_identical(back_period(1:4, offset = 4), rep(1L, 4))

  # Attributes shouldn't do anything
  expect_identical(back_period(matrix(1:4)), c(1L, 1L, 2L, 3L))

  # Change time periods
  expect_identical(
    back_period(factor(rep(1, 4), levels = 0:1), 1:4),
    rep(NA_integer_, 4)
  )
  expect_identical(back_period(factor(1:4, levels = 4:1)), c(2L, 3L, 4L, 4L))

  # A more interesting case
  expect_identical(
    back_period(period, id),
    c(1L, 2L, 3L, 4L, 5L, 5L, 4L, 3L, 2L, 1L)
  )
  expect_identical(
    back_period(period, id, offset = 2),
    c(1L, 2L, 3L, 4L, 5L, 5L, 4L, 3L, 2L, 1L)
  )
  expect_identical(
    back_period(replace(period, 2, NA), id),
    c(1L, NA, 3L, 4L, 5L, 5L, 4L, 3L, NA, 1L)
  )
  expect_identical(
    back_period(period[-1], id[-1]),
    c(1L, 2L, 3L, 4L, 4L, 3L, 2L, 1L, NA)
  )
  expect_identical(
    back_period(factor(period, levels = NA), id, offset = 0),
    1:10
  )

  # Change time periods again
  expect_identical(
    back_period(factor(period, c(1, 3, 2)), id),
    c(1L, 2L, 3L, 4L, 5L, NA, NA, NA, NA, NA)
  )
  expect_identical(
    back_period(factor(period, c(1, 3, 2)), id, offset = 2),
    c(1L, 2L, 3L, 4L, 5L, 5L, 4L, 3L, 2L, 1L)
  )

  # NA products shouldn't trigger a warning
  expect_identical(
    back_period(period, replace(id, 1:2, NA)),
    c(NA, NA, 3L, 4L, 5L, 5L, 4L, 3L, NA, NA)
  )
})

# Not matching the first period works.
local({
  expect_identical(back_period(period, id, FALSE, offset = 0), 1:10)
  expect_identical(back_period(period, id, FALSE), c(rep(NA, 5), 5:1))
  expect_identical(
    back_period(period, id, FALSE, offset = 2),
    rep(NA_integer_, 10)
  )
})

# Warnings and errors.
local({
  expect_warning(
    back_period(c(1, 1, 2, 3)),
    "there are duplicated period-product pairs"
  )
  expect_error(
    back_period(1:5, 1:4),
    "`period` and `product` must be the same length"
  )
  expect_error(
    back_period(c(1, 1, 2, 2), c(1, 2, 1, 2), offset = 3),
    "`offset` must be a positive integer less than `nlevels\\(period\\)`"
  )
})
