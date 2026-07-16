x <- c(1, 2, 1, 0.5, 1, 10, 1, 0.5, 0.2, 0.05)

# Outlier methods work.
local({
  expect_equal(
    outliers(x, upper = 2.5, method = "quartile"),
    x > median(x) + (quantile(x, 0.75) - quantile(x, 0.5)) * 2.5 |
      x < median(x) - (quantile(x, 0.5) - quantile(x, 0.25)) * 2.5
  )
  expect_equal(
    outliers(x, upper = 2.5, method = "resistant-fences"),
    x > quantile(x, 0.75) + (quantile(x, 0.75) - quantile(x, 0.25)) * 2.5 |
      x < quantile(x, 0.25) - (quantile(x, 0.75) - quantile(x, 0.25)) * 2.5
  )
  expect_equal(
    outliers(x, upper = 2.5, method = "kimber"),
    x > quantile(x, 0.75) + (quantile(x, 0.75) - quantile(x, 0.5)) * 2.5 |
      x < quantile(x, 0.25) - (quantile(x, 0.5) - quantile(x, 0.25)) * 2.5
  )
  expect_true(
    sum(outliers(x, upper = 2.5, method = "resistant-fences")) <=
      sum(outliers(x, upper = 2.5, method = "quartile"))
  )
  expect_equal(
    outliers(x, upper = 2.5, method = "robust-z"),
    abs(x - median(x)) / mad(x) > 2.5
  )

  expect_equal(outliers(integer(0), upper = 1, method = "tukey"), logical(0))
  expect_equal(outliers(2, upper = 2.5, method = "tukey"), FALSE)
  expect_equal(
    outliers(seq(0.1, 2, by = 0.2), upper = 2.5, method = "tukey"),
    c(TRUE, rep(FALSE, 8), TRUE)
  )
  expect_equal(
    outliers(c(NA, 1, 2, 3), upper = 2.5, method = "tukey"),
    c(NA, TRUE, FALSE, TRUE)
  )
})

# Outliers work with NAs.
local({
  expect_identical(
    outliers(x, upper = 1, method = "resistant-fences"),
    outliers(c(NA, x), upper = 1, method = "resistant-fences")[-1]
  )
  expect_identical(
    outliers(x, upper = 1, method = "quartile"),
    outliers(c(NA, x), upper = 1, method = "quartile")[-1]
  )
  expect_identical(
    outliers(x, upper = 1, method = "robust-z"),
    outliers(c(NA, x), upper = 1, method = "robust-z")[-1]
  )
  expect_identical(
    outliers(x, upper = 1, method = "tukey"),
    outliers(c(NA, x), upper = 1, method = "tukey")[-1]
  )
  expect_identical(
    outliers(x, upper = 1, method = "kimber"),
    outliers(c(NA, x), upper = 1, method = "kimber")[-1]
  )
})

# HB transform.
local({
  expect_equal(
    hb_transform(x),
    ifelse(x < median(x), 1 - median(x) / x, x / median(x) - 1)
  )
  expect_equal(hb_transform(x), hb_transform(c(NA, x)[-1]))
})

# Recycling gives an error.
local({
  expect_error(outliers(x, upper = 1, lower = rep(2.5, 10)))
  expect_error(outliers(x, upper = rep(2.5, 0)))
  expect_error(outliers(x, upper = 3, scale = rep(0, 11)))
})
