# Combining classifications works.
local({
  x <- c("111", "121", "122")
  y <- c("01", "02", "01")
  expect_identical(
    combine_classifications(
      expand_classification(x),
      expand_classification(y)
    ),
    expand_classification(paste(x, y, sep = "."), c(1, 1, 1, 2, 1))
  )
})

# Corner cases work.
local({
  x <- list(c("11", "12"), c("111", "121"))
  expect_identical(combine_classifications(x, list()), x)
  expect_identical(combine_classifications(list(), x), x)
  expect_identical(combine_classifications(list(), list()), list())
})

# Combining classifications errors with unequal length.
local({
  expect_error(
    combine_classifications(
      list(c("1", "1"), c("11", "12")),
      list(c("01", "02"), c("011"))
    )
  )
})
