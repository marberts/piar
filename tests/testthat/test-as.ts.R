test_that("as.ts works with an index", {
  index <- matrix(1:9, 3, dimnames = list(letters[1:3], NULL))

  expect_equal(as.ts(as_index(index)), as.ts(t(index)))

  expect_equal(
    as.ts(as_index(index), start = 2020, frequency = 12),
    ts(t(index), start = 2020, frequency = 12)
  )
})
