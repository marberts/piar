index <- as_index(matrix(1:4, 2))

test_that("coercion methods work", {
  expect_equal(as.matrix(index),
               matrix(1:4, 2, dimnames = list(1:2, 1:2)))
  expect_equal(
    as.data.frame(index),
    data.frame(period = as.character(rep(1:2, each = 2)),
               level = as.character(1:2),
               value = 1:4)
  )
})
