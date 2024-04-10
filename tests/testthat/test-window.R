test_that("window works", {
  x <- as_index(matrix(1:9, 3))
  expect_equal(window(x), x)
  expect_equal(window(x, "2"), x[, 2:3])
  expect_equal(window(x, end = "2"), x[, 1:2])
  expect_equal(window(x, "2", "2"), x[, 2])
})

test_that("window replacement works", {
  x <- as_index(matrix(1:9, 3))
  window(x, "2") <- 1
  expect_equal(x, as_index(matrix(c(1:3, rep(1, 6)), 3)))
})

test_that("window errors when expected", {
  x <- as_index(matrix(1:9, 3))
  expect_error(window(x, "2", "1"))
  expect_error(window(x, "4"))
  expect_error(window(x, end = "4"))
})
