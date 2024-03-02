test_that("splitting works", {
  x <- as_index(matrix(1:6, 2))
  expect_equal(
    split(x, 1:2),
    list("1" = as_index(matrix(c(1, 3, 5), 1)),
         "2" = as_index(matrix(c(2, 4, 6), 1, dimnames = list(2, 1:3))))
  )
  expect_equal(
    split(x, c(1, 1, 2), margin = "time"),
    list("1" = as_index(matrix(1:4, 2)),
         "2" = as_index(matrix(5:6, 2, dimnames = list(1:2, 3))))
  )
})

test_that("replacement works", {
  x <- as_index(matrix(1:6, 2))
  split(x, 1:2) <- 1:2
  expect_equal(x, as_index(matrix(c(1, 2), 2, 3)))
  
  x <- as_index(matrix(1:6, 2))
  split(x, c(1, 1, 2), margin = "time") <- 1:2
  expect_equal(x, as_index(matrix(c(1, 1, 1, 1, 2, 2), 2)))
})