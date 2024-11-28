test_that("as.matrix work", {
  expect_equal(as.matrix(as_index(matrix(1:4, 2))),
               matrix(1:4, 2, dimnames = list(1:2, 1:2)))
  expect_equal(as.matrix(as_index(matrix(1:4, 1))),
               matrix(1:4, 1, dimnames = list(1, 1:4)))
  expect_equal(as.matrix(as_index(matrix(1:4, 4))),
               matrix(1:4, 4, dimnames = list(1:4, 1)))
})

test_that("as.data.frame works", {
  expect_equal(
    as.data.frame(as_index(matrix(1:4, 2)), row.names = 4:1),
    data.frame(period = as.character(rep(1:2, each = 2)),
               level = as.character(1:2),
               value = 1:4,
               row.names = 4:1)
  )
  expect_equal(
    as.data.frame(as_index(matrix(1:4, 1))),
    data.frame(period = as.character(1:4),
               level = as.character(1),
               value = 1:4)
  )
  expect_equal(
    as.data.frame(as_index(matrix(1:4, 4))),
    data.frame(period = as.character(1),
               level = as.character(1:4),
               value = 1:4)
  )
  # Check that factors are ordered correctly
  x <- matrix(1:6, 2, dimnames = list(c("b", "a"), c(3, 1, 2)))
  expect_equal(
    as.data.frame(as_index(x), stringsAsFactors = TRUE),
    data.frame(period = factor(rep(c(3, 1, 2), each = 2), c(3, 1, 2)),
               level = factor(c("b", "a"), c("b", "a")),
               value = 1:6)
  )
  expect_equal(
    as.data.frame(as_index(x), stringsAsFactors = FALSE),
    data.frame(period = as.character(rep(c(3, 1, 2), each = 2)),
               level = c("b", "a"),
               value = 1:6)
  )
})
