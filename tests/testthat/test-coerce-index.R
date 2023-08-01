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
    as.data.frame(as_index(matrix(1:4, 2))),
    data.frame(period = as.character(rep(1:2, each = 2)),
               level = as.character(1:2),
               value = 1:4)
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
})
