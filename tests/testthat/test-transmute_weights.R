test_that("transmuting weights works", {
  x <- 1:5
  expect_equal(gmean(x, transmute_weights(x)), gmean(x, order = 0))
})
