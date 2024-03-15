x1 <- c("1", "2", "1", "1")
x2 <- c("11", "21", "12", "11")
x3 <- c("111", "211", "121", "112")
agg <- aggregation_structure(list(x1 = x1, x2 = x2, x3 = x3))

test_that("as_aggregation_structure methods work", {
  expect_identical(agg, as_aggregation_structure(data.frame(x1, x2, x3, 1)))
  expect_identical(agg, as_aggregation_structure(cbind(x1, x2, x3, 1)))
  expect_identical(agg, as_aggregation_structure(agg))
})
