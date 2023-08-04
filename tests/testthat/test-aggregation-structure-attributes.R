x1 <- c("1", "2", "1", "1")
x2 <- c("11", "21", "12", "11")
x3 <- c("111", "211", "121", "112")
agg <- aggregation_structure(list(x1, x2, x3))

test_that("levels method works", {
  expect_identical(levels(agg),
                   c("1", "2", "11", "12", "21", "111", "211", "121", "112"))
  expect_error(levels(agg) <- 1:9)
})

test_that("weights method works", {
  expect_equal(weights(agg),
               list(c("1" = 3, "2" = 1),
                    c("11" = 2, "12" = 1, "21" = 1),
                    c("111" = 1, "211" = 1, "121" = 1, "112" = 1)))
  expect_equal(weights(agg, ea_only = TRUE),
               c("111" = 1, "211" = 1, "121" = 1, "112" = 1))
  expect_equal(weights(aggregation_structure(list(x1, x2, x3), 1:4)),
               list(c("1" = 8, "2" = 2),
                    c("11" = 5, "12" = 3, "21" = 2),
                    c("111" = 1, "211" = 2, "121" = 3, "112" = 4)))

  expect_equal(weights(as_aggregation_structure(1:5, 2)),
               list(c("1" = 2), c("2" = 2), c("3" = 2), c("4" = 2), c("5" = 2)))
  expect_equal(weights(as_aggregation_structure(1:5, 2), ea_only = TRUE),
               c("5" = 2))
  expect_equal(weights(as_aggregation_structure(list(1:5), 1:5)),
               list(c("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5)))
  expect_equal(weights(as_aggregation_structure(list(1:5), 1:5),
                       ea_only = TRUE),
               c("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5))
})

test_that("NAs move up the aggregation structure", {
  expect_equal(weights(aggregation_structure(list(x1, x2, x3), c(NA, 2:4))),
               list(c("1" = NA, "2" = 2),
                    c("11" = NA, "12" = 3, "21" = 2),
                    c("111" = NA, "211" = 2, "121" = 3, "112" = 4)))

  expect_equal(
    weights(aggregation_structure(list(x1, x2, x3), c(NA, 2:4)), na.rm = TRUE),
    list(c("1" = 7, "2" = 2),
         c("11" = 4, "12" = 3, "21" = 2),
         c("111" = NA, "211" = 2, "121" = 3, "112" = 4))
  )
})