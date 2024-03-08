x1 <- c("1", "2", "1", "1")
x2 <- c("11", "21", "12", "11")
x3 <- c("111", "211", "121", "112")
agg <- aggregation_structure(list(x1, x2, x3))

test_that("levels method works", {
  expect_identical(
    levels(agg),
    list(c("1", "2"), c("11", "21", "12"), c("111", "211", "121", "112"))
  )
  expect_error(levels(agg) <- 1:9)
  
  expect_identical(levels(agg), lapply(weights(agg, ea_only = FALSE), names))
})

test_that("weights method works", {
  expect_equal(weights(agg, ea_only = FALSE),
               list(c("1" = 3, "2" = 1),
                    c("11" = 2, "21" = 1, "12" = 1),
                    c("111" = 1, "211" = 1, "121" = 1, "112" = 1)))
  expect_equal(weights(agg, ea_only = TRUE),
               c("111" = 1, "211" = 1, "121" = 1, "112" = 1))
  expect_equal(weights(aggregation_structure(list(x1, x2, x3), 1:4),
                       ea_only = FALSE),
               list(c("1" = 8, "2" = 2),
                    c("11" = 5, "21" = 2, "12" = 3),
                    c("111" = 1, "211" = 2, "121" = 3, "112" = 4)))

  expect_equal(weights(as_aggregation_structure(1:5, 2), ea_only = FALSE),
               list(c("1" = 2), c("2" = 2), c("3" = 2), c("4" = 2), c("5" = 2)))
  expect_equal(weights(as_aggregation_structure(1:5, 2), ea_only = TRUE),
               c("5" = 2))
  expect_equal(weights(as_aggregation_structure(list(1:5), 1:5),
                       ea_only = FALSE),
               list(c("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5)))
  expect_equal(weights(as_aggregation_structure(list(1:5), 1:5),
                       ea_only = TRUE),
               c("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5))
})

test_that("NAs move up the aggregation structure", {
  expect_equal(weights(aggregation_structure(list(x1, x2, x3), c(NA, 2:4)),
                       ea_only = FALSE),
               list(c("1" = NA, "2" = 2),
                    c("11" = NA, "21" = 2, "12" = 3),
                    c("111" = NA, "211" = 2, "121" = 3, "112" = 4)))

  expect_equal(
    weights(aggregation_structure(list(x1, x2, x3), c(NA, 2:4)),
            ea_only = FALSE, na.rm = TRUE),
    list(c("1" = 7, "2" = 2),
         c("11" = 4, "21" = 2, "12" = 3),
         c("111" = NA, "211" = 2, "121" = 3, "112" = 4))
  )
})

test_that("replacement method works", {
  weights(agg)[c(1, 3, 4)] <- 10
  expect_equal(weights(agg), c("111" = 10, "211" = 1, "121" = 10, "112" = 10))
})
