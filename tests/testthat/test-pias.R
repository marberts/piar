x1 <- c(  "1",   "2",   "1",   "1")
x2 <- c( "11",  "21",  "12",  "11")
x3 <- c("111", "211", "121", "112")
agg <- aggregation_structure(list(x1, x2, x3))

test_that("coercion methods works", {
  expect_equal(
    as.matrix(agg),
    matrix(c(2, 0, 3, 0, 0, 0, 6, 0, 0, 6, 2, 0, 0, 6, 0, 2, 0, 3, 0, 0) / 6,
           5, 4,
           dimnames = list(c("1", "2", "11", "12", "21"), 
                           c("111", "211", "121", "112")))
  )
  expect_equal(
    as.data.frame(agg),
    data.frame(level1 = c("1", "2", "1", "1"),
               level2 = c("11", "21", "12", "11"),
               ea = c("111", "211", "121", "112"),
               weight = c(1, 1, 1, 1))
  )
  expect_equal(agg, aggregation_structure(as.data.frame(agg)[1:3]))
  
  expect_equal(as.matrix(aggregation_structure(1:3, 2)),
               matrix(c(1, 1), 2, 1, dimnames = list(c("1", "2"), "3")))
  expect_equal(as.data.frame(aggregation_structure(1:3, 2)),
               data.frame(level1 = "1", level2 = "2", ea = "3", weight = 2))
  
  expect_equal(
    as.matrix(aggregation_structure(list(letters[c(2, 1, 3)]), 1:3)),
    matrix(numeric(0), 0, 3, dimnames = list(character(0), c("b", "a", "c")))
  )
  expect_equal(
    as.data.frame(aggregation_structure(list(letters[c(2, 1, 3)]), 1:3)),
    data.frame(ea = c("b", "a", "c"), weight = 1:3)
  )
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

test_that("updating works", {
  # Updating with a length-0 index should make the weights NA
  epr <- elemental_index(integer(0))
  index <- aggregate(epr, agg)
  expect_equal(update(agg, index)[-5], agg[-5])
  
  expect_equal(weights(update(agg, index)),
               list(c("1" = NA_real_, "2" = NA),
                    c("11" = NA_real_, "12" = NA, "21" = NA),
                    c("111" = NA_real_, "211" = NA, "121" = NA, "112" = NA)))
  
  # Updating with an index that doesn't line up with the pias introduces NA
  # weights that should carry up the aggregation structure
  epr <- elemental_index(1, ea = "111")
  index <- aggregate(epr, agg)
  expect_equal(weights(update(agg, index)),
               list(c("1" = NA_real_, "2" = NA),
                    c("11" = NA_real_, "12" = NA, "21" = NA),
                    c("111" = 1, "211" = NA, "121" = NA, "112" = NA)))
})

test_that("reordering works", {
  epr <- as_index(matrix(1:12, 4, 3, dimnames = list(c("111", "112", "121", "211"), 1:3)))
  agg1 <- aggregation_structure(list(x1, x2, x3), 1:4)
  agg2 <- aggregation_structure(list(x1, factor(x2, levels = c("11", "21", "12")), x3), 1:4)
  index1 <- aggregate(epr, agg1)
  index2 <- aggregate(epr, agg2)
  expect_equal(as.matrix(index1[levels(index2)]), as.matrix(index2))
})
