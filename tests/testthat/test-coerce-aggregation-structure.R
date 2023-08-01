x1 <- c("1", "2", "1", "1")
x2 <- c("11", "21", "12", "11")
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
  expect_equal(agg, as_aggregation_structure(as.data.frame(agg)))
  expect_equal(as.list(agg), list(x1, x2, x3))

  expect_equal(as.matrix(aggregation_structure(1:3, 2)),
               matrix(c(1, 1), 2, 1, dimnames = list(c("1", "2"), "3")))
  expect_equal(as.data.frame(aggregation_structure(1:3, 2)),
               data.frame(level1 = "1", level2 = "2", ea = "3", weight = 2))
  expect_equal(as.list(aggregation_structure(1:3, 2)),
               as.list(as.character(1:3)))

  expect_equal(
    as.matrix(aggregation_structure(list(letters[c(2, 1, 3)]), 1:3)),
    matrix(numeric(0), 0, 3, dimnames = list(character(0), c("b", "a", "c")))
  )
  expect_equal(
    as.data.frame(aggregation_structure(list(letters[c(2, 1, 3)]), 1:3)),
    data.frame(ea = c("b", "a", "c"), weight = 1:3)
  )
  expect_equal(
    as.list(aggregation_structure(list(letters[c(2, 1, 3)]), 1:3)),
    list(letters[c(2, 1, 3)])
  )
})
