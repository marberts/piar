x1 <- c("1", "2", "1", "1")
x2 <- c("11", "21", "12", "11")
x3 <- c("111", "211", "121", "112")

test_that("reordering works", {
  epr <- as_index(matrix(1:12, 4, 3,
                         dimnames = list(c("111", "112", "121", "211"), 1:3)))
  agg1 <- aggregation_structure(list(x1, x2, x3), 1:4)
  f <- factor(x2, levels = c("11", "21", "12"))
  agg2 <- aggregation_structure(list(x1, f, x3), 1:4)
  index1 <- aggregate(epr, agg1)
  index2 <- aggregate(epr, agg2)
  expect_equal(as.matrix(index1[levels(index2)]), as.matrix(index2))
  
  f <- factor(x2, levels = c("11", "21", "12", "99"))
  expect_equal(agg2, aggregation_structure(list(x1, f, x3), 1:4))
  
  f <- factor(replace(x2, 2, NA), levels = c("11", "21", "12"))
  expect_error(aggregation_structure(list(x1, addNA(f), x3), 1:4))
})

test_that("errors work", {
  expect_error(aggregation_structure(list()))
  expect_error(aggregation_structure(list(x1, x2, x3), 1:3))
  expect_error(aggregation_structure(list(x1, x2, x3, x3)))
  expect_error(aggregation_structure(list(x1, x2)))
  expect_error(aggregation_structure(list(1:2, c(11, NA))))
  expect_error(aggregation_structure(list(1:2, c(11, 11, 12))))
  expect_error(aggregation_structure(list(1:2, c(3, 3), c(4, 5))))
})
