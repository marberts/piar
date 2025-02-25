x1 <- c("1", "2", "1", "1")
x2 <- c("11", "21", "12", "11")
x3 <- c("111", "211", "121", "112")

test_that("factors do nothing", {
  agg1 <- aggregation_structure(list(x1, x2, x3), 1:4)
  f <- factor(x2, levels = c("11", "21", "12"))
  agg2 <- aggregation_structure(list(x1, f, x3), 1:4)
  expect_identical(agg1, agg2)

  f <- factor(x2, levels = c("11", "21", "12", "99"))
  expect_identical(agg2, aggregation_structure(list(x1, f, x3), 1:4))

  f <- factor(replace(x2, 2, NA), levels = c("11", "21", "12"))
  expect_error(aggregation_structure(list(x1, addNA(f), x3), 1:4))
})

test_that("order is preserved", {
  agg <- aggregation_structure(list(x1, x2, x3))
  expect_identical(
    unlist(levels(agg), use.names = FALSE),
    unique(c(x1, x2, x3))
  )

  ord <- c(2, 4, 1, 3)
  agg <- aggregation_structure(list(x1[ord], x2[ord], x3[ord]))
  expect_identical(
    unlist(levels(agg), use.names = FALSE),
    unique(c(x1[ord], x2[ord], x3[ord]))
  )
})

test_that("names are not required", {
  x <- list(a = as.character(1:3), as.character(4:6), c = as.character(7:9))
  agg <- aggregation_structure(x)
  expect_identical(as.list(agg), x)
  expect_identical(
    weights(agg, ea_only = FALSE),
    list(
      a = c("1" = 1, "2" = 1, "3" = 1),
      c("4" = 1, "5" = 1, "6" = 1),
      c = c("7" = 1, "8" = 1, "9" = 1)
    )
  )
  expect_identical(
    as.data.frame(agg),
    as.data.frame(c(x, weight = list(c(1, 1, 1))))
  )
})

test_that("errors work", {
  expect_error(aggregation_structure(list()))
  expect_error(aggregation_structure(list(x1, x2, x3), 1:3))
  expect_error(aggregation_structure(list(x1, x2, x3, x3)))
  expect_error(aggregation_structure(list(x1, x2)))
  expect_error(aggregation_structure(list(1:2, c(11, NA))))
  expect_error(aggregation_structure(list(1:2, c(11, 11, 12))))
  expect_error(aggregation_structure(list(1:2, c(3, 3), c(4, 5))))
  expect_error(aggregation_structure(list(1:2, c(3, 3), c(4, ""))))
})
