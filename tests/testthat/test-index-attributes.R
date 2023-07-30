dat <- data.frame(rel = c(1:6, NA, 7, 8),
                  period = c(1, 1, 1, 1, 1, 2, 2, 2, 2),
                  ea = c("11", "11", "12", "12", "13", "11", "12", "11", "14"))

pias <- as_aggregation_structure(
  data.frame(level1 = 1, level2 = c(11, 12, 13, 14), weight = 1)
)

epr <- with(dat, elemental_index(rel, period, ea, contrib = TRUE))
epr2 <- with(dat, elemental_index(rel, period, ea, contrib = FALSE))
index <- aggregate(epr, pias)

res <- c(1.41421356237309, 3.46410161513775, 5, NaN,
         6.48074069840786, NaN, NaN, 8)

test_that("getter methods work", {
  expect_equal(levels(epr), as.character(11:14))
  expect_equal(time(epr), as.character(1:2))
  expect_equal(start(epr), "1")
  expect_equal(end(epr), "2")
})

test_that("levels can't be replaced", {
  expect_error(levels(epr) <- 1:4)
})
