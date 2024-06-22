dat <- data.frame(rel = c(1:6, NA, 7, 8),
                  period = c(1, 1, 1, 1, 1, 2, 2, 2, 2),
                  ea = c("11", "11", "12", "12", "13", "11", "12", "11", "14"))

pias <- as_aggregation_structure(
  data.frame(level1 = 1, level2 = c(11, 12, 13, 14), weight = 1)
)

epr <- elemental_index(dat, rel ~ period + ea, contrib = TRUE)
epr2 <- elemental_index(dat, rel ~ period + ea, contrib = FALSE)
index <- aggregate(epr, pias)

res <- c(1.41421356237309, 3.46410161513775, 5, NaN,
         6.48074069840786, NaN, NaN, 8)

test_that("getter methods work", {
  expect_identical(levels(epr), as.character(11:14))
  expect_identical(time(epr), as.character(1:2))
  expect_identical(start(epr), "1")
  expect_identical(end(epr), "2")
})

test_that("setter methods work", {
  epr3 <- epr
  levels(epr3) <- 1:4
  expect_identical(levels(epr3), as.character(1:4))
  levels(epr3) <- levels(epr)
  expect_identical(epr, epr3)
  
  time(epr3) <- 2:1
  expect_identical(time(epr3), as.character(2:1))
  time(epr3) <- time(epr)
  expect_identical(epr, epr3)
  
  expect_error(levels(epr3) <- NULL)
  expect_error(time(epr3) <- NULL)
  expect_error(levels(epr3) <- c(1, 2, 3, 1))
  expect_error(levels(index) <- c(1, 2, 3, 1, 1))
  expect_error(time(epr3) <- c(1, 1))
})

test_that("group generics work", {
  m <- as.matrix(epr)
  expect_equal(1 > epr, 1 > m)
  expect_equal(epr > 1, epr > 1)
  expect_error(epr + 1)
  expect_error(sum(epr))
  expect_error(log(epr))
})
