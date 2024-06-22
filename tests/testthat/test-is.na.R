dat <- data.frame(rel = c(1:2, NA, 4:6, NA, 7, 8),
                  period = c(1, 1, 1, 1, 1, 2, 2, 2, 2),
                  ea = c("11", "11", "12", "12", "13", "11", "12", "11", "14"))

epr <- elemental_index(dat, rel ~ period + ea, contrib = TRUE, na.rm = TRUE)

test_that("is.na works", {
  expect_equal(
    is.na(epr),
    matrix(c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE), 4,
           dimnames = list(levels(epr), time(epr)))
  )
  expect_true(anyNA(epr))
  expect_false(anyNA(epr[1:3, 1]))
  expect_true(anyNA(epr[1:3, 1], recursive = TRUE))
  
  epr <- as_index(as.matrix(epr))
  expect_false(anyNA(epr[1:3, 1]))
  expect_false(anyNA(epr[1:3, 1], recursive = TRUE))
})

test_that("replacement method works", {
  epr2 <- epr
  epr2[is.na(epr2)] <- 1:3
  m <- as.matrix(epr)
  expect_equal(as.matrix(epr2), replace(m, is.na(m), 1:3))
  expect_equal(contrib(epr, "14"), contrib(epr2, "14"))
  
  epr2[is.na(epr2)] <- numeric(0)
})
