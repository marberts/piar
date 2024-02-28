dat <- data.frame(rel = c(1:6, NA, 7, 8),
                  period = c(1, 1, 1, 1, 1, 2, 2, 2, 2),
                  ea = c("11", "11", "12", "12", "13", "11", "12", "11", "14"))

epr <- with(dat, elemental_index(rel, period, ea, contrib = TRUE))

test_that("is.na works", {
  expect_equal(
    is.na(epr),
    matrix(c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE), 4,
           dimnames = list(levels(epr), time(epr)))
  )
  expect_true(anyNA(epr))
  expect_false(anyNA(epr[1]))
})

test_that("replacement method works", {
  epr2 <- epr
  epr2[is.na(epr2)] <- 1:3
  m <- as.matrix(epr)
  expect_equal(as.matrix(epr2), replace(m, is.na(m), 1:3))
  expect_equal(contrib(epr, "14"), contrib(epr2, "14"))
  
  epr2[is.na(epr2)] <- numeric(0)
})
