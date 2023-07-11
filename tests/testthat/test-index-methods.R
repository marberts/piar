dat <- data.frame(rel = c(1:6, NA, 7, 8),
                  period = c(1, 1, 1, 1, 1, 2, 2, 2, 2),
                  ea = c("11", "11", "12", "12", "13", "11", "12", "11", "14"))

epr <- with(dat, elemental_index(rel, period, ea, contrib = TRUE))
epr2 <- with(dat, elemental_index(rel, period, ea, contrib = FALSE))

res <- c(1.41421356237309, 3.46410161513775, 5, NaN,
         6.48074069840786, NaN, NaN, 8)

test_that("getter methods work", {
  expect_equal(levels(epr), as.character(11:14))
  expect_equal(time(epr), as.character(1:2))
  expect_equal(start(epr), "1")
  expect_equal(end(epr), "2")
})

test_that("coercion methods work", {
  expect_equal(as.matrix(epr), matrix(res, 4, 2, dimnames = list(11:14, 1:2)))
  expect_equal(as.matrix(epr), as.matrix(epr2))
  
  expect_equal(
    as.data.frame(epr),
    data.frame(period = as.character(rep(1:2, each = 4)),
               level = as.character(11:14),
               value = res)
  )
  expect_equal(as.data.frame(epr), as.data.frame(epr2))
  
  expect_equal(as_index(as.matrix(epr)), epr2)
  expect_equal(as_index(as.data.frame(epr)), epr2)
  expect_equal(as_index(epr), epr)
  expect_equal(chain(epr), as_index(as.matrix(chain(epr)), chain = FALSE))
})

test_that("head and tail work", {
  expect_equal(head(epr, 1), epr[1])
  expect_equal(head(epr, 5), epr)
  expect_equal(tail(epr, 1), epr[4])
  expect_equal(tail(epr, 5), epr)
  expect_equal(head(epr, 2:1), epr[1:2, 1])
  expect_equal(tail(epr, 2:1), epr[3:4, 2])
})

test_that("contrib works", {
  expect_equal(
    contrib(epr),
    matrix(c(0, 0.414213562373095, 2.5962965079607, 2.88444419044716),
           2, 2, dimnames = list(1:2, 1:2))
  )
  expect_equal(
    contrib(epr, "14"),
    matrix(c(0, 7),
           1, 2, dimnames = list(1, 1:2))
  )
  expect_null(contrib(epr2))
})

test_that("subscripting methods work", {
  expect_equal(epr[], epr)
  expect_equal(
    epr[c(T, F, T, T), 2:1],
    with(
      dat,
      elemental_index(rel, factor(period, 2:1), factor(ea, c(11, 13:14)),
                      contrib = TRUE)
    )
  )
  expect_equal(epr[[4, "2"]], 8)
})

test_that("replacement methods work", {
  expect_equal(replace(epr, values = epr), epr2)
  
  epr[, 1] <- 0
  expect_equal(
    as.matrix(epr),
    matrix(c(0, 0, 0, 0, res[5:8]), 4, 2, dimnames = list(11:14, 1:2))
  )
  expect_equal(
    contrib(epr),
    matrix(c(0, 0, 2.5962965079607, 2.88444419044716),
           2, 2, dimnames = list(1:2, 1:2))
  )
  
  epr[[T, 2]] <- 0
  expect_equal(
    as.matrix(epr),
    matrix(c(0, 0, 0, 0, 0, res[6:8]), 4, 2, dimnames = list(11:14, 1:2))
  )
  expect_equal(
    contrib(epr),
    matrix(0, 0, 2, dimnames = list(NULL, 1:2))
  )
})
