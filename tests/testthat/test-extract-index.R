dat <- data.frame(rel = c(1:6, NA, 7, 8),
                  period = c(1, 1, 1, 1, 1, 2, 2, 2, 2),
                  ea = c("11", "11", "12", "12", "13", "11", "12", "11", "14"))

pias <- as_aggregation_structure(
  data.frame(level1 = 1, level2 = c(11, 12, 13, 14), weight = 1)
)

epr <- with(dat, elemental_index(rel, period, ea, contrib = TRUE))
epr2 <- with(dat, elemental_index(rel, period, ea))

index <- aggregate(epr, pias)

res <- c(1.41421356237309, 3.46410161513775, 5, NaN,
         6.48074069840786, NaN, NaN, 8)

test_that("head and tail work", {
  expect_equal(head(epr, 1), epr[1])
  expect_equal(head(epr, 5), epr)
  expect_equal(tail(epr, 1), epr[4])
  expect_equal(tail(epr, 5), epr)
  expect_equal(head(epr, 2:1), epr[1:2, 1])
  expect_equal(tail(epr, 2:1), epr[3:4, 2])
  expect_equal(head(epr, c(NA, -1)), epr[, 1])
  expect_equal(tail(epr, c(NA, -1)), epr[, 2])
})

test_that("subscripting methods work", {
  expect_equal(epr[], epr)
  expect_equal(index[], index)
  expect_equal(
    epr[c(TRUE, FALSE, TRUE, TRUE), 2:1],
    with(
      dat,
      elemental_index(rel, factor(period, 2:1), factor(ea, c(11, 13:14)),
                      contrib = TRUE)
    )
  )
  expect_equal(
    epr[c("14", "12"), TRUE],
    with(
      dat,
      elemental_index(rel, period, factor(ea, c(14, 12)),
                      contrib = TRUE)
    )
  )

  expect_false(is_aggregate_index(index[1:2, ]))
})

test_that("subscripting methods give errors where expeected", {
  expect_error(epr[1, NA])
  expect_error(epr[1, 3])
  expect_error(epr[c(1, 2, 1), ])
  expect_error(epr[NULL])
  expect_error(epr[integer(0)])
})

test_that("replacement methods work", {
  expect_equal(replace(epr, , values = epr), epr2)

  epr[, 1] <- 0
  expect_equal(
    as.matrix(epr),
    matrix(c(0, 0, 0, 0, res[5:8]), 4, 2, dimnames = list(11:14, 1:2))
  )
  expect_equal(
    contrib(epr),
    matrix(c(0, 0, 2.5962965079607, 2.88444419044716),
           2, 2, dimnames = list(c("11.1", "11.2"), 1:2))
  )

  epr[1, c(FALSE, TRUE)] <- "0"
  expect_equal(
    as.matrix(epr),
    matrix(c(0, 0, 0, 0, 0, res[6:8]), 4, 2, dimnames = list(11:14, 1:2))
  )
  expect_equal(
    contrib(epr),
    matrix(0, 0, 2, dimnames = list(NULL, 1:2))
  )

  # recycling should still happen
  epr[1, c(1, 2, 1)] <- 1:3
  expect_equal(epr[1, ], as_index(matrix(3:2, 1, dimnames = list("11", 1:2))))

  epr["14"] <- 1
  expect_equal(
    as.matrix(epr),
    matrix(c(3, 0, 0, 1, 2, res[6:7], 1), 4, 2, dimnames = list(11:14, 1:2))
  )
  expect_equal(
    contrib(epr, "14"),
    matrix(0, 0, 2, dimnames = list(NULL, 1:2))
  )
  
  index[1] <- 0
  expect_false(is_aggregate_index(index))
})

test_that("replacement methods give errors where expected", {
  expect_error(epr[NULL] <- 1)
  expect_error(epr[NA] <- 1)
  expect_error(epr[integer(0)] <- 1)
  expect_error(epr[0] <- 1)
  expect_error(epr[1, 0] <- 1)
  expect_error(epr[1] <- 1:3)
  expect_error(epr[1, NA] <- 1)
  expect_error(epr[1, 3] <- 1)
  expect_error(epr[5, 1] <- 1)
})
