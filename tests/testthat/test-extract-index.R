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
  expect_equal(head(epr, -1), epr[1:3])
  expect_equal(tail(epr, 1), epr[4])
  expect_equal(tail(epr, 5), epr)
  expect_equal(tail(epr, -1), epr[2:4])
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
})

test_that("subscripting returns an aggregate index when appropriate", {
  expect_true(is_aggregate_index(index[, 1:2]))
  expect_false(is_aggregate_index(index[1:2, ]))
})

test_that("subscripting methods give errors where expected", {
  expect_error(epr[1, NA])
  expect_error(epr[1, 3])
  expect_error(epr[c(1, 2, 1), ])
  expect_error(epr[NULL])
  expect_error(epr[integer(0)])
  expect_error(epr[NA])
})

test_that("replacement methods work with an index", {
  # No op
  expect_equal(replace(epr, 1:4, epr), epr)
  # Adds contributions
  expect_equal(replace(epr2, 1:4, epr), epr)
  # Removes contributions
  expect_equal(replace(epr, 1:4, epr2), epr2)
  
  epr3 <- epr2
  epr3[c(T, F), 2] <- epr[1, 1]
  expect_equal(
    as.matrix(epr3),
    replace(as.matrix(epr), matrix(c(1, 3, 2, 2), 2), as.numeric(epr[1, 1]))
  )
  expect_equal(contrib(epr3, "13")[, 2], contrib(epr)[, 1])
  
  # Index doesn't recycle like a matrix
  m <- as.matrix(epr3)
  epr3[c(1, 1, 2, 2)] <- epr[1:2]
  m[1, ] <- as.matrix(epr)[2, ]
  expect_equal(as.matrix(epr3), m)
  expect_equal(contrib(epr3), contrib(epr, "12"))
  epr3[c(1, 1, 2, 2)] <- as.matrix(epr[1:2])
  expect_error(expect_equal(as.matrix(epr3), m))
  
  # Errors
  expect_error(epr[1:2] <- epr[, 1])
  expect_error(epr[1:3]<- epr[1:2])
  expect_error(epr[NA] <- epr[1])
  expect_error(epr[, NA] <- epr[1])
  expect_error(epr[integer(0)] <- epr[1])
  expect_error(epr[1] <- chain(epr[1]))
  epr <- chain(epr)
  expect_error(epr[1] <- epr2[1])
})

test_that("replacement methods work with a vector", {
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
  epr2[1:3, ] <- 8:9
  expect_equal(
    epr2[1:3, ],
    as_index(matrix(8:9, 3, 2, dimnames = list(11:13, 1:2)))
  )
  
  epr[1, c(1, 2, 1)] <- setNames(1:3, letters[1:3])
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
  
  # Errors
  expect_error(epr[0] <- 1:3)
  expect_error(epr[1] <- 1:3)
  expect_error(epr[1:2] <- 1:3)
  expect_error(epr[NA] <- 1)
  expect_error(epr[1, NA] <- 1)
  expect_error(epr[5] <- 1)
  expect_error(epr[1] <- numeric(0))
})

test_that("replacement method works with a matrix", {
  epr3 <- replace(epr, epr > 2, 1:2)
  expect_equal(
    as.matrix(epr3),
    matrix(c(sqrt(2), 1, 2, NA, 1, NA, NA, 2), 4, dimnames = list(11:14, 1:2))
  )
  expect_equal(contrib(epr3), replace(contrib(epr), 3:4, 0))
  
  epr4 <- replace(
    epr3, matrix(c("11", "11", "12", "12", "2", "2", "2", "2"), 4), 1:2
  )
  expect_equal(as.matrix(epr4), replace(as.matrix(epr3), 5:6, 2))
  expect_equal(contrib(epr4), contrib(epr3))
  
  expect_error(epr[is.na(epr)] <- numeric(0))
  expect_error(epr[matrix(TRUE, 3, 3)] <- 1)
  expect_error(epr[matrix(1, nrow = 1, ncol = 3)] <- 1)
  expect_error(epr[is.na(epr)] <- 1:2)
})

