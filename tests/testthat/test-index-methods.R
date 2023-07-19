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
  mat <- as.matrix(epr)
  mat[] <- as.character(mat)
  expect_equal(as_index(mat), epr2)
  expect_equal(as_index(as.data.frame(epr)), epr2)
  expect_equal(as_index(epr), epr)
  expect_equal(chain(epr), as_index(as.matrix(chain(epr)), chain = FALSE))
})

test_that("as_index makes a valid index", {
  res <- as_index(matrix(1:6, 2))
  expect_identical(levels(res), as.character(1:2))
  expect_identical(time(res), as.character(1:3))
  expect_error(as_index(numeric(0)))
  expect_error(as_index(matrix(1:2, 2, dimnames = list(c(1, 1), 2))))
  expect_error(as_index(matrix(1:2, 1, dimnames = list(1, c(2, 2)))))
})

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
  
  expect_equal(
    contrib(elemental_index(c(a = 1, b = 2, c = 3, a = 4, a = 5),
                            c(1, 1, 1, 2, 2), contrib = TRUE, r = 1)),
    matrix(c(0, 1 / 3, 2 / 3, 0, 1.5, 0, 0, 2), 4, 2,
           dimnames = list(c("a", "b", "c", "a.1"), 1:2))
  )
  
  w <- setNames(rep(1, 9), letters[1:9])
  expect_equal(
    contrib(epr),
    contrib(
      with(dat, elemental_index(rel, period, ea, w, contrib = TRUE))
    )
  )
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
  expect_equal(epr[[4, "2"]], 8)
  expect_false(is_aggregate_index(index[1:2, ]))
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
           2, 2, dimnames = list(1:2, 1:2))
  )

  epr[[TRUE, 2]] <- "0"
  expect_equal(
    as.matrix(epr),
    matrix(c(0, 0, 0, 0, 0, res[6:8]), 4, 2, dimnames = list(11:14, 1:2))
  )
  expect_equal(
    contrib(epr),
    matrix(0, 0, 2, dimnames = list(NULL, 1:2))
  )
})
