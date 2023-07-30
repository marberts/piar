dat <- data.frame(rel = c(1:6, NA, 7, 8),
                  period = c(1, 1, 1, 1, 1, 2, 2, 2, 2),
                  ea = c("11", "11", "12", "12", "13", "11", "12", "11", "14"))

pias <- as_aggregation_structure(
  data.frame(level1 = 1, level2 = c(11, 12, 13, 14), weight = 1)
)

epr <- with(dat, elemental_index(rel, period, ea, contrib = TRUE))
epr2 <- with(dat, elemental_index(rel, period, ea, contrib = FALSE))
index <- aggregate(epr, pias)

test_that("as_index makes a valid index", {
  res <- as_index(matrix(1:6, 2))
  expect_identical(levels(res), as.character(1:2))
  expect_identical(time(res), as.character(1:3))
  expect_error(as_index(numeric(0)))
  expect_error(as_index(matrix(1:2, 2, dimnames = list(c(1, 1), 2))))
  expect_error(as_index(matrix(1:2, 1, dimnames = list(1, c(2, 2)))))
  expect_error(as_index(matrix(1:2, 1, dimnames = list(1, c(NA, 2)))))
})

test_that("as_index works work matrices", {
  expect_equal(as_index(as.matrix(epr)), epr2)
  mat <- as.matrix(epr)
  mat[] <- as.character(mat)
  expect_equal(as_index(mat), epr2)
  expect_equal(as_index(as.matrix(chain(epr)), chain = FALSE), chain(epr))
})

test_that("as_index works for data frames", {
  expect_equal(as_index(as.data.frame(epr)), epr2)
  df <- as.data.frame(epr)
  df[[1]] <- factor(df[[1]], levels = 2:1)
  expect_equal(
    as_index(df),
    with(dat, elemental_index(rel, factor(period, levels = 2:1), ea))
  )
})

test_that("as_index works for indexes", {
  expect_equal(as_index(epr), epr)
  expect_equal(as_index(index), index)
  expect_equal(as_index(epr, chainable = FALSE), chain(epr))
  expect_equal(as_index(index, chainable = FALSE), chain(index))
  expect_equal(
    as_index(chain(aggregate(epr, pias, na.rm = TRUE)), chainable = TRUE),
    aggregate(epr2, pias, na.rm = TRUE)
  )
})
