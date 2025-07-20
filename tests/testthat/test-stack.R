epr1 <- elemental_index(1:8,
  period = rep(1:2, each = 4), ea = rep(letters[1:4], 2),
  contrib = TRUE
)
epr2 <- as_index(matrix(1:8, 4, 2, dimnames = list(letters[1:4], 3:4)))
pias1 <- as_aggregation_structure(
  data.frame(level1 = "0", level2 = letters[1:4], weight = 1:4)
)
pias2 <- as_aggregation_structure(
  data.frame(level1 = "0", level2 = letters[1:4], weight = 4:1)
)
index1 <- aggregate(epr1, pias1)
index2 <- aggregate(epr2, pias2)

test_that("stack returns the correct type of index", {
  expect_true(is_chainable_index(stack(epr1, epr2)))
  expect_true(is_chainable_index(stack(index1, index2)))
  expect_true(is_chainable_index(stack(epr1, chain(epr2))))
  expect_true(is_direct_index(stack(chain(epr1), epr2)))
  expect_error(suppressWarnings(stack(index1, epr1)))
  time(epr2) <- time(epr1)
  expect_error(stack(epr1, epr2))
  expect_error(stack(index1, aggregate(epr2, pias2, r = 0)))
})

test_that("stacking and unstacking are opposite operations", {
  expect_equal(epr1, Reduce(stack, unstack(epr1)))
  expect_equal(chain(epr1), Reduce(stack, unstack(chain(epr1))))
  expect_equal(index2, Reduce(stack, unstack(index2)))
  expect_equal(chain(index2), Reduce(stack, unstack(chain(index2))))
})

test_that("stacking returns the correct result", {
  epr3 <- stack(epr1, epr2)
  expect_equal(
    as.matrix(epr3),
    matrix(1:8, 4, 4, dimnames = list(levels = letters[1:4], time = 1:4))
  )
  expect_identical(levels(epr3), levels(epr1))
  expect_identical(time(epr3), as.character(1:4))
  expect_equal(
    contrib(epr3),
    matrix(c(0, 4, 0, 0), 1, 4, dimnames = list(product = "a.1", time = 1:4))
  )
  expect_equal(
    as.matrix(stack(epr2, epr1)),
    matrix(1:8, 4, 4, dimnames = list(levels = letters[1:4], time = c(3:4, 1:2)))
  )
})

test_that("stack works with [", {
  epr3 <- stack(epr1, epr2)
  expect_equal(epr3[, 1:2], epr1)
  expect_equal(epr3[, 3:4], epr2)
})

test_that("coercion works as expected", {
  expect_equal(stack(epr1, epr2), stack(epr1, chain(epr2)))
  expect_equal(stack(chain(epr1), chain(epr2)), stack(chain(epr1), chain(epr2)))
})

test_that("price updating works with a stacked index", {
  index <- stack(index1, index2)
  expect_equal(aggregate(index[, 2], update(pias1, index, 1)), index[, 2])
  expect_equal(aggregate(index[, 4], update(pias2, index[, 3:4], 3)), index[, 4])
})

test_that("stack matches y to x", {
  expect_equal(stack(epr1, epr2), stack(epr1, epr2[4:1]))
})
