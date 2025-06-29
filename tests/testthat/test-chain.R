epr1 <- elemental_index(
  ms_prices,
  price_relative(price, period = period, product = product) ~ period + business,
  contrib = TRUE, na.rm = TRUE
)

epr2 <- elemental_index(
  ms_prices,
  price_relative(price, period = period, product = product) ~ period + business,
  na.rm = TRUE
)

pias <- with(
  ms_weights,
  aggregation_structure(
    c(expand_classification(classification), list(business)), weight
  )
)

index1 <- aggregate(epr1, pias, na.rm = TRUE)
index2 <- aggregate(epr2, pias, na.rm = TRUE)

test_that("chain is the same as apply", {
  expect_equal(
    as.matrix(chain(epr1)),
    t(apply(as.matrix(epr1), 1, cumprod))
  )
  expect_equal(
    as.matrix(chain(epr1, link = 1:4)),
    t(apply(as.matrix(epr1), 1, cumprod)) * 1:4
  )
  expect_equal(
    as.matrix(chain(index1)),
    t(apply(as.matrix(index1), 1, cumprod))
  )
})

test_that("unchain and chain are inverses with no NAs", {
  expect_equal(unchain(chain(index2)), index2)
  expect_equal(unchain(rebase(chain(index2), 1:8), 1:8), index2)
  expect_equal(unchain(chain(index2, 1:8), 1 / 1:8), index2)
  expect_failure(expect_equal(unchain(chain(epr2)), epr2))
})

test_that("unchaining/rebasing a chainable index does nothing", {
  expect_equal(unchain(epr1), epr1)
  expect_equal(unchain(index1), index1)
  expect_equal(rebase(epr1), epr1)
  expect_equal(rebase(index1), index1)
})

test_that("chaining a fixed-base index does nothing", {
  expect_equal(chain(epr1), chain(chain(epr1)))
  expect_equal(chain(index1), chain(chain(index1)))
})

test_that("rebase should be the same as division", {
  expect_equal(
    as.matrix(rebase(chain(epr2), 1:4)),
    as.matrix(chain(epr2)) / 1:4
  )
  expect_equal(
    as.matrix(rebase(chain(index2), 1:8)),
    as.matrix(chain(index2)) / 1:8
  )
})

test_that("rebase works with mean", {
  index2 <- chain(index2)
  expect_equal(
    rebase(index2, mean(index2, window = 2)[, 1]),
    rebase(index2, rowMeans(as.matrix(index2[, 1:2])))
  )

  expect_equal(
    rebase(index2, mean(index2, window = 4)),
    rebase(index2, rowMeans(as.matrix(index2)))
  )
})


test_that("chaining returns the correct type of index", {
  expect_true(is_chainable_index(epr1))
  expect_true(is_chainable_index(unchain(chain(epr1))))
  expect_true(is_direct_index(chain(epr1)))
  expect_true(is_chainable_index(index1))
  expect_true(is_chainable_index(unchain(chain(index1))))
  expect_true(is_direct_index(chain(index1)))
})

test_that("chaining keeps EA names", {
  expect_equal(
    as.matrix(chain(as_index(matrix(1:5, 1)))),
    matrix(cumprod(1:5), 1, dimnames = list(levels = 1, time = 1:5))
  )
})

test_that("link and base values are the right length", {
  expect_error(chain(epr1, link = 1))
  expect_error(rebase(chain(epr1), base = 1))
})

test_that("rebasing with a character vector works", {
  index1_chain <- chain(index1)
  index1_rebase <- rebase(index1_chain, index1_chain[, end(index1_chain)])
  expect_equal(index1_rebase, rebase(index1_chain, end(index1_chain)))
  expect_equal(index2, unchain(index1_rebase, end(index1_rebase)))
})
