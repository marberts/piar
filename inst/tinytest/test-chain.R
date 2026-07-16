epr1 <- elementary_index(
  ms_prices,
  price_relative(price, period = period, product = product) ~ period + business,
  contrib = TRUE,
  na.rm = TRUE
)

epr2 <- elementary_index(
  ms_prices,
  price_relative(price, period = period, product = product) ~ period + business,
  na.rm = TRUE
)

pias <- with(
  ms_weights,
  aggregation_structure(
    c(expand_classification(classification), list(business)),
    weight
  )
)

index1 <- aggregate(epr1, pias, na.rm = TRUE)
index2 <- aggregate(epr2, pias, na.rm = TRUE)

# Chain is the same as apply.
local({
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

# Unchain and chain are inverses with no NAs.
local({
  expect_equal(unchain(chain(index2)), index2)
  expect_equal(unchain(rebase(chain(index2), base = 1:8), base = 1:8), index2)
  expect_equal(unchain(chain(index2, link = 1:8), base = 1 / 1:8), index2)
  expect_false(isTRUE(all.equal(unchain(chain(epr2)), epr2)))
})

# Unchaining/rebasing a chainable index does nothing.
local({
  expect_equal(unchain(epr1), epr1)
  expect_equal(unchain(index1), index1)
  expect_equal(rebase(epr1), epr1)
  expect_equal(rebase(index1), index1)
})

# Chaining a fixed-base index does nothing.
local({
  expect_equal(chain(epr1), chain(chain(epr1)))
  expect_equal(chain(index1), chain(chain(index1)))
})

# Rebase should be the same as division.
local({
  expect_equal(
    as.matrix(rebase(chain(epr2), base = 1:4)),
    as.matrix(chain(epr2)) / 1:4
  )
  expect_equal(
    as.matrix(rebase(chain(index2), base = 1:8)),
    as.matrix(chain(index2)) / 1:8
  )
})

# Rebase works with mean.
local({
  index2 <- chain(index2)
  expect_equal(
    rebase(index2, base = mean(index2, window = 2)[, 1]),
    rebase(index2, base = rowMeans(as.matrix(index2[, 1:2])))
  )

  expect_equal(
    rebase(index2, base = mean(index2, window = 4)),
    rebase(index2, base = rowMeans(as.matrix(index2)))
  )
})

# Chaining returns the correct type of index.
local({
  expect_true(is_chainable_index(epr1))
  expect_true(is_chainable_index(unchain(chain(epr1))))
  expect_true(is_direct_index(chain(epr1)))
  expect_true(is_chainable_index(index1))
  expect_true(is_chainable_index(unchain(chain(index1))))
  expect_true(is_direct_index(chain(index1)))
})

# Chaining keeps EA names.
local({
  expect_equal(
    as.matrix(chain(as_index(matrix(1:5, 1)))),
    matrix(cumprod(1:5), 1, dimnames = list(levels = 1, time = 1:5))
  )
})

# Link and base values are the right length.
local({
  expect_error(chain(epr1, link = 1))
  expect_error(rebase(chain(epr1), base = 1))
})

# Rebasing with a character vector works.
local({
  index1_chain <- chain(index1)
  index1_rebase <- rebase(
    index1_chain,
    base = index1_chain[, end(index1_chain)]
  )
  expect_equal(index1_rebase, rebase(index1_chain, base = end(index1_chain)))
  expect_equal(index2, unchain(index1_rebase, base = end(index1_rebase)))
})

# It used to be possible to get negative indexes values.
local({
  x <- as_index(matrix(1:9, 3))
  expect_error(chain(x, link = c(-1, 2, 3)), "`link` must be strictly positive")

  x <- chain(x)
  expect_error(
    unchain(x, base = c(-1, 2, 3)),
    "`base` must be strictly positive"
  )
  expect_error(
    rebase(x, base = c(-1, 2, 3)),
    "`base` must be strictly positive"
  )
})
