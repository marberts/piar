# Extended mean satisfied key identities.
a <- 1:5
b <- c(1, 5, 4, 1, 2)

# Symmetry.
expect_equal(emean(a, b), emean(b, a))
expect_equal(
  emean(a, b, order = c(-0.1, 2.5)),
  emean(b, a, order = c(2.5, -0.1))
)
expect_equal(
  emean(a, b, order = c(0, 2)),
  emean(b, a, order = c(2, 0))
)
expect_equal(
  emean(a, b, order = c(0, 0)),
  emean(b, a, order = c(0, 0))
)
expect_equal(
  emean(a, b, order = c(1, 1)),
  emean(b, a, order = c(1, 1))
)
# Identities.
expect_equal(
  emean(a, b, order = c(-1, 1)),
  apply(matrix(c(a, b), ncol = 2), 1, \(x) gmean(x, order = 0))
)
expect_equal(
  emean(a, b, order = c(2, 1)),
  apply(matrix(c(a, b), ncol = 2), 1, mean)
)
expect_equal(
  emean(a, b, order = c(-2, 1)),
  apply(
    matrix(c(a, b), ncol = 2),
    1,
    \(x) {
      (gmean(x, order = -1) * gmean(x, order = 0)^2)^(1 /
        3)
    }
  )
)
expect_equal(
  emean(a, b, order = c(0.5, 1)),
  apply(
    matrix(c(a, b), ncol = 2),
    1,
    \(x) nested_gmean(x, order = c(1, 0), outer_order = 1)
  )
)
expect_equal(
  emean(a, b),
  apply(matrix(c(a, b), ncol = 2), 1, \(x) gmean(x, order = 0))^2 *
    emean(1 / a, 1 / b)
)
expect_equal(
  emean(a, b, order = c(-2, -1)),
  apply(matrix(c(a, b), ncol = 2), 1, \(x) gmean(x, order = -1))
)
expect_equal(
  emean(a, b, order = c(-2, 2)),
  apply(matrix(c(a, b), ncol = 2), 1, \(x) gmean(x, order = 0))
)
expect_equal(
  emean(a, b, order = c(2, 2)),
  c(1, ((a^a^2 / b^b^2)^(1 / (a^2 - b^2)) / exp(1)^(1 / 2))[-1])
)

# Errors when expected.
expect_error(emean(a, b, tol = 1:10))
