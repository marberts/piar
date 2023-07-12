epr1 <- elemental_index(1:8, rep(1:2, each = 4), rep(letters[1:4], 2),
                        contrib = TRUE)
epr2 <- as_index(matrix(1:8, 4, 2, dimnames = list(letters[5:8], 1:2)))
pias <- as_aggregation_structure(
  data.frame(level1 = "0", level2 = letters[1:4], weight = 1:4)
)
index1 <- aggregate(epr1, pias)

test_that("merge returns the correct type of index", {
  expect_true(is_chainable_index(merge(epr1, epr2)))
  expect_true(is_direct_index(merge(chain(epr1), chain(epr2))))
  expect_false(is_aggregate_index(merge(index1, epr2)))
  expect_error(merge(epr1, chain(epr2)))
  expect_error(merge(chain(epr1), epr2))
})

test_that("merge returns the correct result", {
  epr3 <- merge(epr1, epr2)
  expect_equal(
    as.matrix(epr3),
    rbind(matrix(1:8, 4, 2, dimnames = list(letters[1:4], 1:2)),
          matrix(1:8, 4, 2, dimnames = list(letters[5:8], 1:2)))
  )
  expect_identical(levels(epr3), letters[1:8])
  expect_identical(time(epr3), time(epr1))
  expect_equal(contrib(epr3, "a"),
               matrix(c(0, 4), 1, 2, dimnames = list(1, 1:2)))
  expect_equal(contrib(epr3, "e"),
               matrix(numeric(0), 0, 2, dimnames = list(NULL, 1:2)))
  expect_equal(
    as.matrix(merge(epr2, epr1)),
    rbind(matrix(1:8, 4, 2, dimnames = list(letters[5:8], 1:2)),
          matrix(1:8, 4, 2, dimnames = list(letters[1:4], 1:2)))
  )
})
