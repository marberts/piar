x1 <- c("1", "2", "1", "1")
x2 <- c("11", "21", "12", "11")
x3 <- c("111", "211", "121", "112")
agg <- aggregation_structure(list(x1, x2, x3))

test_that("updating with a length-0 index makes the weights NA", {
  epr <- elemental_index(integer(0), period = gl(1, 0), ea = gl(1, 0))
  index <- aggregate(epr, agg)
  expect_equal(update(agg, index)[-4], agg[-4])

  expect_equal(weights(update(agg, index), ea_only = FALSE),
               list(c("1" = NA_real_, "2" = NA),
                    c("11" = NA_real_, "21" = NA, "12" = NA),
                    c("111" = NA_real_, "211" = NA, "121" = NA, "112" = NA)))
})

test_that("an index that doesn't line up with the pias introduces NA", {
  epr <- elemental_index(1:2, ea = c("111", "121"), period = gl(1, 2))
  index <- aggregate(epr, agg)
  expect_equal(weights(update(agg, index), ea_only = FALSE),
               list(c("1" = NA_real_, "2" = NA),
                    c("11" = NA_real_, "21" = NA, "12" = 2),
                    c("111" = 1, "211" = NA, "121" = 2, "112" = NA)))
  expect_equal(weights(update(agg, index, r = 0), ea_only = FALSE),
               list(c("1" = NA_real_, "2" = NA),
                    c("11" = NA_real_, "21" = NA, "12" = 1),
                    c("111" = 1, "211" = NA, "121" = 1, "112" = NA)))
})

test_that("updating works with only EAs", {
  # This used to not work
  epr <- elemental_index(1:2, ea = c("111", "121"), period = gl(1, 2))
  pias <- aggregation_structure(111)
  index <- aggregate(epr, pias)
  expect_equal(update(pias, index), pias)
})
