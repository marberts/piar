agg <- as_aggregation_structure(1:5)
ind <- as_index(1:5)

# These used to not work
test_that("summary methods works", {
  expect_no_condition(summary(agg))
  expect_no_condition(summary(ind))
  expect_no_condition(summary(elemental_index(1:5, contrib = TRUE)))
})

test_that("str methods works", {
  expect_no_condition(str(agg))
  expect_no_condition(str(ind))
})
