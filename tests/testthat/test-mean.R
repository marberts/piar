test_that("aggregating over subperiods works", {
  ms_epr <- with(
    ms_prices,
    elemental_index(price_relative(price, period, product),
                    period, business, contrib = TRUE, na.rm = TRUE)
  )

  epr2 <- mean(ms_epr, window = 2)
  expect_identical(levels(epr2), levels(ms_epr))
  expect_identical(time(epr2), c("202001", "202003"))

  expect_equal(as.matrix(epr2)[, 1], rowMeans(as.matrix(ms_epr)[, 1:2]))
  expect_equal(as.matrix(epr2)[, 2], rowMeans(as.matrix(ms_epr)[, 3:4]))

  expect_true(is_chainable_index(epr2))
  expect_false(is_chainable_index(mean(chain(ms_epr))))

  w <- matrix(seq_len(4 * 4), 4)
  expect_equal(
    as.matrix(mean(ms_epr, w, window = 2))[, 1],
    diag(as.matrix(ms_epr)[, 1:2] %*% apply(w[, 1:2], 1, scale_weights)),
    ignore_attr = TRUE
  )
  expect_equal(
    as.matrix(mean(ms_epr, w, window = 2))[, 2],
    diag(as.matrix(ms_epr)[, 3:4] %*% apply(w[, 3:4], 1, scale_weights)),
    ignore_attr = TRUE
  )
  
  # Test contributions
  con <- matrix(0, 5, 2)
  rownames(con) <- c("1", "2", "2.1", "3", "3.1")
  colnames(con) <- c("202001", "202003")
  con[c(2, 3, 9, 10)] <- NA
  con[c(5, 7)] <- contrib(ms_epr)[c(6, 8)] / 2:1
  expect_equal(contrib(epr2), con)
  
  expect_equal(as.numeric(t(contrib(ms_epr, "B3")[, 3:4])) / 2,
               as.numeric(contrib(epr2, "B3")[, 2]))
  expect_equal(colSums(contrib(epr2, "B3")), as.matrix(epr2)["B3", ] - 1)
  
  epr3 <- mean(ms_epr, w, window = 2, r = 2.5, na.rm = TRUE)
  expect_equal(colSums(contrib(epr3, "B1"), na.rm = TRUE),
               as.matrix(epr3)["B1", ] - 1)
  expect_equal(colSums(contrib(epr3, "B3")), as.matrix(epr3)["B3", ] - 1)
})

test_that("aggregating with a window of 1 does nothing", {
  x <- as_index(matrix(1:9, 3))
  expect_equal(mean(x, window = 1), x)
  expect_equal(mean(x, 9:1, window = 1, r = 0), x)
})

test_that("mean requires a suitable window", {
  index <- as_index(matrix(1:9, 3))
  expect_error(mean(index, window = 4))
  expect_error(mean(index, window = 0))
  expect_error(mean(index, window = 1:3))
  expect_warning(mean(index, window = 2))
})

test_that("averaging an aggregate index works", {
  ms_epr <- with(
    ms_prices,
    elemental_index(price_relative(price, period, product),
                    period, business, contrib = TRUE, na.rm = TRUE)
  )
  
  ms_pias <- with(
    ms_weights,
    aggregation_structure(
      c(expand_classification(classification), list(business)), weight
    )
  )
  
  ms_index <- aggregate(ms_epr, ms_pias, na.rm = TRUE)
  
  expect_true(is_aggregate_index(mean(ms_index, window = 2)))
  expect_false(is_aggregate_index(mean(ms_index, window = 2, r = -1)))
})