test_that("a matched-sample index aggregates correctly", {
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

  res <- c(1, 1, 1, 1, 1, 1, 1, 1, 1.30072391107879, 1.30072391107879,
           1.30072391107879, 0.894909688013136, 1.30072391107879,
           2.02000360773041, 1.30072391107879, 1.30072391107879,
           1.06307432720167, 1.06307432720167, 1.06307432720167,
           0.334293948126801, 1.06307432720167, 1.63533549062897,
           1.06307432720167, 1.06307432720167, 2.73476132776126,
           1.57451535678038, 4.57628618296306, 1.57451535678038,
           2.77045633390764, 0.537995998322608, 4.57628618296306,
           4.57628618296306)
  res <- matrix(res, 8, 4,
                dimnames = list(c("1", "11", "12", paste0("B", 1:5)),
                                sprintf("2020%02d", 1:4)))
  expect_equal(as.matrix(ms_index), res)

  # Same as matrix calculation
  expect_equal(
    as.matrix(ms_pias) %*% as.matrix(chain(ms_index[paste0("B", 1:5)])),
    as.matrix(chain(ms_index[1:3, ]))
  )

  # Lower levels add up
  expect_equal(
    apply(
      as.matrix(chain(ms_index)[4:8, ]), 2, weighted.mean, weights(ms_pias)[[3]]
    ),
    as.matrix(chain(ms_index))[1, ]
  )
  expect_equal(
    apply(
      as.matrix(chain(ms_index)[2:3, ]), 2, weighted.mean, weights(ms_pias)[[2]]
    ),
    as.matrix(chain(ms_index))[1, ]
  )

  # Re aggregating does nothing to the index values
  expect_equal(as.matrix(aggregate(ms_index, ms_pias)), as.matrix(ms_index))
  expect_equal(aggregate(chain(ms_index), ms_pias), chain(ms_index))
  expect_equal(as.matrix(aggregate(ms_index, ms_pias, na.rm = TRUE)),
               as.matrix(ms_index))

  # Re aggregating breaks contributions for imputed indexes
  expect_failure(
    expect_equal(contrib(ms_index), contrib(aggregate(ms_index, ms_pias)))
  )
  expect_equal(contrib(aggregate(ms_index, ms_pias)),
               contrib(aggregate(aggregate(ms_index, ms_pias), ms_pias)))

  # Two step aggregation gives the same result
  pias2 <- aggregation_structure(list(c(1, 1), c(11, 12)),
                                 weights(ms_pias)[[2]])
  expect_equal(as.matrix(aggregate(ms_index, pias2)), as.matrix(ms_index[1:3]))

  # Re-arranging the index shouldn't do anything
  s1 <- c(
    14, 16, 26, 28, 24, 29, 11, 32, 36, 2, 22, 34, 6, 7, 10, 17, 8, 27, 37,
    1, 12, 33, 20, 3, 9, 40, 13, 4, 38, 23, 31, 15, 25, 39, 21, 30, 35, 19,
    18, 5
  )
  s2 <- c(5, 3, 4, 1, 2)
  ms_epr <- with(
    ms_prices[s1, ],
    elemental_index(price_relative(price, period, product),
                    period, business, contrib = TRUE, na.rm = TRUE)
  )

  ms_pias <- with(
    ms_weights[s2, ],
    aggregation_structure(
      c(expand_classification(classification), list(business)), weight
    )
  )

  expect_equal(
    as.matrix(aggregate(ms_epr, ms_pias, na.rm = TRUE)[levels(ms_index), ]),
    as.matrix(ms_index)
  )
  expect_equal(
    as.matrix(aggregate(ms_epr, ms_pias, na.rm = TRUE)[1:3, ]),
    as.matrix(ms_index)[1:3, ]
  )

  # Aggregated contributions should add up
  expect_equal(as.matrix(ms_index)[1, ],
               colSums(contrib(ms_index), na.rm = TRUE) + 1)
  expect_equal(as.matrix(ms_index)[2, ],
               colSums(contrib(ms_index, "11"), na.rm = TRUE) + 1)

  # Check that weights are getting price updated correctly
  w <- apply(as.matrix(chain(ms_index)[4:8, ]), 2, `*`, ms_weights$weight)

  expect_equal(weights(update(ms_pias, ms_index), ea_only = TRUE), w[s2, 4])

  expect_equal(weights(update(ms_pias, ms_index, "202003"), ea_only = TRUE),
               w[s2, 3])
})

test_that("a weird index aggregates correctly", {
  ms_epr <- with(
    ms_prices,
    elemental_index(price_relative(price, period, product),
                    period, business, contrib = TRUE, r = 0.2)
  )

  ms_pias <- with(
    ms_weights,
    aggregation_structure(
      c(expand_classification(classification), list(business)), weight
    )
  )

  ms_index <- aggregate(ms_epr, ms_pias, r = -1.7, na.rm = TRUE)

  res <- c(1, 1, 1, 1, 1, 1, 1, 1, 3.59790686240372, 3.59790686240372,
           3.59790686240372, 3.59790686240372, 3.59790686240372,
           3.59790686240372, 3.59790686240372, 3.59790686240372,
           1.83344068593548, 1.83344068593548, 1.83344068593548,
           1.83344068593548, 1.83344068593548, 1.83344068593548,
           1.83344068593548, 1.83344068593548, 1.31335391535665,
           1.00564223748248, 5.47029871913538, 1.00564223748248,
           3.15295124015891, 0.551843416514059, 5.47029871913538,
           5.47029871913538)
  res <- matrix(res, 8, 4,
                dimnames = list(c("1", "11", "12", paste0("B", 1:5)),
                                sprintf("2020%02d", 1:4)))
  expect_equal(as.matrix(ms_index), res)

  expect_equal(as.matrix(aggregate(ms_index, ms_pias, r = -1.7, na.rm = TRUE)),
               as.matrix(ms_index))

  expect_equal(aggregate(chain(ms_index), ms_pias, r = -1.7), chain(ms_index))

  expect_equal(as.matrix(ms_index)[1, ],
               colSums(contrib(ms_index), na.rm = TRUE) + 1)

  expect_equal(
    apply(as.matrix(chain(ms_index)[2:3, ]), 2,
          gpindex::generalized_mean(-1.7), weights(ms_pias)[[2]]),
    as.matrix(chain(ms_index))[1, ]
  )

  expect_equal(
    (as.matrix(ms_pias) %*%
       as.matrix(chain(ms_index[paste0("B", 1:5)]))^(-1.7))^(1 / -1.7),
    as.matrix(chain(ms_index[1:3, ]))
  )

  ms_index <- aggregate(ms_epr, ms_pias, r = -1.7)

  expect_equal(aggregate(ms_index, ms_pias, r = -1.7), ms_index)

  expect_equal(as.matrix(ms_index)[1, ], colSums(contrib(ms_index)) + 1)
  expect_equal(as.matrix(ms_index)["B2", ],
               colSums(contrib(ms_index, "B2")) + 1)
})

test_that("a fixed-sample index aggregates correctly", {
  fs_epr <- with(
    fs_prices,
    elemental_index(price_relative(price, period, business),
                    period, classification, w = weight, contrib = TRUE)
  )

  fs_pias <- with(
    fs_weights,
    aggregation_structure(expand_classification(classification), weight)
  )

  fs_index <- aggregate(fs_epr, fs_pias, na.rm = TRUE)

  res <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0.687039919074495, 0.935841069334005,
           0.612229588412764, 0.611111111111111, 0.935841069334005,
           0.935841069334005, 0.612229588412764, 0.612229588412764,
           0.611111111111111, 2.45961303155833, 2.45961303155833,
           2.48533513976043, 1.68506493506494, 2.45961303155833,
           2.45961303155833, 2.48533513976043, 2.48533513976043,
           1.68506493506494, 1.08265987174601, 1.08265987174601,
           1.08265987174601, 1.08265987174601, 1.08265987174601,
           1.08265987174601, 1.61372443582219, 0.860786536350297,
           1.08265987174601)
  res <- matrix(res, 9, 4,
                dimnames = list(c(1, 11:13, 111, 112, 121, 122, 131),
                                sprintf("2020%02d", 1:4)))
  expect_equal(as.matrix(fs_index), res)

  # Re-aggregating the index shouldn't do anything
  expect_equal(as.matrix(aggregate(fs_index, fs_pias)), as.matrix(fs_index))

  # Contributions should add up
  expect_equal(as.matrix(fs_index)[1, ],
               colSums(contrib(fs_index), na.rm = TRUE) + 1)

  # Check adding up of lower level indexes
  expect_equal(
    apply(as.matrix(chain(fs_index)[5:9, ]), 2,
          weighted.mean, weights(fs_pias)[[3]]),
    as.matrix(chain(fs_index))[1, ]
  )

  expect_equal(
    apply(as.matrix(chain(fs_index)[2:4, ]), 2,
          weighted.mean, weights(fs_pias)[[2]]),
    as.matrix(chain(fs_index))[1, ]
  )

  # Non-missing indexes should be the same when missing values are not removed
  fs_index2 <- aggregate(fs_epr, fs_pias)
  expect_equal(as.matrix(fs_index2)[!is.na(as.matrix(fs_index2))],
               as.matrix(fs_index)[!is.na(as.matrix(fs_index2))])

  expect_equal(as.matrix(fs_index2)["121", ],
               colSums(contrib(fs_index2, "121"), na.rm = TRUE) + 1)

  expect_equal(as.matrix(fs_index2)["13", 1:3],
               contrib(fs_index2, "13")[, 1:3] + 1)
})

test_that("a fixed-based index aggregates correctly", {
  prices <- data.frame(price = 1:15,
                       period = letters[1:3],
                       product = rep(1:5, each = 3),
                       ea = rep(c("f1", "f2"), c(6, 9)))
  prices$pop_rel <- with(prices, price_relative(price, period, product))
  prices$fx_rel <- with(prices,
                        price / price[gpindex::base_period(period, product)])

  pias <- aggregation_structure(list(c("1", "1"), c("f1", "f2")), 1:2)

  epr_pop <- with(prices, elemental_index(pop_rel, period, ea))
  epr_fx <- with(prices, elemental_index(fx_rel, period, ea, chain = FALSE))

  index_pop <- aggregate(epr_pop, pias)
  index_fx <- aggregate(epr_fx, pias)

  # Chained calculation and fixed-base calculation should be the same
  expect_equal(index_fx, chain(index_pop))
  expect_equal(chain(index_pop[, -1], as.matrix(index_fx[, 1])), index_fx[, -1])

  # Should work for a non-arithmetic index
  expect_equal(chain(aggregate(epr_pop, pias, r = 3)),
               aggregate(epr_fx, pias, r = 3))
  
  # Consistency in aggregation holds with a change in base period
  expect_equal(
    rebase(index_fx, index_fx[, 2]),
    aggregate(rebase(index_fx, index_fx[, 2]), update(pias, index_fx, "b"))
  )
})

test_that("corner cases work", {
  expect_equal(as.matrix(aggregate(as_index(1:5), list(1:5))),
               matrix(1:5, 5, dimnames = list(1:5, 1)))
  expect_equal(as.matrix(aggregate(as_index(1:5), 6)),
               matrix(NA_real_, dimnames = list(6, 1)))
})

test_that("partial contributions are correct", {
  prices <- data.frame(
    rel = 1:8,
    period = rep(1:2, each = 4),
    ea = rep(letters[1:2], 4)
  )

  epr <- with(prices, elemental_index(rel, period, ea, contrib = TRUE))

  epr <- merge(epr, matrix(9:10, 1, dimnames = list("c", 1:2)))

  pias <- aggregation_structure(
    list(c("top", "top", "top"), c("a", "b", "c")), 1:3
  )

  index <- aggregate(epr, pias)
  expect_equal(
    sum(contrib(index[, 1])),
    sum(gpindex::arithmetic_contributions(as.numeric(index[letters[1:3], 1]),
                                          weights(pias, ea_only = TRUE))[1:2])
  )

  pias2 <- update(pias, index[, 1])
  expect_equal(
    sum(contrib(index[, 2])),
    sum(gpindex::arithmetic_contributions(as.numeric(index[letters[1:3], 2]),
                                          weights(pias2, ea_only = TRUE))[1:2])
  )
})

test_that("duplicate products get unique names during aggregation", {
  epr1 <- elemental_index(setNames(1:4, 1:4), ea = gl(2, 2), contrib = TRUE, r = 1)
  epr2 <- epr1
  levels(epr2) <- 3:4
  index <- aggregate(merge(epr1, epr2), list(c(0, 0, 0, 0), 1:4))
  expect_equal(
    contrib(index),
    matrix(rep(c(0, 0.125, 0.25, 0.375), 2), 8,
           dimnames = list(c(1:4, "1.1", "2.1", "3.1", "4.1"), 1))
  )
})
