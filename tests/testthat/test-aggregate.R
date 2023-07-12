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
  
  # # Aggregated contributions should add up
  # all.equal(as.matrix(ms_index)[1, ], 
  #           colSums(contrib(ms_index), na.rm = TRUE) + 1)
  # 
  # # Check that weights are getting price updated correctly
  # apply(as.matrix(chain(ms_index)[4:8, ]), 2, `*`, ms_weights$weight)
  # 
  # weights(update(ms_pias, ms_index), ea_only = TRUE)
  # 
  # weights(update(ms_pias, ms_index, "202003"), ea_only = TRUE)
})



# # Do the same tests but with a weird index
# ms_epr <- with(
#   ms_prices,
#   elemental_index(price_relative(price, period, product), 
#                   period, business, contrib = TRUE, r = 0.2)
# )
# 
# ms_index <- aggregate(ms_epr, ms_pias, r = -1.7, na.rm = TRUE)
# 
# all.equal(as.matrix(aggregate(ms_index, ms_pias, r = -1.7, na.rm = TRUE)), as.matrix(ms_index))
# 
# all.equal(aggregate(chain(ms_index), ms_pias, r = -1.7), chain(ms_index))
# 
# all.equal(as.matrix(ms_index)[1, ], 
#           colSums(contrib(ms_index), na.rm = TRUE) + 1)
# 
# all.equal(apply(as.matrix(chain(ms_index)[2:3, ]), 2, gpindex::generalized_mean(-1.7), weights(ms_pias)[[2]]),
#           as.matrix(chain(ms_index))[1, ])
# 
# all.equal((as.matrix(ms_pias) %*% as.matrix(chain(ms_index[paste0("B", s)]))^(-1.7))^(1 /-1.7), 
#           as.matrix(chain(ms_index[1:3, ])))
# 
# ms_index <- aggregate(ms_epr, ms_pias, r = -1.7)
# 
# all.equal(aggregate(ms_index, ms_pias, r = -1.7), ms_index)
# 
# all.equal(as.matrix(ms_index)[1, ], 
#           colSums(contrib(ms_index)) + 1)
# 
# # Tests with a fixed-sample index
# fs_epr <- with(
#   fs_prices, 
#   elemental_index(price_relative(price, period, business),
#                   period, classification, w = weight, contrib = TRUE)
# )
# 
# fs_pias <- with(
#   fs_weights,
#   aggregation_structure(expand_classification(classification), weight)
# )
# 
# (fs_index <- aggregate(fs_epr, fs_pias, na.rm = TRUE))
# 
# unclass(fs_index)
# 
# # Re-aggregating the index shouldn't do anything
# all.equal(as.matrix(aggregate(fs_index, fs_pias)), as.matrix(fs_index))
# 
# # Contributions should add up
# all.equal(as.matrix(fs_index)[1, ], 
#           colSums(contrib(fs_index), na.rm = TRUE) + 1)
# 
# # Check adding up of lower level indexes
# all.equal(apply(as.matrix(chain(fs_index)[5:9, ]), 2, weighted.mean, weights(fs_pias)[[3]]),
#           as.matrix(chain(fs_index))[1, ])
# 
# all.equal(apply(as.matrix(chain(fs_index)[2:4, ]), 2, weighted.mean, weights(fs_pias)[[2]]),
#           as.matrix(chain(fs_index))[1, ])
# 
# # Non-missing indexes should be the same when missing values are not removed
# fs_index2 <- aggregate(fs_epr, fs_pias)
# as.matrix(fs_index2) - as.matrix(fs_index)
# 
# all.equal(as.matrix(fs_index2)["121", ], 
#           colSums(contrib(fs_index2, "121"), na.rm = TRUE) + 1)
# 
# all.equal(as.matrix(fs_index2)["13", 1:3], 
#           contrib(fs_index2, "13")[, 1:3] + 1)
# 
# # Tests with a fixed-base index
# prices <- data.frame(price = 1:15, 
#                      period = letters[1:3], 
#                      product = rep(1:5, each = 3), 
#                      ea = rep(c("f1", "f2"), c(6, 9)))
# prices$pop_rel <- with(prices, price_relative(price, period, product))
# prices$fx_rel <- with(prices, price / price[gpindex::base_period(period, product)])
# 
# pias <- aggregation_structure(list(c("1", "1"), c("f1", "f2")), 1:2)
# 
# epr_pop <- with(prices, elemental_index(pop_rel, period, ea))
# epr_fx <- with(prices, elemental_index(fx_rel, period, ea, chain = FALSE))
# 
# index_pop <- aggregate(epr_pop, pias)
# index_fx <- aggregate(epr_fx, pias)
# 
# # Chained calculation and fixed-base calculation should be the same
# all.equal(index_fx, chain(index_pop))
# all.equal(chain(index_pop[, -1], as.matrix(index_fx[, 1])), index_fx[, -1])
# 
# # Should work for a non-arithmetic index
# all.equal(chain(aggregate(epr_pop, pias, r = 3)), aggregate(epr_fx, pias, r = 3))

# # Test mean.ind()
# epr4 <- mean(epr1, window = 12)
# all.equal(levels(epr4), levels(epr1))
# time(epr4)
# all.equal(as.matrix(epr4)[, 1], rowMeans(as.matrix(epr1)[, 1:12]))
# all.equal(as.matrix(epr4)[, 2], rowMeans(as.matrix(epr1)[, 13:24]))
# is_chainable_index(epr4)
# is_chainable_index(mean(chain(epr1)))
# epr4$contrib
# 
# w <- matrix(seq_len(5 * 26), 5)
# all.equal(as.matrix(mean(epr1, w, window = 12))[, 1], 
#           diag(as.matrix(epr1)[, 1:12] %*% apply(w[, 1:12], 1, scale_weights)), check.attributes = FALSE)
# all.equal(as.matrix(mean(epr1, w, window = 12))[, 2], 
#           diag(as.matrix(epr1)[, 13:24] %*% apply(w[, 13:24], 1, scale_weights)), check.attributes = FALSE)
