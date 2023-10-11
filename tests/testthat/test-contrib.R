dat <- data.frame(rel = c(1:6, NA, 7, 8),
                  period = c(1, 1, 1, 1, 1, 2, 2, 2, 2),
                  ea = c("11", "11", "12", "12", "13", "11", "12", "11", "14"))

pias <- as_aggregation_structure(
  data.frame(level1 = 1, level2 = c(11, 12, 13, 14), weight = 1)
)

epr <- with(
  dat,
  elemental_index(setNames(rel, c(1:5, 1, 3, 2, 6)), period, ea, contrib = TRUE)
)
index <- aggregate(epr, pias, na.rm = TRUE)
epr2 <- with(dat, elemental_index(rel, period, ea))

test_that("contrib works", {
  expect_equal(
    contrib(epr),
    matrix(c(0, 0.414213562373095, 2.5962965079607, 2.88444419044716),
           2, 2, dimnames = list(c("1", "2"), 1:2))
  )
  expect_equal(
    contrib(epr, "14"),
    matrix(c(0, 7),
           1, 2, dimnames = list("6", 1:2))
  )
  expect_equal(
    contrib(epr2),
    matrix(numeric(0), 0, 2, dimnames = list(NULL, 1:2))
  )
})

test_that("aggregate contributions have the right form", {
  contributions <- contrib(index)
  expect_equal(rownames(contributions),
               c("1", "2", "3", "4", "5", "6"))
  expect_equal(colnames(contributions), c("1", "2"))
})

test_that("product names are correct", {
  expect_equal(
    suppressWarnings(
      contrib(elemental_index(c(a = 1, b = 2, c = 3, a = 4, a = 5),
                              c(1, 1, 1, 2, 2), contrib = TRUE, r = 1))
    ),
    matrix(c(0, 1 / 3, 2 / 3, 0, 1.5, 0, 0, 2), 4, 2,
           dimnames = list(c("a", "b", "c", "a.1"), 1:2))
  )

  w <- setNames(rep(1, 9), letters[1:9])
  expect_equal(
    contrib(epr),
    contrib(
      with(dat, elemental_index(setNames(rel, c(1:5, 1, 3, 2, 6)),
                                period, ea, w, contrib = TRUE))
    )
  )
})
