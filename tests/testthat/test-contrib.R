dat <- data.frame(rel = c(1:6, NA, 7, 8),
                  period = c(1, 1, 1, 1, 1, 2, 2, 2, 2),
                  ea = c("11", "11", "12", "12", "13", "11", "12", "11", "14"))

pias <- as_aggregation_structure(
  data.frame(level1 = 1, level2 = c(11, 12, 13, 14), weight = 1)
)

epr <- elemental_index(
  dat, setNames(rel, c(1:5, 1, 3, 2, 6)) ~ period + ea, contrib = TRUE
)
index <- aggregate(epr, pias, na.rm = TRUE)
epr2 <- elemental_index(dat, rel ~ period + ea)

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
  expect_equal(
    contrib(epr, 12, 2),
    matrix(NA_real_, 1, 1, dimnames = list(3, 2))
  )
})

test_that("contrib2DF works", {
  expect_equal(
    contrib2DF(epr),
    data.frame(
      period = c("1", "1", "2", "2"),
      level = c("11", "11", "11", "11"),
      product = c("1", "2", "1", "2"),
      value = as.numeric(contrib(epr))
    )
  )
  
  expect_equal(
    contrib2DF(epr, levels(epr), 2),
    data.frame(
      period = c("2", "2", "2", "2"),
      level = c("11", "11", "12", "14"),
      product = c("1", "2", "3", "6"),
      value = c(
        contrib(epr, "11", 2), contrib(epr, "12", 2), contrib(epr, "14", 2)
      )
    )
  )
  
  expect_equal(
    contrib2DF(epr2),
    data.frame(
      period = character(0),
      level = character(0),
      product = character(0),
      value = numeric(0)
    )
  )
})

test_that("padding work", {
  expect_equal(
    contrib(epr, 12, pad = -99),
    matrix(c(1.07179676972449, 1.39230484541326, NA, -99), 2, 2,
           dimnames = list(3:4, 1:2))
  )
  expect_error(contrib(epr, 12, pad = 1:2))
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
        period = c(1, 1, 1, 2, 2), ea = gl(1, 5), contrib = TRUE, r = 1
      ))
    ),
    matrix(c(0, 0, 1 / 3, 2 / 3, 1.5, 2, 0, 0), 4, 2,
           dimnames = list(c("a", "a.1", "b", "c"), 1:2))
  )

  w <- setNames(rep(1, 9), letters[1:9])
  expect_equal(
    contrib(epr),
    contrib(
      elemental_index(dat, rel ~ period + ea,
        weights = w, contrib = TRUE,
        product = c(1:5, 1, 3, 2, 6)
      )
    )
  )
})

test_that("replacement works", {
  # No op.
  epr2 <- epr
  contrib(epr) <- contrib(epr)
  expect_identical(epr, epr2)
  
  # Replacing with change in index.
  contrib(epr) <- matrix(as.numeric(epr[1]) - 1, 1)
  expect_equal(
    contrib(epr),
    matrix(as.numeric(epr[1]) - 1, 1, dimnames = list(1, 1:2))
  )
  
  # Deleting contributions, used to be give an error.
  contrib(epr, 12) <- numeric(0)
  expect_equal(
    contrib2DF(epr, 12),
    data.frame(
      period = character(0),
      level = character(0),
      product = character(0),
      value = numeric(0)
    )
  )
  
  # Adding contributions.
  contrib(epr, 14) <- matrix(c(NA, 2:3, 6, 1, NA), 3, dimnames = list(letters[1:3], NULL))
  expect_equal(
    contrib(epr, 14),
    matrix(c(NA, 2:3, 6, 1, NA), 3, dimnames = list(letters[1:3], 1:2))
  )
  
  # Errors work.
  expect_error(contrib(epr, period = 1) <- 1)
  expect_error(contrib(epr, 14, 1) <- 1:3)
  expect_error(contrib(epr) <- matrix(0, 0, 0))
  expect_warning(contrib(epr) <- matrix(c(as.numeric(epr[1]), 3) - 1, 1, 3))
  expect_warning(contrib(epr, 14, 1) <- c(a = 1, a = NA))
})
