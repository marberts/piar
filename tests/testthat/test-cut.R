pias <- aggregation_structure(
  list(
    c(1, 1, 1, 2, 2, 1),
    c(11, 11, 12, 21, 22, 12),
    c(112, 111, 121, 211, 221, 121),
    c(1121, 1111, 1211, 2111, 2211, 1212),
    letters[1:6]
  )
)

test_that("cutting a pias work with upper == TRUE", {
  expect_equal(cut(pias, 5), pias)
  
  expect_identical(
    cut(pias, 4),
    aggregation_structure(
      list(
        c(1, 1, 1, 2, 2, 1),
        c(11, 11, 12, 21, 22, 12),
        c(112, 111, 121, 211, 221, 121),
        c(1121, 1111, 1211, 2111, 2211, 1212)
      )
    )
  )
  
  expect_identical(
    cut(pias, 3),
    aggregation_structure(
      list(
        c(1, 1, 1, 2, 2),
        c(11, 11, 12, 21, 22),
        c(112, 111, 121, 211, 221)
      ),
      weights = c(1, 1, 2, 1, 1)
    )
  )
  
  expect_identical(
    cut(pias, 2),
    aggregation_structure(
      list(
        c(1, 1, 2, 2),
        c(11, 12, 21, 22)
      ),
      weights = c(2, 2, 1, 1)
    )
  )
  
  expect_identical(
    cut(pias, 1),
    aggregation_structure(
      list(
        c(1, 2)
      ),
      weights = c(4, 2)
    )
  )
})

test_that("cutting a pias work with upper == FALSE", {
  expect_equal(cut(pias, 1, upper = FALSE), pias)
  
  expect_identical(
    cut(pias, 2, upper = FALSE),
    aggregation_structure(
      list(
        c(11, 11, 12, 21, 22, 12),
        c(112, 111, 121, 211, 221, 121),
        c(1121, 1111, 1211, 2111, 2211, 1212),
        letters[1:6]
      )
    )
  )
  
  expect_identical(
    cut(pias, 3, upper = FALSE),
    aggregation_structure(
      list(
        c(112, 111, 121, 211, 221, 121),
        c(1121, 1111, 1211, 2111, 2211, 1212),
        letters[1:6]
      )
    )
  )
  
  expect_identical(
    cut(pias, 4, upper = FALSE),
    aggregation_structure(
      list(
        c(1121, 1111, 1211, 2111, 2211, 1212),
        letters[1:6]
      )
    )
  )
  
  expect_identical(
    cut(pias, 5, upper = FALSE),
    aggregation_structure(
      list(
        letters[1:6]
      )
    )
  )
})

test_that("errors work", {
  expect_error(cut(pias, 0))
  expect_error(cut(pias, 6))
})

test_that("length-1 pias works", {
  expect_identical(cut(cut(pias, 1), 1, upper = FALSE), cut(pias, 1))
  expect_identical(cut(cut(pias, 1), 1), cut(pias, 1))
})