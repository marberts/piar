test_that("expand classification works", {
  expect_equal(expand_classification(c("1.1.1", "1.1.2", "1.2.1"), c(2, 2, 1)),
               list(c("1.", "1.", "1."),
                    c("1.1.", "1.1.", "1.2."),
                    c("1.1.1", "1.1.2", "1.2.1")))
  expect_equal(expand_classification(c("1.1.1", "1.1.2", "1.2.1"), c(3, 2)),
               list(c("1.1", "1.1", "1.2"),
                    c("1.1.1", "1.1.2", "1.2.1")))
  expect_equal(expand_classification(c("1.1.1", "1.1.2", "1.2.1"), 3),
               list(c("1.1", "1.1", "1.2"),
                    c("1.1.1NA", "1.1.2NA", "1.2.1NA")))
  expect_equal(expand_classification(character(0)), list())
})

test_that("expand classification fails when expected", {
  expect_error(expand_classification("123", width = c(1, 0, 1)))
  expect_error(expand_classification("123", width = c(1, NA, 1)))
})
