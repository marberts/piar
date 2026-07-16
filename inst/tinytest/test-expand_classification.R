# expand classification works.
local({
  expect_equal(
    expand_classification(c("1.1.1", "1.1.2", "1.2.1"), c(2, 2, 1)),
    list(
      c("1.", "1.", "1."),
      c("1.1.", "1.1.", "1.2."),
      c("1.1.1", "1.1.2", "1.2.1")
    )
  )
  expect_equal(
    expand_classification(c("1.1.1", "1.1.2", "1.2.1"), c(3, 2)),
    list(
      c("1.1", "1.1", "1.2"),
      c("1.1.1", "1.1.2", "1.2.1")
    )
  )
  expect_equal(
    expand_classification(c("1.1.1", "1.1.2", "1.2.1"), 3),
    list(
      c("1.1", "1.1", "1.2"),
      c("1.1.1NA", "1.1.2NA", "1.2.1NA")
    )
  )
  expect_equal(expand_classification(character(0)), list())
  expect_equal(expand_classification(c("", "")), list(c("NA", "NA")))
})

# Expand classification fails when expected.
local({
  expect_error(expand_classification("123", width = c(1, 0, 1)))
  expect_error(expand_classification("123", width = c(1, NA, 1)))
  expect_error(expand_classification("123", pad = 1:3))
  expect_error(expand_classification("123", pad = NULL))
})

# Expand classification works with unbalanced classification.
local({
  expect_identical(
    expand_classification(c(1234, 12345), c(1, 3, 1), pad = 0),
    list(c("1", "1"), c("1234", "1234"), c("12340", "12345"))
  )
  expect_identical(
    expand_classification(c(1234, 12345), c(1, 3, 1), pad = "00"),
    list(c("1", "1"), c("1234", "1234"), c("123400", "12345"))
  )
})

# Interaction works.
local({
  c1 <- expand_classification(c(11, 11, 12, 12))
  c2 <- expand_classification(c(111, 112, 121, 122))

  expect_identical(
    interact_classifications(c1),
    list(c1)
  )

  expect_identical(
    interact_classifications(c1, c2),
    list(
      list(
        c("1:1", "1:1", "1:1", "1:1"),
        c("1:11", "1:11", "1:12", "1:12"),
        c("1:111", "1:112", "1:121", "1:122")
      ),
      list(
        c("11:1", "11:1", "12:1", "12:1"),
        c("11:11", "11:11", "12:12", "12:12"),
        c("11:111", "11:112", "12:121", "12:122")
      )
    )
  )

  expect_identical(
    interact_classifications(list(rep(1, 4)), c1, c2),
    list(
      list(
        c("1:1:1", "1:1:1", "1:1:1", "1:1:1"),
        c("1:1:11", "1:1:11", "1:1:12", "1:1:12"),
        c("1:1:111", "1:1:112", "1:1:121", "1:1:122")
      ),
      list(
        c("1:11:1", "1:11:1", "1:12:1", "1:12:1"),
        c("1:11:11", "1:11:11", "1:12:12", "1:12:12"),
        c("1:11:111", "1:11:112", "1:12:121", "1:12:122")
      )
    )
  )

  expect_error(interact_classifications(c1, list(c2)))
  expect_error(interact_classifications(list(), c1))
  expect_error(interact_classifications(c1, list(1)))
})

# Interacting a single classification does nothing.
local({
  expect_identical(interact_classifications(), list())
  expect_identical(interact_classifications(list(), list()), list())
  expect_identical(interact_classifications(list(1)), list(list(1)))

  x <- expand_classification(c("1.1.1", "1.1.2", "1.2.1"), c(2, 2, 1))
  expect_identical(interact_classifications(x)[[1]], x)
})
