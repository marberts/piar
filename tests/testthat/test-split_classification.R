test_that("split agrees with expand", {
  expect_identical(
    split_classification(c("111", "112", "121"), "", sep = ""),
    expand_classification(c("111", "112", "121"))
  )
  expect_identical(
    split_classification(c("01.1.1", "01.1.2", "01.2.1"), ".", fixed = TRUE),
    expand_classification(c("01.1.1", "01.1.2", "01.2.1"), width = 2)
  )
})

test_that("missing levels are padded with NA", {
  expect_identical(
    split_classification(c("ab.c.d", "a.bc..e"), ".", fixed = T),
    list(
      c("ab", "a"),
      c("ab.c", "a.bc"),
      c("ab.c.d", "a.bc."),
      c("ab.c.d.NA", "a.bc..e")
    )
  )
})

test_that("works with empty strings", {
  expect_identical(split_classification(character(0), "1"), list())
  expect_identical(split_classification(c("", ""), "1"), list())
  expect_identical(split_classification(c("", "a"), "a"),
                   list(c(NA, "")))
})
