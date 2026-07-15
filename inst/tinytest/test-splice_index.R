x <- list(1:3, 3:5, 5:7)

# Key splice methods works.
local({
  # Movement splice.
  expect_equal(splice_index(x, 3), c(1, 2, 6, 30, 210))
  expect_equal(splice_index(x, 3, published = TRUE), c(1, 2, 6, 30, 210))
  # Window splice.
  expect_equal(splice_index(x, 1), c(1, 2, 6, 60, 630))
  expect_equal(splice_index(x, 1, published = TRUE), c(1, 2, 6, 60, 420))
  # Mean splice.
  expect_equal(
    splice_index(x),
    c(
      1,
      2,
      6,
      gmean(c(60, 40, 30), order = 0),
      gmean(rev(cumprod(rev(5:7))) * 3:5 * c(1, 2, 6), order = 0)
    )
  )
  expect_equal(
    splice_index(x, published = TRUE),
    c(
      1,
      2,
      6,
      gmean(c(60, 40, 30), order = 0),
      gmean(c(420, 252, 7 * gmean(c(60, 40, 30), order = 0)), order = 0)
    )
  )
})

# Result length is correct.
local({
  expect_equal(
    splice_index(x[-1], 3, c(1, 1, 1, 1, 2, 3)),
    c(1, 1, 1, 1, 2, 6, 30, 210)
  )
  expect_equal(
    splice_index(x[-1], 1, c(1, 1, 1, 1, 2, 3)),
    c(1, 1, 1, 1, 2, 6, 60, 630)
  )
  expect_equal(
    splice_index(x[-1], 1, c(1, 1, 1, 1, 2, 3), published = TRUE),
    c(1, 1, 1, 1, 2, 6, 60, 420)
  )
})

# Period subscripting works.
local({
  expect_equal(
    splice_index(x[-1], rep(3, 10), c(1, 1, 1, 1, 2, 3)),
    c(1, 1, 1, 1, 2, 6, 30, 210)
  )
  expect_equal(
    splice_index(x[-1], 4, c(1, 1, 1, 1, 2, 3)),
    c(1, 1, 1, 1, 2, 6, NA, NA)
  )
  expect_equal(
    splice_index(x[-1], -(2:3), c(1, 1, 1, 1, 2, 3), published = TRUE),
    c(1, 1, 1, 1, 2, 6, 60, 420)
  )
})

# Splicing is invariant.
local({
  # Movement
  expect_equal(splice_index(x, 3), splice_index(x[-1], 3, x[[1]]))
  expect_equal(splice_index(x, 3), splice_index(x[-(1:2)], 3, c(1, 2, 3, 5)))
  # Window
  expect_equal(splice_index(x, 1), splice_index(x[-1], 1, x[[1]]))
  expect_equal(
    splice_index(x, 1, published = TRUE),
    splice_index(x[-(1:2)], 1, c(1, 2, 3, 10), published = TRUE)
  )
  # Mean
  expect_equal(splice_index(x), splice_index(x[-1], initial = x[[1]]))
  expect_equal(
    splice_index(x, published = TRUE),
    splice_index(
      x[-(1:2)],
      initial = c(1, 2, 3, gmean(c(10, 40 / 6, 5), order = 0)),
      published = TRUE
    )
  )
})

# NAs return NA.
local({
  expect_equal(splice_index(x, c(1, NA, 4)), c(1, 2, 6, NA, NA))
  x[[2]][2] <- NA
  expect_equal(splice_index(x), c(1, 2, 6, NA, NA))
  expect_equal(splice_index(x, 3), c(1, 2, 6, 30, 210))
})

# Corner cases.
local({
  expect_equal(splice_index(list()), numeric(0))
  expect_equal(splice_index(list(), initial = 1:5), cumprod(1:5))
  expect_equal(splice_index(list(1:5)), cumprod(1:5))
  expect_equal(splice_index(list(1, 2, 3, 4, 5)), cumprod(1:5))
})

# Errors.
local(
  {
    expect_error(splice_index(list(1:3, 1:2)))
    expect_error(splice_index(list(1:3, 1:3), initial = 1:2))
  }
)

# Splicing is the same as chaining.
local({
  x <- as.list(1:5)
  expect_equal(splice_index(x), cumprod(1:5))
  expect_equal(splice_index(x, initial = 1:3), cumprod(c(1:3, 1:5)))
  expect_equal(splice_index(x, published = TRUE), cumprod(1:5))
})

# Splicing keeps names.
local({
  x <- list(1:3, c(a = 1, b = 2, c = 3), z = c(1:2, c = 3))
  expect_identical(names(splice_index(x)), c("", "", "", "c", "z.c"))
})
