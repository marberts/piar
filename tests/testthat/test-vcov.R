library(gpindex)

set.seed(4321)

prices <- data.frame(rel = runif(24),
                     period = 1:3,
                     id = rep(letters[1:8], each = 3))

weights <- data.frame(l1 = rep(1, 8),
                      l2 = rep(c(11, 12), each = 4),
                      l3 = rep(c(111, 112, 121, 122), each = 2),
                      l4 = letters[1:8],
                      ew = round(1000 * runif(8)),
                      dw = c(1, runif(6, 1, 10), 1))

epr <- with(prices, elemental_index(rel, period, id))
pias <- with(weights, aggregation_structure(as.list(weights[1:4]), ew * dw))
index <- aggregate(epr, pias)

rw <- matrix(seq_len(8 * 25), 8)

test_that("repweights must be the right size", {
  expect_error(vcov(index, matrix(seq_len(8 * 25), 4)))
})

test_that("corner cases work", {
  expect_equal(
    vcov(aggregate(elemental_index(numeric(0)), aggregation_structure("a")),
         matrix(1:10, ncol = 10)),
    matrix(numeric(0), 0, 1, dimnames = list(NULL, "1"))
  )

  expect_equal(
    vcov(aggregate(elemental_index(numeric(0)), aggregation_structure(1:5)),
         matrix(runif(10), ncol = 10)),
    matrix(NA_real_, 4, 1, dimnames = list(1:4, 1))
  )
})

test_that("mse == FALSE works", {
  covar <- vcov(index, rw * weights$ew, mse = FALSE)

  # Variance for higher levels should agree with manual calculation
  # Period 1
  rws <- apply(rw * weights$ew, 2, scale_weights)
  expect_equal(
    sum(
      crossprod(
        as.matrix(epr[, 1]),
        tcrossprod(sweep(rws, 1, rowMeans(rws))) / 25) * t(as.matrix(epr[, 1]))
      ),
    covar[1, 1] # 0.004973962 according to svymean
  )
  # Period 2
  rws <- apply(rw * weights(update(pias, index, 1), TRUE) / weights$dw, 2,
               scale_weights)
  expect_equal(
    sum(
      crossprod(
        as.matrix(epr[, 2]),
        tcrossprod(sweep(rws, 1, rowMeans(rws))) / 25) * t(as.matrix(epr[, 2]))
      ),
    covar[1, 2] # 0.003076949 according to svymean
  )
})

test_that("mse == TRUE case works", {
  covar <- vcov(index, rw * weights$ew)

  # Variance for higher levels should agree with manual calculation
  # Period 1
  rws <- apply(rw * weights$ew, 2, scale_weights)
  w <- weights(pias, ea_only = TRUE)
  expect_equal(
    sum(
      crossprod(
        as.matrix(epr[, 1]),
        tcrossprod(sweep(rws, 1, scale_weights(w))) / 25) *
          t(as.matrix(epr[, 1]))
      ),
    covar[1, 1] # 0.02001844 according to svymean
  )

  # Period 2
  rws <- apply(rw * weights(update(pias, index, 1), TRUE) / weights$dw, 2,
               scale_weights)
  expect_equal(
    sum(
      crossprod(
        as.matrix(epr[, 2]),
        tcrossprod(
          sweep(
            rws, 1,
            scale_weights(weights(update(pias, index, 1), TRUE)))
          ) / 25
        ) * t(as.matrix(epr[, 2]))
      ),
    covar[1, 2] # 0.008153173 according to svymean
  )
})

test_that("vcov works for chained index", {
  covar <- vcov(chain(index), rw * weights$ew)

  # Variance for higher levels should agree with manual calculation
  # Period 1
  rws <- apply(rw * weights$ew, 2, scale_weights)
  w <- weights(pias, ea_only = TRUE)
  expect_equal(
    sum(
      crossprod(
        as.matrix(epr[, 1]),
        tcrossprod(sweep(rws, 1, scale_weights(w))) / 25) *
          t(as.matrix(epr[, 1]))
    ),
    covar[1, 1] # 0.02001844 according to svymean
  )

  # Period 2
  epr <- chain(epr)
  expect_equal(
    sum(
      crossprod(
        as.matrix(epr[, 2]),
        tcrossprod(sweep(rws, 1, scale_weights(w))) / 25) *
          t(as.matrix(epr[, 2]))
    ),
    covar[1, 2] # 0.004125957 according to svymean
  )
})
