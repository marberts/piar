#---- Shadow price imputation ----
shadow_price <- function(x, period, product, ea,
                         pias = NULL, w = NULL, r1 = 0, r2 = 1) {
  if (different_length(x, period, product, ea, w)) {
    stop("input vectors must be the same length")
  }
  # this is mostly a combination of gpindex::back_period() and aggregate()
  # it just does it period-by-period and keeps track of prices to impute
  period <- as.factor(period)
  if (nlevels(period) == 0L) {
    return(rep.int(NA_integer_, length(period)))
  }

  res <- split(x, period)
  product <- as.factor(product)
  attributes(product) <- NULL
  product <- split(product, period)
  if (max(vapply(product, anyDuplicated, numeric(1L), incomparables = NA))) {
    warning("there are duplicated period-product pairs")
  }
  ea <- split(as.factor(ea), period)
  if (is.null(w)) {
    w <- rep.int(list(NULL), nlevels(period))
  } else {
    w <- split(as.numeric(w), period)
  }
  if (!is.null(pias)) {
    pias <- as_aggregation_structure(pias)
  }
  for (t in seq_along(res)[-1L]) {
    # calculate relatives
    matches <- match(product[[t]], product[[t - 1L]], incomparables = NA)
    back_price <- res[[t - 1L]][matches]
    price <- res[[t]]
    # calculate indexes
    epr <- elemental_index(price / back_price, ea = ea[[t]],
                           w = w[[t]], na.rm = TRUE, r = r1)
    if (!is.null(pias)) {
      epr <- aggregate(epr, pias, na.rm = TRUE, r = r2)
      pias <- update(pias, epr)
    }
    # add shadow prices to 'x'
    impute <- is.na(price)
    eas <- match(as.character(ea[[t]][impute]), epr$levels)
    res[[t]][impute] <- epr$index[[1L]][eas] * back_price[impute]
  }
  res <- unsplit(res, period)
  attributes(res) <- attributes(x)
  res
}

#---- Carry forward imputation ----
carry_forward <- function(x, period, product) {
  if (different_length(x, period, product)) {
    stop("input vectors must be the same length")
  }
  period <- as.factor(period)
  if (nlevels(period) == 0L) {
    return(rep.int(NA_integer_, length(period)))
  }

  res <- split(x, period)
  product <- as.factor(product)
  attributes(product) <- NULL
  product <- split(product, period)
  if (max(vapply(product, anyDuplicated, numeric(1L), incomparables = NA))) {
    warning("there are duplicated period-product pairs")
  }
  for (t in seq_along(res)[-1L]) {
    impute <- is.na(res[[t]])
    matches <- match(product[[t]][impute],
                     product[[t - 1L]],
                     incomparables = NA)
    res[[t]][impute] <- res[[t - 1L]][matches]
  }
  res <- unsplit(res, period)
  attributes(res) <- attributes(x)
  res
}
