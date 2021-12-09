#---- Shadow price imputation ----
shadow_price <- function(x, period, product, ea, pias, w, r1 = 0, r2 = 1) {
  if (missing(w)) {
    if (different_length(x, period, product, ea)) {
      stop(gettext("'x', 'period', 'product, and 'ea' must be the same length"))
    }
  } else {
    if (different_length(x, period, product, ea, w)) {
      stop(gettext("'x', 'period', 'product, 'ea', and 'w' must be the same length"))
    }
  }
  # this is mostly a combination of gpindex::back_price() and aggregate.index()
  # it just does it period-by-period and keeps track of prices to impute
  if (!length(x)) return(x[0])
  period <- as.factor(period)
  res <- split(x, period)
  product <- as.factor(product)
  attributes(product) <- NULL
  product <- split(product, period)
  if (max(vapply(product, anyDuplicated, numeric(1), incomparables = NA))) {
    warning(gettext("there are duplicated period-product pairs"))
  }
  ea <- split(as.factor(ea), period)
  if (!missing(w)) w <- split(w, period)
  for (t in seq_along(res)[-1]) {
    # calculate relatives
    matches <- match(product[[t]], product[[t - 1]], incomparables = NA)
    back_price <- res[[t - 1]][matches]
    price <- res[[t]]
    # calculate indexes
    epr <- if (missing(w)) {
      elemental_index(price / back_price, ea = ea[[t]], na.rm = TRUE, r = r1)
    } else {
      elemental_index(price / back_price, ea = ea[[t]], 
                      w = w[[t]], na.rm = TRUE, r = r1)
    }
    if (!missing(pias)) {
      index <- aggregate(epr, pias, na.rm = TRUE, r = r2)
      epr <- as_elemental_index(index)
      pias <- update(pias, index)
    }
    # add shadow prices to 'x'
    impute <- is.na(price)
    res[[t]][impute] <- epr$index[[1]][as.character(ea[[t]][impute])] * back_price[impute]
  }
  res <- unsplit(res, period)
  attributes(res) <- attributes(x)
  res
}

#---- Carry forward imputation ----
carry_forward <- function(x, period, product) {
  if (different_length(x, period, product)) {
    stop(gettext("all arguments must be the same length"))
  }
  if (!length(x)) return(x[0])
  period <- as.factor(period)
  res <- split(x, period)
  product <- as.factor(product)
  attributes(product) <- NULL
  product <- split(product, period)
  if (max(vapply(product, anyDuplicated, numeric(1), incomparables = NA))) {
    warning(gettext("there are duplicated period-product pairs"))
  }
  for (t in seq_along(res)[-1]) {
    impute <- is.na(res[[t]])
    matches <- match(product[[t]][impute], product[[t - 1]], incomparables = NA)
    res[[t]][impute] <- res[[t - 1]][matches]
  }
  res <- unsplit(res, period)
  attributes(res) <- attributes(x)
  res
}
