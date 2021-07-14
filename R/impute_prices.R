#---- Shadow price imputation ----
shadow_price <- function(x, period, product, ea, pias = NULL, w = rep(1, length(x)), r1 = 0, r2 = 1) {
  if (!same_length(x, period, product, ea, w)) {
    stop(gettext("'x', 'period', 'product', 'ea', and 'w' must be the same length"))
  }
  # this is mostly a combination of gpindex::back_price() and aggregate.index()
  # it just does it period-by-period and keeps track of prices to impute
  if (!length(x)) return(x[0])
  period <- as.factor(period)
  res <- split(x, period)
  product <- split(as.integer(as.factor(product)), period)
  if (any(vapply(product, anyDuplicated, numeric(1)) > 0)) {
    warning(gettext("there are duplicated period-product pairs"))
  }
  ea <- split(as.factor(ea), period)
  w <- split(w, period)
  price_update <- factor_weights(r2)
  for (t in seq_along(res)[-1]) {
    # calculate relatives
    matches <- match(product[[t]], product[[t - 1]], incomparables = NA)
    back_price <- res[[t - 1]][matches]
    price <- res[[t]]
    # calculate indexes
    epr <- elemental_index(price / back_price, ea = ea[[t]], 
                           w = w[[t]], na.rm = TRUE, r = r1)
    if (!is.null(pias)) {
      index <- aggregate(epr, pias, na.rm = TRUE, r = r2)
      epr <- index[names(pias$weights)]
      if (pias$height) pias$weights <- price_update(epr, pias$weights)
    }
    # add shadow prices to 'x'
    impute <- is.na(price)
    res[[t]][impute] <- epr[as.character(ea[[t]][impute])] * back_price[impute]
  }
  res <- unsplit(res, period)
  attributes(res) <- attributes(x)
  res
}

#---- Carry forward imputation ----
carry_forward <- function(x, period, product) {
  if (!same_length(x, period, product)) {
    stop(gettext("all arguments must be the same length"))
  }
  if (!length(x)) return(x[0])
  period <- as.factor(period)
  res <- split(x, period)
  product <- split(as.integer(as.factor(product)), period)
  if (any(vapply(product, anyDuplicated, numeric(1)) > 0)) {
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