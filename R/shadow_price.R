shadow_price <- function(x, period, product, ea, pias = NULL, w = rep(1, length(x)), r1 = 0, r2 = 1) {
  if (!same_length(x, period, product, ea, w)) {
    stop("'x', 'period', 'product', 'ea', and 'w' must be the same length")
  }
  # this is mostly a combination of gpindex::back_price() and aggregate.index()
  # it just does it period-by-period and keeps track of prices to impute
  if (!length(x)) return(x[0])
  period <- as.factor(period)
  res <- split(x, period)
  product <- split(as.integer(as.factor(product)), period)
  warn <- any(vapply(product, anyDuplicated, numeric(1)) > 0)
  if (warn) {
    warning("there are duplicated period-product pairs")
  }
  ea <- split(as.factor(ea), period)
  w <- split(w, period)
  price_update <- weights_factor(r2)
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
