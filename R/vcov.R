# Fast aggregate
.aggregate <- function(x, pias, chained, r) {
  # helpful functions
  price_update <- factor_weights(r)
  gen_mean <- generalized_mean(r)
  # 'i' is defined in the loop below; it's used to loop over the height of 'pias'
  aggregate_index <- function(z) {
    gen_mean(rel[[i - 1]][z], w[[i - 1]][z])
  }
  # initialize weights
  eas <- pias$eas
  w <- rev(weights(pias))
  # loop over each time period
  for (t in seq_along(x$periods)) {
    rel <- vector("list", pias$height)
    # align epr with weights so that positional indexing works
    # preserve names if epr and pias weights don't agree
    rel[[1]] <- named_extract(x$index[[t]], eas)
    if (t > 1 && chained) w <- rev(weights(pias))
    # loop over each level in the pias from the bottom up and aggregate
    for (i in seq_along(rel)[-1]) {
      rel[[i]] <- vapply(pias$child[[i - 1]], aggregate_index, numeric(1))
    }
    # return index and contributions
    index <- unlist(rev(rel))
    x$index[[t]] <- index
    # price update weights for all periods after the first
    if (pias$height && chained) {
      pias$weights <- price_update(index[eas], w[[1]]) 
    }
  }
  list2matrix(x$index)
}

vcov.aggregate <- function(object, repweights, mse = TRUE, ...) {
  repweights <- as.matrix(repweights)
  if (nrow(repweights) != length(object$pias$eas)) {
    stop(gettext("'repweights' must have a row for each weight in 'pias'"))
  }
  n <- ncol(repweights)
  eas <- object$pias$eas
  pias <- structure(list(parent = object$pias$parent,
                         child = object$pias$child,
                         levels = object$levels,
                         eas = object$pias$eas,
                         weights = structure(numeric(length(eas)), names = eas),
                         height = object$pias$height),
                    class = "pias")
  upper <- setdiff(object$levels, object$pias$eas)
  dimnm <- list(upper, object$periods, seq_len(n))
  index_boot <- array(0, dim = lengths(dimnm), dimnames = dimnm)
  for (i in seq_len(n)) {
    pias$weights[] <- repweights[, i]
    index_boot[, , i] <- .aggregate(object, pias, object$chained, object$r)[upper, , drop = FALSE]
  }
  centre <- if (mse) {
    as.matrix(object)[upper, , drop = FALSE]
  } else {
    apply(index_boot, 2, rowMeans)
  }
  res <- array(0, lengths(dimnm[c(1, 1, 2)]), dimnames = dimnm[c(1, 1, 2)])
  res[] <- apply(sweep(index_boot, 1:2, centre), 2, tcrossprod) / n
  res
}
