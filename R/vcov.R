vcov.aggregate <- function(object, pias, repweights, mse = TRUE) {
  if (!inherits(pias, "pias")) {
    stop(gettext("'pias' must be a price index aggregation structure; use aggregation_structure() to make one"))
  }
  eas <- names(pias$weights)
  upper <- setdiff(pias$levels, eas)
  repweights <- as.matrix(repweights)
  if (!setequal(eas, rownames(repweights))) {
    stop(gettext("'pias' and 'rep_weights' must have the same weights"))
  }
  repweights <- repweights[eas, , drop = FALSE]
  n <- ncol(repweights)
  dimnm <- list(upper, object$periods, seq_len(n))
  index_boot <- array(0, dim = lengths(dimnm), dimnames = dimnm)
  for (i in seq_len(n)) {
    pias$weights <- repweights[, i]
    index_boot[, , i] <- aggregate(object, pias, na.rm = TRUE)[upper, ]
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
