vcov.aggregate_index <- function(object, repweights, mse = TRUE, ...) {
  repweights <- as.matrix(repweights)
  eas <- object$pias$eas
  if (nrow(repweights) != length(eas)) {
    stop("'repweights' must have a row for each weight in 'pias'")
  }
  upper <- setdiff(object$levels, eas)
  n <- ncol(repweights)
  r <- object$r
  # template aggregation structure with no weights for each bootstrap replicate
  pias <- as_aggregation_structure(object)
  # matrix aggregation is much faster than aggregate(), but needs to be
  # done with a chained index
  elem <- as.matrix(chain(object[eas, ]))
  repindex <- lapply(seq_len(n), function(i) {
    weights(pias) <- repweights[, i]
    res <- (as.matrix(pias) %*% elem^r)^(1 / r)
    # undo chaining for a period-over-period index
    if (is_chainable_index(object)) {
      res[, -1] <- res[, -1] / res[, -ncol(res)]
    }
    res
  })
  # it's easier to calculate the variance with an array of indexes
  dimnm <- list(upper, object$time, seq_len(n))
  repindex <- array(unlist(repindex, use.names = FALSE),
                    dim = lengths(dimnm), dimnames = dimnm)
  # mse = TRUE is the default for variance estimation in SAS,
  # but not the survey package
  centre <- if (mse) {
    as.matrix(object)[upper, , drop = FALSE]
  } else {
    apply(repindex, 2L, rowMeans)
  }
  apply(sweep(repindex, 1:2, centre), 1:2, crossprod) / n
}
