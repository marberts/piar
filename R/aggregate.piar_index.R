aggregate.piar_index <- function(x, pias, na.rm = FALSE, r = 1,
                                 ..., chainable) {
  pias <- as_aggregation_structure(pias)
  r <- as.numeric(r)

  # helpful functions
  price_update <- factor_weights(r)
  gen_mean <- generalized_mean(r)
  aw <- transmute_weights(r, 1)

  # put the aggregation weights upside down to line up with pias
  w <- rev(weights(pias, na.rm = na.rm))
  has_contrib <- has_contrib(x)
  pias_eas <- match(pias$eas, pias$levels) 
  # loop over each time period
  for (t in seq_along(x$time)) {
    rel <- con <- vector("list", pias$height)
    # align epr with weights so that positional indexing works
    # preserve names if epr and pias weights don't agree
    eas <- match(pias$eas, x$levels)
    rel[[1L]] <- x$index[[t]][eas]
    con[[1L]] <- x$contrib[[t]][eas]
    #names(rel[[1L]]) <- names(con[[1L]]) <- pias$eas
    # get rid of any NULL contributions
    con[[1L]][lengths(con[[1L]]) == 0L] <- list(numeric(0L))
    # loop over each level in the pias from the bottom up and aggregate
    for (i in seq_along(rel)[-1L]) {
      rel[[i]] <- vapply(
        pias$child[[i - 1L]],
        \(z) gen_mean(rel[[i - 1L]][z], w[[i - 1L]][z], na.rm = na.rm),
        numeric(1L)
      )
      if (has_contrib) {
        con[[i]] <- lapply(
          pias$child[[i - 1L]],
          \(z) {
            w <- scale_weights(aw(rel[[i - 1L]][z], w[[i - 1L]][z]))
            unlist(Map("*", con[[i - 1L]][z], w))
          }
        )
      } else {
        con[[i]] <- lapply(pias$child[[i - 1L]], \(z) numeric(0L))
      }

    }
    # parental imputation
    if (na.rm) {
      for (i in rev(seq_along(rel))[-1L]) {
        impute <- is.na(rel[[i]])
        rel[[i]][impute] <- rel[[i + 1L]][pias$parent[[i]][impute]]
      }
    }
    # return index and contributions
    index <- unlist(rev(rel), use.names = FALSE)
    x$index[[t]] <- index
    x$contrib[[t]] <- unlist(rev(con), recursive = FALSE, use.names = FALSE)
    # price update weights for all periods after the first
    if (chainable) {
      weights(pias) <- price_update(index[pias_eas], w[[1L]])
      w <- rev(weights(pias, na.rm = na.rm))
    }
  }
  aggregate_piar_index(x$index, x$contrib, pias$levels, x$time, r,
                       pias[c("child", "parent", "eas", "height")],
                       chainable)
}

aggregate.chainable_piar_index <- function(x, pias, na.rm = FALSE, r = 1, ...) {
  NextMethod("aggregate", chainable = TRUE)
}

aggregate.direct_piar_index <- function(x, pias, na.rm = FALSE, r = 1, ...) {
  NextMethod("aggregate", chainable = FALSE)
}
