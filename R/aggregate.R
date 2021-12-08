aggregate.index <- function(x, pias, na.rm = FALSE, r = 1, ...) {
  if (!inherits(pias, "pias")) {
    stop(gettext("'pias' must be a price index aggregation structure; use aggregation_structure() to make one"))
  }
  # helpful functions
  price_update <- factor_weights(r)
  gen_mean <- generalized_mean(r)
  aw <- arithmetic_weights(r)
  # functions to aggregate index values and contributions
  # 'i' is defined in the loop below; it's used to loop over the height of 'pias'
  aggregate_index <- function(z) {
    gen_mean(rel[[i - 1]][z], w[[i - 1]][z], na.rm = na.rm)
  }
  aggregate_contrib <- if (x$has_contrib) {
    function(z) {
      res <- Map(`*`, conw[[i - 1]][z], aw(rel[[i - 1]][z], w[[i - 1]][z]))
      unlist(res)
    }
  } else {
    function(z) numeric(0)
  }
  aggregate_rel <- if (x$has_contrib) {
    function(z) unlist(conr[[i - 1]][z])
  } else {
    function(z) numeric(0)
  }
  # initialize weights
  eas <- pias$eas
  x$weights <- structure(rep(list(pias$weights), length(x$time)), 
                         names = x$time)
  w <- rev(weights(pias, na.rm = na.rm))
  # loop over each time period
  for (t in seq_along(x$time)) {
    rel <- conw <- conr <- vector("list", pias$height)
    # align epr with weights so that positional indexing works
    # preserve names if epr and pias weights don't agree
    rel[[1]] <- named_extract(x$index[[t]], eas)
    conw[[1]] <- named_extract(x$contrib$w[[t]], eas)
    conr[[1]] <- named_extract(x$contrib$r[[t]], eas)
    # get rid of any NULL contributions
    conw[[1]][!lengths(conw[[1]])] <- list(numeric(0))
    conr[[1]][!lengths(conr[[1]])] <- list(numeric(0))
    # re-aggregate price-updated weights for all periods after first
    if (t > 1 && x$chained) {
      w <- rev(weights(pias, na.rm = na.rm))
      x$weights[[t]] <- w[[1]]
    }
    # loop over each level in the pias from the bottom up and aggregate
    for (i in seq_along(rel)[-1]) {
      rel[[i]] <- vapply(pias$child[[i - 1]], aggregate_index, numeric(1))
      conw[[i]] <- lapply(pias$child[[i - 1]], aggregate_contrib)
      conr[[i]] <- lapply(pias$child[[i - 1]], aggregate_rel)
    }
    # parental imputation
    if (na.rm) {
      for (i in rev(seq_along(rel))[-1]) {
        impute <- is.na(rel[[i]])
        rel[[i]][impute] <- rel[[i + 1]][pias$parent[[i]][impute]]
      }
    }
    # return index and contributions
    index <- unlist(rev(rel))
    x$index[[t]] <- index
    x$contrib$w[[t]] <- unlist(rev(conw), recursive = FALSE)
    x$contrib$r[[t]] <- unlist(rev(conr), recursive = FALSE)
    # price update weights for all periods after the first
    if (pias$height && x$chained) {
      pias$weights <- price_update(index[eas], w[[1]]) 
    }
  }
  x$levels <- pias$levels
  x$r <- r
  x$pias <- pias[c("child", "parent", "eas", "height")]
  structure(x, class = c("aggregate", "index"))
}
