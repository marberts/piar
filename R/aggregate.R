aggregate.index <- function(x, pias, na.rm = FALSE, r = 1, ...) {
  if (!inherits(pias, "pias")) {
    stop(gettext("'pias' must be a price index aggregation structure; use aggregation_structure() to make one"))
  }
  # helpful functions
  price_update <- factor_weights(r)
  gen_mean <- generalized_mean(r)
  arithmetic_weights <- transmute_weights(r, 1)
  # functions to aggregate index values and contributions
  # 'i' is defined in the loop below; it's used to loop over the height of 'pias'
  aggregate_index <- function(z) {
    gen_mean(rel[[i - 1]][z], w[[i - 1]][z], na.rm = na.rm)
  }
  aggregate_contrib <- if (x$has_contrib) {
    function(z) {
      aw <- scale_weights(arithmetic_weights(rel[[i - 1]][z], w[[i - 1]][z]))
      unlist(Map("*", con[[i - 1]][z], aw))
    }
  } else {
    function(z) numeric(0)
  }
  # initialize weights
  eas <- pias$eas
  w <- rev(weights(pias, na.rm = na.rm))
  # loop over each time period
  for (t in seq_along(x$time)) {
    rel <- con <- vector("list", pias$height)
    # align epr with weights so that positional indexing works
    # preserve names if epr and pias weights don't agree
    rel[[1]] <- named_extract(x$index[[t]], eas)
    con[[1]] <- named_extract(x$contrib[[t]], eas)
    # get rid of any NULL contributions
    con[[1]][!lengths(con[[1]])] <- list(numeric(0))
    # re-aggregate price-updated weights for all periods after first
    if (t > 1 && x$chained) {
      w <- rev(weights(pias, na.rm = na.rm))
    }
    # loop over each level in the pias from the bottom up and aggregate
    for (i in seq_along(rel)[-1]) {
      rel[[i]] <- vapply(pias$child[[i - 1]], aggregate_index, numeric(1))
      con[[i]] <- lapply(pias$child[[i - 1]], aggregate_contrib)
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
    x$contrib[[t]] <- unlist(rev(con), recursive = FALSE)
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
