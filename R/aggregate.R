aggregate.index <- function(x, pias, chained = TRUE, na.rm = FALSE, r = 1, ...) {
  if (!inherits(pias, "pias")) {
    stop(gettext("'pias' must be a price index aggregation structure; use aggregation_structure() to make one"))
  }
  # helpful functions
  price_update <- factor_weights(r)
  gen_mean <- generalized_mean(r)
  arithmetic_weights <- transmute_weights(r, 1)
  # 'i' is defined in the loop below; it's used to loop over the height of 'pias'
  aggregate_index <- function(z) {
    gen_mean(rel[[i - 1]][z], w[[i - 1]][z], na.rm = na.rm)
  }
  aggregate_contrib <- function(z) {
    aw <- scale_weights(arithmetic_weights(rel[[i - 1]][z], w[[i - 1]][z]))
    unlist(Map("*", con[[i - 1]][z], aw))
  }
  # initialize weights
  eas <- pias$eas
  x$weights <- structure(rep(list(pias$weights), length(x$periods)), names = x$periods)
  w <- rev(weights(pias, na.rm = na.rm))
  # loop over each time period
  for (t in seq_along(x$periods)) {
    rel <- con <- vector("list", pias$height)
    # align epr with weights so that positional indexing works
    # preserve names if epr and pias weights don't agree
    rel[[1]] <- named_extract(x$index[[t]], eas)
    con[[1]] <- named_extract(x$contributions[[t]], eas)
    # get rid of any NULL contributions
    con[[1]][!lengths(con[[1]])] <- list(numeric(0))
    if (t > 1 && chained) {
      w <- rev(weights(pias, na.rm = na.rm))
      x$weights[[t]] <- w[[1]]
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
    x$contributions[[t]] <- unlist(rev(con), recursive = FALSE)
    # price update weights for all periods after the first
    if (pias$height && chained) {
      pias$weights <- price_update(index[eas], w[[1]]) 
    }
  }
  x$levels <- pias$levels
  x$r <- r
  x$chained <- chained
  x$pias <- pias[c("child", "parent", "eas", "height")]
  structure(x, class = c("aggregate", "index"))
}
