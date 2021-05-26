aggregate.aggregate <- function(x, pias, na.rm = FALSE, r = 1, ...) {
  warning("aggregating an aggregated index")
  NextMethod()
}

aggregate.index <- function(x, pias, na.rm = FALSE, r = 1, ...) {
  if (!inherits(pias, "pias")) {
    stop("'pias' must be a price index aggregation structure; use pias() to make one")
  }
  # helpful functions
  price_update <- weights_factor(r)
  # 'i' is defined in the loop below; it's used to loop over the height of 'pias'
  aggregate_index <- function(z) {
    mean_generalized(r)(rel[[i - 1]][z], w[[i - 1]][z], na.rm = na.rm)
  }
  aggregate_contrib <- function(z) {
    w <- weights_scale(weights_transmute(r, 1)(rel[[i - 1]][z], w[[i - 1]][z]))
    unlist(Map("*", con[[i - 1]][z], w))
  }
  # loop over each time period
  eas <- names(pias$weights)
  for (t in seq_along(x$periods)) {
    rel <- con <- vector("list", pias$height)
    # align epr with weights so that positional indexing works
    # preserve names if epr and pias weights don't agree
    rel[[1]] <- named_extract(x$index[[t]], eas)
    con[[1]] <- named_extract(x$contributions[[t]], eas)
    # get rid of any NULL contributions
    con[[1]][!lengths(con[[1]])] <- list(numeric(0))
    w <- weights(pias, na.rm = na.rm)
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
    if (pias$height) pias$weights <- price_update(index[eas], w[[1]])
  }
  x$levels <- pias$levels
  structure(x, class = c("aggregate", "index"))
}
