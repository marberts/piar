new_aggregate_index <- function(index,
                                contrib,
                                levels,
                                time,
                                r,
                                pias,
                                chainable) {
  res <- list(index = as.list(index),
              contrib = as.list(contrib),
              levels = as.character(levels),
              time = as.character(time),
              r = as.numeric(r),
              pias = as.list(pias))
  type <- if (chainable) "chainable_index" else "direct_index"
  structure(res, class = c("aggregate_index", type, "index"))
}

aggregate.index <- function(x, pias, na.rm = FALSE, r = 1, ...) {
  pias <- as_aggregation_structure(pias)
  r <- as.numeric(r)

  # helpful functions
  price_update <- factor_weights(r)
  gen_mean <- generalized_mean(r)
  aw <- transmute_weights(r, 1)

  # put the aggregation weights upside down to line up with pias
  w <- rev(weights(pias, na.rm = na.rm))
  has_contrib <- has_contrib(x)
  chainable <- is_chainable_index(x)
  # loop over each time period
  for (t in seq_along(x$time)) {
    rel <- con <- vector("list", pias$height)
    # align epr with weights so that positional indexing works
    # preserve names if epr and pias weights don't agree
    rel[[1L]] <- x$index[[t]][pias$eas]
    con[[1L]] <- x$contrib[[t]][pias$eas]
    names(rel[[1L]]) <- names(con[[1L]]) <- pias$eas
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
    index <- unlist(rev(rel))
    x$index[[t]] <- index
    x$contrib[[t]] <- unlist(rev(con), recursive = FALSE)
    # price update weights for all periods after the first
    if (chainable) {
      weights(pias) <- price_update(index[pias$eas], w[[1L]])
      w <- rev(weights(pias, na.rm = na.rm))
    }
  }
  validate_index(
    new_aggregate_index(x$index, x$contrib, pias$levels, x$time, r,
                        pias[c("child", "parent", "eas", "height")],
                        is_chainable_index(x))
  )
}
