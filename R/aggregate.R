new_aggregate_index <- function(index,
                                contrib,
                                levels,
                                time,
                                r,
                                pias,
                                chainable) {
  res <- list(index = index,
              contrib = contrib,
              levels = levels,
              time = time,
              r = r,
              pias = pias)
  type <- if (chainable) {
    "chainable_index"
  } else {
    "direct_index"
  }
  structure(res, class = c("aggregate_index", type, "index"))
}

#---- Aggregate an index ----
aggregate.index <- function(x, pias, na.rm = FALSE, r = 1, ...) {
  if (!is_aggregation_structure(pias)) {
    stop("'pias' must be a price index aggregation structure; use ",
         "aggregation_structure() to make one")
  }
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
    rel[[1L]] <- named_extract(x$index[[t]], pias$eas)
    con[[1L]] <- named_extract(x$contrib[[t]], pias$eas)
    # get rid of any NULL contributions
    con[[1L]][lengths(con[[1L]]) == 0L] <- list(numeric(0L))
    # re-aggregate price-updated weights for all periods after first
    if (t > 1L && chainable) {
      w <- rev(weights(pias, na.rm = na.rm))
    }
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
    }
  }
  new_aggregate_index(x$index, x$contrib, pias$levels, x$time, r,
                      pias[c("child", "parent", "eas", "height")],
                      is_chainable_index(x))
}

#---- Averaging over subperiods ----
mean.index <- function(x, w = NULL, window = 3, na.rm = FALSE, r = 1, ...) {
  if (!is.null(w)) {
    if (length(w) != length(x$time) * length(x$levels)) {
      stop("'x' and 'w' must be the same length")
    }
    w <- split(w, gl(length(x$time), length(x$levels)))
  }
  gen_mean <- Vectorize(generalized_mean(r))
  len <- length(x$time) %/% window
  if (len == 0L) {
    stop("'x' must have at least 'window' time periods")
  }
  # get the starting location for each window
  loc <- seq(1L, by = window, length.out = len)
  periods <- x$time[loc]
  res <- contrib <- structure(vector("list", len), names = periods)
  # loop over each window and calculate the mean for each level
  for (i in seq_along(loc)) {
    j <- seq(loc[i], length.out = window)
    # structure() is needed because .mapply doesn't keep names
    index <- structure(.mapply(c, x$index[j], list()), names = x$levels)
    res[[i]] <- if (is.null(w)) {
        gen_mean(index, na.rm = na.rm)
      } else {
        gen_mean(index, .mapply(c, w[j], list()), na.rm = na.rm)
      }
  }
  contrib[] <- empty_contrib(x$levels)
  x$index <- res
  x$contrib <- contrib
  x$time <- periods
  x
}
