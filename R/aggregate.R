#---- Aggregate an index ----
aggregate.ind <- function(x, pias, na.rm = FALSE, r = 1, ...) {
  if (!is_pias(pias)) {
    stop(gettext("'pias' must be a price index aggregation structure; use aggregation_structure() to make one"))
  }
  # helpful functions
  price_update <- factor_weights(r)
  gen_mean <- generalized_mean(r)
  aw <- transmute_weights(r, 1)
  # functions to aggregate index values and contributions
  # 'i' is defined in the loop below; it's used to loop over the height of 'pias'
  aggregate_index <- function(z) {
    gen_mean(rel[[i - 1]][z], w[[i - 1]][z], na.rm = na.rm)
  }
  aggregate_contrib <- if (x$has_contrib) {
    function(z) {
      unlist(Map("*", con[[i - 1]][z], scale_weights(aw(rel[[i - 1]][z], w[[i - 1]][z]))))
    }
  } else {
    function(z) numeric(0)
  }
  # initialize weights
  w <- rev(weights(pias, na.rm = na.rm))
  # loop over each time period
  for (t in seq_along(x$time)) {
    rel <- con <- vector("list", pias$height)
    # align epr with weights so that positional indexing works
    # preserve names if epr and pias weights don't agree
    rel[[1]] <- named_extract(x$index[[t]], pias$eas)
    con[[1]] <- named_extract(x$contrib[[t]], pias$eas)
    # get rid of any NULL contributions
    con[[1]][!lengths(con[[1]])] <- list(numeric(0))
    # re-aggregate price-updated weights for all periods after first
    if (t > 1 && x$chain) {
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
    if (pias$height && x$chain) {
      pias$weights <- price_update(index[pias$eas], w[[1]]) 
    }
  }
  x$levels <- pias$levels
  x$r <- r
  x$pias <- pias[c("child", "parent", "eas", "height")]
  structure(x, class = c("agg_ind", "ind"))
}

#---- Covariance calculation ----
# Fast aggregate
# This can be removed if aggregate.ind gets fast enough
.aggregate <- function(x, pias, chain, r) {
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
  for (t in seq_along(x$time)) {
    rel <- vector("list", pias$height)
    # align epr with weights so that positional indexing works
    # preserve names if epr and pias weights don't agree
    rel[[1]] <- named_extract(x$index[[t]], eas)
    if (t > 1 && chain) w <- rev(weights(pias))
    # loop over each level in the pias from the bottom up and aggregate
    for (i in seq_along(rel)[-1]) {
      rel[[i]] <- vapply(pias$child[[i - 1]], aggregate_index, numeric(1))
    }
    # return index and contributions
    index <- unlist(rev(rel))
    x$index[[t]] <- index
    # price update weights for all periods after the first
    if (pias$height && chain) {
      pias$weights <- price_update(index[eas], w[[1]]) 
    }
  }
  list2matrix(x$index)
}

vcov.agg_ind <- function(object, repweights, mse = TRUE, ...) {
  repweights <- as.matrix(repweights)
  if (nrow(repweights) != length(object$pias$eas)) {
    stop(gettext("'repweights' must have a row for each weight in 'pias'"))
  }
  # it is possible to have levels but no periods, so return an empty array
  if (!length(object$time)) {
    return(array(numeric(0), c(0, 0, 0), dimnames = list(NULL, NULL, NULL)))
  }
  n <- ncol(repweights)
  eas <- object$pias$eas
  upper <- setdiff(object$levels, object$pias$eas)
  dimnm <- list(upper, object$time, seq_len(n))
  # initialize an aggregation structure with no weights and an array to store
  # indexes for looping over repweights
  pias <- aggregate2pias(object, numeric(length(eas)))
  index_boot <- array(0, dim = lengths(dimnm), dimnames = dimnm)
  for (i in seq_len(n)) {
    pias$weights[] <- repweights[, i]
    index_boot[, , i] <- .aggregate(object, pias, object$chain, object$r)[upper, , drop = FALSE]
  }
  # mse = TRUE is the default for variance estimation in SAS, 
  # but not the survey package
  centre <- if (mse) {
    as.matrix(object)[upper, , drop = FALSE]
  } else {
    apply(index_boot, 2, rowMeans)
  }
  res <- array(0, lengths(dimnm[c(1, 1, 2)]), dimnames = dimnm[c(1, 1, 2)])
  res[] <- apply(sweep(index_boot, 1:2, centre), 2, tcrossprod) / n
  res
}

#---- Extract contributions ----
contrib <- function(x, ...) {
  UseMethod("contrib")
}

contrib.ind <- function(x, level = levels(x), ...) {
  res <- lapply(x$contrib, `[[`, match.arg(level))
  products <- unique(nested_names(res))
  res <- lapply(res, named_extract, products)
  list2matrix(res)
}