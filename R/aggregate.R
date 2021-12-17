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
    gen_mean(rel[[i - 1L]][z], w[[i - 1L]][z], na.rm = na.rm)
  }
  aggregate_contrib <- if (x$has_contrib) {
    function(z) {
      unlist(Map("*", con[[i - 1L]][z], scale_weights(aw(rel[[i - 1L]][z], w[[i - 1L]][z]))))
    }
  } else {
    function(z) numeric(0L)
  }
  # put the aggregation weights upside down to line up with pias
  w <- rev(weights(pias, na.rm = na.rm))
  # loop over each time period
  for (t in seq_along(x$time)) {
    rel <- con <- vector("list", pias$height)
    # align epr with weights so that positional indexing works
    # preserve names if epr and pias weights don't agree
    rel[[1L]] <- named_extract(x$index[[t]], pias$eas)
    con[[1L]] <- named_extract(x$contrib[[t]], pias$eas)
    # get rid of any NULL contributions
    con[[1L]][!lengths(con[[1L]])] <- list(numeric(0L))
    # re-aggregate price-updated weights for all periods after first
    if (t > 1L && x$chain) {
      w <- rev(weights(pias, na.rm = na.rm))
    }
    # loop over each level in the pias from the bottom up and aggregate
    for (i in seq_along(rel)[-1L]) {
      rel[[i]] <- vapply(pias$child[[i - 1L]], aggregate_index, numeric(1L))
      con[[i]] <- lapply(pias$child[[i - 1L]], aggregate_contrib)
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
    if (x$chain) {
      pias$weights <- price_update(index[pias$eas], w[[1L]]) 
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
    gen_mean(rel[[i - 1L]][z], w[[i - 1L]][z])
  }
  # initialize weights
  eas <- pias$eas
  w <- rev(weights(pias))
  # loop over each time period
  for (t in seq_along(x$time)) {
    rel <- vector("list", pias$height)
    # align epr with weights so that positional indexing works
    # preserve names if epr and pias weights don't agree
    rel[[1L]] <- named_extract(x$index[[t]], eas)
    if (t > 1L && chain) w <- rev(weights(pias))
    # loop over each level in the pias from the bottom up and aggregate
    for (i in seq_along(rel)[-1L]) {
      rel[[i]] <- vapply(pias$child[[i - 1L]], aggregate_index, numeric(1))
    }
    # return index and contributions
    index <- unlist(rev(rel))
    x$index[[t]] <- index
    # price update weights for all periods after the first
    if (chain) {
      pias$weights <- price_update(index[eas], w[[1L]]) 
    }
  }
  do.call(cbind, x$index)
}

vcov.agg_ind <- function(object, repweights, mse = TRUE, parallel = c("no", "mc", "snow"), ncpus = 1, cl = makeCluster(ncpus), ...) {
  repweights <- as.matrix(repweights)
  if (nrow(repweights) != length(object$pias$eas)) {
    stop(gettext("'repweights' must have a row for each weight in 'pias'"))
  }
  parallel <- match.arg(parallel)
  n <- ncol(repweights)
  eas <- object$pias$eas
  upper <- setdiff(object$levels, object$pias$eas)
  dimnm <- list(upper, object$time, seq_len(n))
  # initialize an aggregation structure with no weights and an array to store
  # indexes for looping over repweights
  pias <- aggregate2pias(object, numeric(length(eas)))
  # function to aggregate index for replicate 'i'
  repl <- function(i) {
    pias$weights[] <- repweights[, i]
    .aggregate(object, pias, object$chain, object$r)[upper, , drop = FALSE]
  }
  if (parallel == "no") {
    index_boot <- lapply(seq_len(n), repl)
  } else if (parallel == "mc") {
    index_boot <- mclapply(seq_len(n), repl, mc.cores = ncpus)
  } else if (parallel == "snow") {
    index_boot <- parLapply(cl, seq_len(n), repl)
    stopCluster(cl)
  }
  index_boot <- array(unlist(index_boot, use.names = FALSE), 
                      dim = lengths(dimnm), dimnames = dimnm)
  # mse = TRUE is the default for variance estimation in SAS, 
  # but not the survey package
  centre <- if (mse) {
    as.matrix(object)[upper, , drop = FALSE]
  } else {
    apply(index_boot, 2L, rowMeans)
  }
  res <- array(0, lengths(dimnm[c(1L, 1L, 2L)]), dimnames = dimnm[c(1L, 1L, 2L)])
  res[] <- apply(sweep(index_boot, 1:2, centre), 2L, tcrossprod) / n
  res
}

#---- Test ----
is_aggregate_index <- function(x) inherits(x, "agg_ind")