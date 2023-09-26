#---- Chaining ----
chain <- function(x, ...) {
  UseMethod("chain")
}

chain.default <- function(x, ...) {
  chain(as_index(x), ...)
}

chain.aggregate_piar_index <- function(x, ...) {
  res <- NextMethod("chain")
  new_aggregate_piar_index(res$index, res$contrib, res$levels,
                           res$time, x$r, x$pias, is_chainable_index(res))
}

chain.chainable_piar_index <- function(x, link = rep(1, nlevels(x)), ...) {
  link <- as.numeric(link)
  if (length(link) != length(x$levels)) {
    stop("'link' must have a value for each level of 'x'")
  }
  x$index[[1L]] <- x$index[[1L]] * link
  # x$index[] <- Reduce(`*`, x$index, accumulate = TRUE) simplifies results
  # with one level
  # TODO: use Reduce once my patch is in a released version of R
  for (t in seq_along(x$time)[-1L]) {
    x$index[[t]] <- x$index[[t]] * x$index[[t - 1L]]
  }
  # contributions are difficult to chain, so remove them
  x$contrib[] <- empty_contrib(x$levels)
  new_piar_index(x$index, x$contrib, x$levels, x$time, chainable = FALSE)
}

chain.direct_piar_index <- function(x, ...) {
  x
}

#---- Unchaining ----
unchain <- function(x, ...) {
  UseMethod("unchain")
}

unchain.default <- function(x, ...) {
  unchain(as_index(x, chainable = FALSE), ...)
}

unchain.aggregate_piar_index <- function(x, ...) {
  res <- NextMethod("unchain")
  new_aggregate_piar_index(res$index, res$contrib, res$levels,
                           res$time, x$r, x$pias, is_chainable_index(res))
}

unchain.chainable_piar_index <- function(x, ...) {
  x
}

unchain.direct_piar_index <- function(x, ...) {
  x$index[-1L] <- Map(`/`, x$index[-1L], x$index[-length(x$index)])
  # contributions are difficult to unchain, so remove them
  x$contrib[] <- empty_contrib(x$levels)
  new_piar_index(x$index, x$contrib, x$levels, x$time, chainable = TRUE)
}

#---- Rebase ----
rebase <- function(x, ...) {
  UseMethod("rebase")
}

rebase.default <- function(x, ...) {
  rebase(as_index(x, chainable = FALSE), ...)
}

rebase.chainable_piar_index <- function(x, ...) {
  x
}

rebase.direct_piar_index <- function(x, base = rep(1, nlevels(x)), ...) {
  base <- as.numeric(base)
  if (length(base) != length(x$levels)) {
    stop("'base' must have a value for each level of 'x'")
  }
  x$index <- Map(`/`, x$index, list(base))
  # contributions are difficult to rebase, so remove them
  x$contrib[] <- empty_contrib(x$levels)
  x
}
