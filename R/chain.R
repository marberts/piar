#---- Chaining ----
chain <- function(x, ...) {
  UseMethod("chain")
}

chain.default <- function(x, ...) {
  chain(as_index(x), ...)
}

chain.chainable_pindex <- function(x, link = rep(1, nlevels(x)), ...) {
  link <- as.numeric(link)
  if (length(link) != length(x$levels)) {
    stop("'link' must have a value for each level of 'x'")
  }
  x$index[[1L]] <- x$index[[1L]] * link
  x$index[] <- Reduce(`*`, x$index, accumulate = TRUE)
  # contributions are difficult to chain, so remove them
  x$contrib[] <- empty_contrib(x$levels)
  class(x)[class(x) == "chainable_pindex"] <- "direct_pindex"
  x
}

chain.direct_pindex <- function(x, ...) {
  x
}

#---- Unchaining ----
unchain <- function(x, ...) {
  UseMethod("unchain")
}

unchain.default <- function(x, ...) {
  unchain(as_index(x, chainable = FALSE), ...)
}

unchain.chainable_pindex <- function(x, ...) {
  x
}

unchain.direct_pindex <- function(x, ...) {
  x$index[-1L] <- Map(`/`, x$index[-1L], x$index[-length(x$index)])
  # contributions are difficult to unchain, so remove them
  x$contrib[] <- empty_contrib(x$levels)
  class(x)[class(x) == "direct_pindex"] <- "chainable_pindex"
  x
}

#---- Rebase ----
rebase <- function(x, ...) {
  UseMethod("rebase")
}

rebase.default <- function(x, ...) {
  rebase(as_index(x, chainable = FALSE), ...)
}

rebase.chainable_pindex <- function(x, ...) {
  x
}

rebase.direct_pindex <- function(x, base = rep(1, nlevels(x)), ...) {
  base <- as.numeric(base)
  if (length(base) != length(x$levels)) {
    stop("'base' must have a value for each level of 'x'")
  }
  x$index[] <- Map(`/`, x$index, list(base))
  # contributions are difficult to rebase, so remove them
  x$contrib[] <- empty_contrib(x$levels)
  x
}
