#---- Chaining ----
chain <- function(x, ...) {
  UseMethod("chain")
}

chain.default <- function(x, ...) {
  chain(as_index(x), ...)
}

chain.ind <- function(x, link = rep(1, length(levels(x))), ...) {
  if (x$chain) {
    link <- as.numeric(link)
    if (length(link) != length(x$levels)) {
      stop(gettext("'link' must have a value for each level of 'x'"))
    }
    x$chain <- FALSE
    x$index[[1]] <- x$index[[1]] * link
    x$index[] <- Reduce(`*`, x$index, accumulate = TRUE)
    # contributions are difficult to chain, so remove them
    x$has_contrib <- FALSE
    x$contrib[] <- empty_contrib(x$levels)
  }
  # do nothing for a fixed-based index
  x
}

cumprod.ind <- function(x) {
  warnings(gettext("'cumprod()' is deprecated for index objects; use 'chain()' instead"))
  as.matrix(chain.ind(x))
}

#---- Unchaining ----
unchain <- function(x, ...) {
  UseMethod("unchain")
}

unchain.default <- function(x, ...) {
  unchain(as_index(x, chain = FALSE), ...)
}

unchain.ind <- function(x, ...) {
  if (!x$chain) {
    x$chain <- TRUE
    x$index[-1L] <- Map(`/`, x$index[-1L], x$index[-length(x$index)])
    # contributions are difficult to unchain, so remove them
    x$has_contrib <- FALSE
    x$contrib[] <- empty_contrib(x$levels)
  }
  # do nothing for a chained index
  x
}

#---- Rebase ----
rebase <- function(x, ...) {
  UseMethod("rebase")
}

rebase.default <- function(x, ...) {
  rebase(as_index(x, chain = FALSE), ...)
}

rebase.ind <- function(x, base = rep(1, length(levels(x))), ...) {
  if (!x$chain) {
    base <- as.numeric(base)
    if (length(base) != length(x$levels)) {
      stop(gettext("'base' must have a value for each level of 'x'"))
    }
    x$index <- Map(`/`, x$index, list(base))
    # contributions are difficult to rebase, so remove them
    x$has_contrib <- FALSE
    x$contrib[] <- empty_contrib(x$levels)
  }
  # do nothing for a period-over-period index
  x
}

#---- Test ----
is_chain_index <- function(x) {
  is_index(x) && x$chain
}