#---- Chaining ----
chain <- function(x, ...) {
  UseMethod("chain")
}

chain.default <- function(x, ...) {
  chain(as_elemental_index(x), ...)
}

chain.ind <- function(x, ...) {
  if (x$chain) {
    x$chain <- FALSE
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
  chain.ind(x)
}

#---- Unchaining ----
unchain <- function(x, ...) {
  UseMethod("unchain")
}

unchain.default <- function(x, ...) {
  unchain(as_elemental_index(x, chain = FALSE), ...)
}

unchain.ind <- function(x, ...) {
  if (!x$chain) {
    x$chain <- TRUE
    x$index[-1] <- Map(`/`, x$index[-1], x$index[-length(x$index)])
    # contributions are difficult to unchain, so remove them
    x$has_contrib <- FALSE
    x$contrib[] <- empty_contrib(x$levels)
  }
  # do nothing for a chain index
  x
}

#---- Test ----
is_chain_index <- function(x) {
  is_index(x) && x$chain
}