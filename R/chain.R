#---- Chaining ----
chain <- function(x, ...) {
  UseMethod("chain")
}

chain.default <- function(x, ...) {
  x <- as_elemental_index(x)
  NextMethod()
}

chain.index <- function(x, ...) {
  if (x$chained) {
    x$chained <- FALSE
    x$index[] <- Reduce(`*`, x$index, accumulate = TRUE)
    # contributions are difficult to chain, so remove them
    x$has_contrib <- FALSE
    x$contrib[] <- empty_contrib(x$levels)
  }
  # do nothing for a fixed-based index
  x
}

#---- Unchaining ----
unchain <- function(x, ...) {
  UseMethod("unchain")
}

unchain.default <- function(x, ...) {
  x <- as_elemental_index(x)
  NextMethod()
}

unchain.index <- function(x, ...) {
  if (!x$chained) {
    x$chained <- TRUE
    x$index[-1] <- Map(`/`, x$index[-1], x$index[-length(x$index)])
    # contributions are difficult to unchain, so remove them
    x$has_contrib <- FALSE
    x$contrib[] <- empty_contrib(x$levels)
  }
  # do nothing for a chain index
  x
}