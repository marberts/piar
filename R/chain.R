#' Chain and rebase a price index
#'
#' @description
#' Chain a period-over-period index by taking the cumulative product of its
#' values to turn it into a fixed-base (direct) index.
#'
#' Unchain a fixed-base index by dividing its values for successive periods to
#' get a period-over-period index.
#'
#' Rebase a fixed-base index by dividing its values with the value of the index
#' in the new base period.
#'
#' @details
#' The default methods attempts to coerce `x` into an index with
#' [as_index()] prior to chaining/unchaining/rebasing.
#'
#' Chaining an index takes the cumulative product of the index values for each
#' level; this is roughly the same as
#' `t(apply(as.matrix(x), 1, cumprod)) * link`. Unchaining does the opposite,
#' so these are inverse operations. Note that unchaining a period-over-period
#' index (i.e., when `is_chainable_index(x) == TRUE`) does nothing, as does
#' chaining a fixed-base index (i.e., when `is_chainable_index(x) == FALSE`).
#'
#' Rebasing a fixed-base index divides the values for each level of this index
#' by the corresponding values for each level in the new base period. It's
#' roughly the same as `as.matrix(x) / base`. Like unchaining, rebasing a
#' period-over-period index does nothing.
#'
#' Percent-change contributions are removed when chaining/unchaining/rebasing
#' an index, as it's not usually possible to update them correctly.
#'
#' @param x A price index, as made by, e.g., [elemental_index()].
#' @param link A numeric vector, or something that can coerced into one, of
#' link values for each level in `x`. The default is a vector of 1s so
#' that no linking is done.
#' @param base A numeric vector, or something that can coerced into one, of
#' base-period index values for each level in `x`. The default is a vector
#' of 1s so that the base period remains the same.
#' @param ... Further arguments passed to or used by methods.
#'
#' @returns
#' `chain()` and `rebase()` return a fixed-base index that inherits
#' from [`direct_piar_index`].
#'
#' `unchain()` returns a period-over-period index that inherits from
#' [`chainable_piar_index`].
#'
#' @examples
#' prices <- data.frame(
#'   rel = 1:8,
#'   period = rep(1:2, each = 4),
#'   ea = rep(letters[1:2], 4)
#' )
#'
#' # A simple period-over-period elemental index
#'
#' (epr <- with(prices, elemental_index(rel, period, ea)))
#'
#' # Make period 0 the fixed base period
#'
#' chain(epr)
#'
#' # Chaining and unchaining reverse each other
#'
#' all.equal(epr, unchain(chain(epr)))
#'
#' # Change the base period to period 2 (note the
#' # loss of information for period 0)
#'
#' epr <- chain(epr)
#' rebase(epr, epr[, 2])
#'
#' @export chain
chain <- function(x, ...) {
  UseMethod("chain")
}

#' @rdname chain
#' @export
chain.default <- function(x, ...) {
  chain(as_index(x), ...)
}

#' @rdname chain
#' @export
chain.aggregate_piar_index <- function(x, ...) {
  res <- NextMethod("chain")
  new_aggregate_piar_index(
    res$index, res$contrib, res$levels,
    res$time, x$r, x$pias, is_chainable_index(res)
  )
}

#' @rdname chain
#' @export
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

#' @rdname chain
#' @export
chain.direct_piar_index <- function(x, ...) {
  x
}

#' @rdname chain
#' @export
unchain <- function(x, ...) {
  UseMethod("unchain")
}

#' @rdname chain
#' @export
unchain.default <- function(x, ...) {
  unchain(as_index(x, chainable = FALSE), ...)
}

#' @rdname chain
#' @export
unchain.aggregate_piar_index <- function(x, ...) {
  res <- NextMethod("unchain")
  new_aggregate_piar_index(
    res$index, res$contrib, res$levels,
    res$time, x$r, x$pias, is_chainable_index(res)
  )
}

#' @rdname chain
#' @export
unchain.chainable_piar_index <- function(x, ...) {
  x
}

#' @rdname chain
#' @export
unchain.direct_piar_index <- function(x, ...) {
  x$index[-1L] <- Map(`/`, x$index[-1L], x$index[-length(x$index)])
  # contributions are difficult to unchain, so remove them
  x$contrib[] <- empty_contrib(x$levels)
  new_piar_index(x$index, x$contrib, x$levels, x$time, chainable = TRUE)
}

#' @rdname chain
#' @export
rebase <- function(x, ...) {
  UseMethod("rebase")
}

#' @rdname chain
#' @export
rebase.default <- function(x, ...) {
  rebase(as_index(x, chainable = FALSE), ...)
}

#' @rdname chain
#' @export
rebase.chainable_piar_index <- function(x, ...) {
  x
}

#' @rdname chain
#' @export
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
