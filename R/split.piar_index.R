#' Split an index into groups
#'
#' Split an index into groups of indexes according to a factor, along either
#' the levels or time periods of the index.
#'
#' @param x A price index, as made by, e.g., [elementary_index()].
#' @param f A factor or list of factors to group elements of `x`.
#' @param drop Should levels that do not occur in `f` be dropped? By default
#'   all levels are kept.
#' @param along Either `"levels"` to split over the levels of `x` (the default),
#'   or `"time"` to split over the time periods of `x`.
#' @param value A list of values compatible with the splitting of `x`, or
#'   something that can be coerced into one, recycled if necessary.
#' @param ... Further arguments passed to [`split.default()`].
#'
#' @returns
#' `split()` returns a list of index objects for each level in `f`. The
#' replacement method replaces these values with the corresponding element of
#' `value`.
#'
#' @examples
#' index <- as_index(matrix(1:6, 2))
#'
#' split(index, 1:2)
#'
#' split(index, c(1, 1, 2), along = "time")
#'
#' @family index methods
#' @export
split.piar_index <- function(
  x,
  f,
  drop = FALSE,
  ...,
  along = c("levels", "time")
) {
  along <- match.arg(along)
  ix <- split(seq_along(x[[along]]), f = f, drop = drop, ...)
  if (along == "levels") {
    lapply(ix, \(i) x[i, ])
  } else {
    lapply(ix, \(i) x[, i])
  }
}

#' @rdname split.piar_index
#' @export
`split<-.piar_index` <- function(
  x,
  f,
  drop = FALSE,
  ...,
  along = c("levels", "time"),
  value
) {
  value <- as.list(value)
  along <- match.arg(along)
  ix <- split(seq_along(x[[along]]), f = f, drop = drop, ...)
  n <- length(value)
  if (n > 0L && length(ix) %% n != 0) {
    warning(
      "number of items to replace is not a multiple of replacement length"
    )
  }
  j <- 0
  if (along == "levels") {
    for (i in ix) {
      j <- j %% n + 1
      x[i, ] <- value[[j]]
    }
  } else {
    for (i in ix) {
      j <- j %% n + 1
      x[, i] <- value[[j]]
    }
  }
  x
}
