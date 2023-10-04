dim_indices <- function(x, i) {
  names(x) <- x
  res <- match(x[i], x, incomparables = NA)
  if (anyNA(res) || length(res) == 0L) {
    stop("subscript out of bounds")
  }
  res
}

`[.piar_index` <- function(x, i, j, ...) {
  levels <- dim_indices(x$levels, i)
  periods <- dim_indices(x$time, j)
  x$index <- lapply(x$index[periods], `[`, levels)
  x$contrib <- lapply(x$contrib[periods], `[`, levels)
  x$levels <- x$levels[levels]
  x$time <- x$time[periods]
  validate_piar_index(x)
}

`[.aggregate_piar_index` <- function(x, i, j, ...) {
  res <- NextMethod("[")
  if (!identical(res$levels, x$levels)) {
    new_piar_index(res$index, res$contrib, res$levels, res$time,
                   is_chainable_index(res))
  } else {
    res
  }
}

`[<-.piar_index` <- function(x, i, j, ..., value) {
  levels <- dim_indices(x$levels, i)
  periods <- dim_indices(x$time, j)
  res <- as.matrix(x)
  res[levels, periods] <- as.numeric(value)
  # only loop over periods that have a value replaced
  for (t in periods) {
    x$index[[t]][levels] <- res[levels, t]
    # drop contributions for replaced values
    x$contrib[[t]][levels] <- list(numeric(0L))
  }
  validate_piar_index(x)
}

`[<-.aggregate_piar_index` <- function(x, i, j, ..., value) {
  x <- new_piar_index(x$index, x$contrib, x$levels, x$time,
                      is_chainable_index(x))
  NextMethod("[<-")
}
