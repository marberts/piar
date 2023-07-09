#---- Coerce to an index ----
as_index <- function(x, ...) {
  UseMethod("as_index")
}

as_index.default <- function(x, ...) {
  as_index(as.matrix(x), ...)
}

as_index.matrix <- function(x, chainable = TRUE, ...) {
  if (nrow(x) == 0L || ncol(x) == 0L) {
    stop("cannot make an index with no periods or elemental aggregates")
  }
  storage.mode(x) <- "numeric"
  if (is.null(rownames(x))) {
    rownames(x) <- seq_len(nrow(x))
  }
  if (is.null(colnames(x))) {
    colnames(x) <- seq_len(ncol(x))
  }
  levels <- rownames(x)
  periods <- colnames(x)
  if (anyDuplicated(levels) || anyDuplicated(periods)) {
    stop("'x' cannot have duplicated row or column names")
  }

  index <- contrib <- structure(
    vector("list", ncol(x)), names = periods
  )
  contrib[] <- empty_contrib(levels)
  for (t in seq_along(periods)) {
    # EA names are not kept for matrices with 1 row
    index[[t]] <- structure(x[, t], names = rownames(x))
  }
  pindex(index, contrib, levels, periods, as_TorF(chainable))
}

as_index.data.frame <- function(x, cols = 1:3, chainable = TRUE, ...) {
  x <- x[cols]
  x[1:2] <- lapply(x[1:2], as.character)
  time <- unique(x[[1L]])
  levels <- unique(x[[2L]])
  # elemental_index() usually gives NaN for missing cells
  res <- matrix(NA_real_, nrow = length(levels), ncol = length(time),
                dimnames = list(levels, time))
  res[as.matrix(x[2:1])] <- as.numeric(x[[3L]])
  as_index(res, chainable, ...)
}

as_index.pindex <- function(x, ...) {
  x
}

#---- Test ----
is_index <- function(x) {
  inherits(x, "pindex")
}

is_aggregate_index <- function(x) {
  inherits(x, "agg_pindex")
}

is_chainable_index <- function(x) {
  is_index(x) && x$chainable
}
