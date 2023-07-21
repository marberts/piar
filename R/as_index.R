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
    levels <- as.character(seq_len(nrow(x)))
  } else {
    levels <- as.character(rownames(x))
  }
  if (is.null(colnames(x))) {
    periods <- as.character(seq_len(ncol(x)))
  } else {
    periods <- as.character(colnames(x))
  }
  
  if (anyNA(levels) || anyNA(periods)) {
    stop("cannot make an index with missing levels/time periods")
  }
  if (anyDuplicated(levels) || anyDuplicated(periods)) {
    stop("'x' cannot have duplicated row or column names")
  }

  index <- contrib <- structure(vector("list", ncol(x)), names = periods)
  contrib[] <- empty_contrib(levels)
  for (t in seq_along(periods)) {
    # EA names are not kept for matrices with 1 row
    index[[t]] <- structure(x[, t], names = levels)
  }
  new_index(index, contrib, levels, periods, chainable)
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

as_index.index <- function(x, ...) {
  x
}

#---- Test ----
is_index <- function(x) {
  inherits(x, "index")
}

is_aggregate_index <- function(x) {
  inherits(x, "aggregate_index")
}

is_chainable_index <- function(x) {
  inherits(x, "chainable_index")
}

is_direct_index <- function(x) {
  inherits(x, "direct_index")
}
