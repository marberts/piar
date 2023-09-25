#---- Coerce to an index ----
as_index <- function(x, ...) {
  UseMethod("as_index")
}

as_index.default <- function(x, chainable = TRUE, ...) {
  as_index(as.matrix(x), chainable, ...)
}

as_index.matrix <- function(x, chainable = TRUE, ...) {
  storage.mode(x) <- "numeric"
  levels <- as.character(
    if (is.null(rownames(x))) seq_len(nrow(x)) else rownames(x)
  )
  periods <- as.character(
    if (is.null(colnames(x))) seq_len(ncol(x)) else colnames(x)
  )
  if (any(x <= 0, na.rm = TRUE)) {
    warning("some elements of 'x' are less than or equal to 0")
  }

  index <- index_skeleton(levels, periods)
  for (t in seq_along(periods)) {
    index[[t]][] <- x[, t]
  }
  contrib <- contrib_skeleton(levels, periods)
  validate_index(new_index(index, contrib, levels, periods, chainable))
}

as_index.data.frame <- function(x, cols = 1:3, chainable = TRUE, ...) {
  x <- x[cols]
  x[1:2] <- lapply(x[1:2], as.factor)
  time <- levels(x[[1L]])
  levels <- levels(x[[2L]])
  # elemental_index() usually gives NaN for missing cells
  res <- matrix(NA_real_, nrow = length(levels), ncol = length(time),
                dimnames = list(levels, time))
  res[as.matrix(x[2:1])] <- as.numeric(x[[3L]])
  as_index(res, chainable, ...)
}

as_index.chainable_index <- function(x, chainable = TRUE, ...) {
  if (chainable) {
    x
  } else {
    chain(x)
  }
}

as_index.direct_index <- function(x, chainable = FALSE, ...) {
  if (chainable) {
    unchain(x)
  } else {
    x
  }
}

#---- Test ----
is_index <- function(x) {
  inherits(x, "abstract_index")
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
