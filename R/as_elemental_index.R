as_elemental_index <- function(x, ...) {
  UseMethod("as_elemental_index")
}

as_elemental_index.default <- function(x, ...) {
  as_elemental_index(as.matrix(x), ...)
}

as_elemental_index.matrix <- function(x, ...) {
  x[] <- as.numeric(x)
  if (is.null(rownames(x))) {
    rownames(x) <- seq_len(nrow(x))
  }
  if (is.null(colnames(x))) {
    colnames(x) <- seq_len(ncol(x))
  }
  levels <- as.character(rownames(x)) # as.character is for matrices without rows
  periods <- as.character(colnames(x)) # same for columns
  res <- structure(vector("list", 4), names = c("index", "contributions", "levels", "periods"))
  res$index <- res$contributions <- structure(vector("list", ncol(x)), names = periods)
  contrib <- structure(rep(list(numeric(0)), length(levels)), names = levels)
  for (i in seq_along(periods)) {
    res$index[[i]] <- x[, i]
    res$contributions[[i]] <- contrib
  }
  res$levels <- levels
  res$periods <- periods
  structure(res, class = c("elemental", "index"))
}
