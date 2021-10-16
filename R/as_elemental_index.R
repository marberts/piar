as_elemental_index <- function(x, ...) {
  UseMethod("as_elemental_index")
}

as_elemental_index.default <- function(x, ...) {
  as_elemental_index(as.matrix(x), ...)
}

as_elemental_index.matrix <- function(x, ...) {
  storage.mode(x) <- "numeric"
  if (is.null(rownames(x))) {
    rownames(x) <- seq_len(nrow(x))
  }
  if (is.null(colnames(x))) {
    colnames(x) <- seq_len(ncol(x))
  }
  levels <- as.character(rownames(x)) # as.character is for matrices without rows
  periods <- as.character(colnames(x)) # same for columns
  res <- structure(vector("list", 3), names = c("index", "levels", "periods"))
  res$index <- structure(vector("list", ncol(x)), names = periods)
  for (i in seq_along(periods)) res$index[[i]] <- x[, i]
  res$levels <- levels
  res$periods <- periods
  structure(res, class = c("elem_index", "index"))
}
