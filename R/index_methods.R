#---- Coercion ----
as.data.frame.index <- function(x, row.names = NULL, ...) {
  index <- x$index
  # as.numeric() ensures that the value column shows up with NULL values
  value <- as.numeric(unlist(index, use.names = FALSE))
  level <- nested_names(index)
  period <- rep(x$periods, lengths(index))
  data.frame(period, level, value, row.names = row.names, stringsAsFactors = FALSE)
}

as.matrix.index <- function(x, ...) {
  list2matrix(x$index)
}

as.data.frame.contrib <- function(x, row.names = NULL, ...) {
  contrib <- lapply(x$contributions, unlist)
  # as.numeric() ensures that the value column shows up with NULL values
  value <- as.numeric(unlist(contrib, use.names = FALSE))
  level <- nested_names(contrib)
  period <- rep(x$periods, lengths(contrib))
  data.frame(period, level, value, row.names = row.names, stringsAsFactors = FALSE)
}

as.matrix.contrib <- function(x, ...) {
  contrib <- lapply(x$contributions, unlist)
  nm <- unique(nested_names(contrib))
  list2matrix(lapply(contrib, named_extract, nm))
}

#---- Extraction ----
`[.index` <- function(x, i, j, drop = TRUE) {
  as.matrix(x)[i, j, drop = drop]
}

levels.index <- function(x) {
  x$levels
}

time.index <- function(x, ...) {
  x$periods
}

start.index <- function(x, ...) {
  x$periods[min(length(x$period), 1L)]
}

end.index <- function(x, ...) {
  x$periods[length(x$periods)]
}

levels.contrib <- function(x) {
  x$contrib
}

time.contrib <- function(x, ...) {
  x$contrib
}

start.contrib <- function(x, ...) {
  x$periods[min(length(x$period), 1L)]
}

end.contrib <- function(x, ...) {
  x$periods[length(x$periods)]
}

weights.agg_index <- function(object, ...) {
  list2matrix(object$weights)
}

#---- Printing ----
print.index <- function(x, ...) {
  print(as.matrix(x), ...)
  invisible(x)
}

print.contrib <- function(x, ...) {
  print(as.matrix(x), ...)
  invisible(x)
}

#---- Math ----
cumprod.elem_index <- function(x) {
  x$index[] <- Reduce("*", x$index, accumulate = TRUE)
  x
}

cumprod.agg_index <- function(x) {
  if (!x$chained) warning(gettext("'x' is not a chained index"))
  NextMethod()
}

#---- Merging/stacking ----

merge.index <- function(x, y, ...) {
  if (!inherits(y, "index")) {
    stop(gettext("'y' is not an index; use elemental_index() to make one"))
  }
  if (!identical(class(x), class(y))) {
    stop(gettext("'x' and 'y' must be the same type of index"))
  }
  if (any(x$periods != y$periods)) {
    stop(gettext("'x' and 'y' must be indexes for the same time periods"))
  }
  if (length(intersect(x$levels, y$levels))) {
    warning(gettext("some levels appear in both 'x' and 'y'"))
  }
  if (!length(x$levels)) return(y)
  if (!length(y$levels)) return(x)
  for (t in x$periods) {
    x$index[[t]] <- c(x$index[[t]], y$index[[t]])
    x$contributions[[t]] <- c(x$contributions[[t]], y$contributions[[t]])
    x$weights[[t]] <- c(x$weights[[t]], y$weights[[t]])
  }
  x$levels <- union(x$levels, y$levels)
  x
}

stack.index <- function(x, y, ...) {
  if (!inherits(y, "index")) {
    stop(gettext("'y' is not an index; use elemental_index() to make one"))
  }
  if (!identical(class(x), class(y))) {
    stop(gettext("'x' and 'y' must be the same type of index"))
  }
  if (any(x$levels != y$levels)) {
    stop(gettext("'x' and 'y' must be indexes for the same levels"))
  }
  if (length(intersect(x$periods, y$periods))) {
    warning(gettext("some periods appear in both 'x' and 'y'"))
  }
  if (!length(x$periods)) return(y)
  if (!length(y$periods)) return(x)
  x$index <- c(x$index, y$index)
  x$contributions <- c(x$contributions, y$contributions)
  x$periods <- union(x$periods, y$periods)
  x$weights <- c(x$weights, y$weights)
  x
}

unstack.index <- function(x, ...) {
  if (!length(x$periods)) return(x)
  res <- vector("list", length(x$periods))
  for (i in seq_along(res)) {
    res[[i]]$index <- x$index[i]
    res[[i]]$contributions <- x$contributions[i]
    res[[i]]$levels <- x$levels
    res[[i]]$periods <- x$periods[i]
    res[[i]]$weights <- x$weights[i]
    class(res[[i]]) <- class(x)
  }
  res
}