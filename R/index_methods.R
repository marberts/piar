as.data.frame.index <- function(x, row.names = NULL, ..., type = c("index", "contributions")) {
  type <- match.arg(type)
  res <- lapply(x[[type]], unlist)
  # as.*() ensures that level and value columns show up with NULL values
  value <- as.numeric(unlist(res, use.names = FALSE))
  level <- nested_names(res)
  period <- rep(x$periods, lengths(res))
  data.frame(period, level, value, row.names = row.names, stringsAsFactors = FALSE)
}

as.matrix.index <- function(x, type = c("index", "contributions"), ...) {
  type <- match.arg(type)
  x <- if (type == "index") {
    x$index
  } else {
    contrib <- lapply(x$contributions, unlist)
    nm <- unique(nested_names(contrib))
    lapply(contrib, named_extract, nm)
  }
  if (any(lengths(x))) { # rbind returns NULL for empty lists
    do.call(cbind, x)
  } else {
    matrix(numeric(0), ncol = 0, nrow = 0, dimnames = list(character(0), character(0)))
  }
}

`[.index` <- function(x, i, j, drop = TRUE) {
  as.matrix(x)[i, j, drop = drop]
}

`[<-.index` <- function(x, i, j, value) {
  x$index[[j]][[i]][] <- value
}

cumprod.index <- function(x) {
  x$index[] <- Reduce("*", x$index, accumulate = TRUE)
  as.matrix(x)
}

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

print.index <- function(x, ...) {
  print(as.matrix(x), ...)
  invisible(x)
}
