as.data.frame.index <- function(x, row.names = NULL, ..., type = c("index", "contributions")) {
  type <- match.arg(type)
  res <- lapply(x[[type]], unlist)
  # as.*() ensures that level and value columns show up with NULL values
  value <- as.numeric(unlist(res, use.names = FALSE))
  level <- as.character(unlist(lapply(res, names), use.names = FALSE))
  period <- rep(x$periods, lengths(res))
  data.frame(period, level, value, row.names = row.names, stringsAsFactors = FALSE)
}

as.matrix.index <- function(x, type = c("index", "contributions"), ...) {
  type <- match.arg(type)
  x <- if (type == "index") {
    x$index
  } else {
    contrib <- lapply(x$contributions, unlist)
    nm <- unlist(lapply(contrib, names), use.names = FALSE)
    lapply(contrib, named_extract, unique(nm))
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

cumprod.index <- function(x) {
  x$index[] <- Reduce("*", x$index, accumulate = TRUE)
  as.matrix(x)
}

merge.index <- function(x, y, ...) {
  if (!inherits(y, "index")) {
    stop("'y' is not an index; use elemental_index() to make one")
  }
  if (any(x$periods != y$periods)) {
    stop("'x' and 'y' must be indexes for the same time periods")
  }
  if (length(intersect(x$levels, y$levels))) {
    warning("some levels appear in both 'x' and 'y'")
  }
  if (!identical(class(x), class(y))) {
    warning("'x' is a ", paste(class(x), collapse = " "), " but 'y' is a ", paste(class(y), collapse = " "))
  }
  for (t in x$periods) {
    x$index[[t]] <- c(x$index[[t]], y$index[[t]])
    x$contributions[[t]] <- c(x$contributions[[t]], y$contributions[[t]])
  }
  x$levels <- union(x$levels, y$levels)
  x
}

stack.index <- function(x, y, ...) {
  if (!inherits(y, "index")) {
    stop("'y' is not an index; use elemental_index() to make one")
  }
  if (any(x$levels != y$levels)) {
    stop("'x' and 'y' must be indexes for the same levels")
  }
  if (length(intersect(x$periods, y$periods))) {
    warning("some periods appear in both 'x' and 'y'")
  }
  if (!identical(class(x), class(y))) {
    warning("'x' is a ", paste(class(x), collapse = " "), " but 'y' is a ", paste(class(y), collapse = " "))
  }
  x$index <- c(x$index, y$index)
  x$contributions <- c(x$contributions, y$contributions)
  x$periods <- union(x$periods, y$periods)
  x
}

print.index <- function(x, ...) {
  print(as.matrix(x), ...)
  invisible(x)
}
