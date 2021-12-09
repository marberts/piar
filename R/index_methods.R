#---- Coerce ----
as.data.frame.index <- function(x, ...) {
  # as.numeric() ensures that the value column shows up with NULL values
  value <- as.numeric(unlist(x$index, use.names = FALSE))
  period <- rep(x$time, each = length(x$levels))
  data.frame(period, level = x$levels, value, ...)
}

as.matrix.index <- function(x, ...) {
  list2matrix(x$index)
}

#---- Extract ----
`[.index` <- function(x, i, j) {
  as.matrix(x)[i, j, drop = FALSE]
}

`[<-.index` <- function(x, i, j, value) {
  res <- as.matrix(x)
  res[i, j] <- as.numeric(value)
  periods <- colnames(res[0, j, drop = FALSE])
  # only loop over periods that have a value replaced
  for (t in periods) {
    x$index[[t]][i] <- res[i, t]
    # drop contributions for replaced values
    if (x$has_contrib) x$contrib[[t]][i] <- list(numeric(0))
  }
  x
}

levels.index <- function(x) {
  x$levels
}

time.index <- function(x, ...) {
  x$time
}

start.index <- function(x, ...) {
  x$time[min(length(x$time), 1L)]
}

end.index <- function(x, ...) {
  x$time[length(x$time)]
}

weights.aggregate <- function(object, ...) {
  list2matrix(object$weights)
}

#---- Merge ----
merge.aggregate <- function(x, y, ...) {
  stop(gettext("cannot merge aggregated indexes"))
}

merge.elemental <- function(x, y, ...) {
  if (!inherits(y, "elemental")) {
    stop(gettext("'y' is not an elemental index; use elemental_index() to make one"))
  }
  NextMethod()
}

merge.index <- function(x, y, ...) {
  if (any(x$time != y$time)) {
    stop(gettext("'x' and 'y' must be indexes for the same time periods"))
  }
  if (x$chained != y$chained) {
    stop(gettext("cannot merge a fixed-base and period-over-period index"))
  }
  if (length(intersect(x$levels, y$levels))) {
    warning(gettext("some levels appear in both 'x' and 'y'"))
  }
  if (!length(x$levels)) return(y)
  if (!length(y$levels)) return(x)
  for (t in x$time) {
    x$index[[t]] <- c(x$index[[t]], y$index[[t]])
    x$contrib[[t]] <- c(x$contrib[[t]], y$contrib[[t]])
    # might be useful if this method is extended to aggregated indexes
    # x$weights[[t]] <- c(x$weights[[t]], y$weights[[t]])
  }
  x$levels <- union(x$levels, y$levels)
  x$has_contrib <- x$has_contrib || y$has_contrib
  x
}

#---- Stack ----
stack.aggregate <- function(x, y, ...) {
  if (!inherits(y, "aggregate")) {
    stop(gettext("'y' is not an aggregate index; use aggregate() to make one"))
  }
  if (x$r != y$r) {
    stop(gettext("cannot stack indexes of different orders"))
  }
  if (!identical(x$pias, y$pias)) {
    stop(gettext("'x' and 'y' must be generated from the same aggregation structure"))
  }
  NextMethod()
}

stack.elemental <- function(x, y, ...) {
  if (!inherits(y, "elemental")) {
    stop(gettext("'y' is not an elemental index; use elemental_index() to make one"))
  }
  NextMethod()
}

stack.index <- function(x, y, ...) {
  if (any(x$levels != y$levels)) {
    stop(gettext("'x' and 'y' must be indexes for the same levels"))
  }
  if (x$chained != y$chained) {
    stop(gettext("cannot stack a period-over-period and a fixed-base index"))
  }
  if (length(intersect(x$time, y$time))) {
    warning(gettext("some periods appear in both 'x' and 'y'"))
  }
  if (!length(x$time)) return(y)
  if (!length(y$time)) return(x)
  x$index <- c(x$index, y$index)
  x$contrib <- c(x$contrib, y$contrib)
  x$time <- union(x$time, y$time)
  x$has_contrib <- x$has_contrib || y$has_contrib
  x
}

unstack.index <- function(x, ...) {
  if (!length(x$time)) return(x)
  res <- vector("list", length(x$time))
  for (i in seq_along(res)) {
    res[[i]]$index <- x$index[i]
    res[[i]]$contrib <- x$contrib[i]
    res[[i]]$levels <- x$levels
    res[[i]]$time <- x$time[i]
    res[[i]]$has_contrib <- x$has_contrib
    res[[i]]$chained <- x$chained
    res[[i]]$r <- x$r
    res[[i]]$pias <- x$pias
    class(res[[i]]) <- class(x)
  }
  res
}

#---- Printing ----
print.index <- function(x, ...) {
  print(as.matrix(x), ...)
  invisible(x)
}

head.index <- function(x, ...) {
  head(as.matrix(x), ...)
}

tail.index <- function(x,  ...) {
  tail(as.matrix(x), ...)
}

#---- Summary ----
summary.index <- function(object, ...) {
  res <- structure(vector("list", 2), names = c("index", "contrib"))
  res$index <- summary.data.frame(object$index, ...)
  res$contrib <- if (object$has_contrib) {
    summary.data.frame(lapply(object$contrib, unlist, use.names = FALSE), ...)
  }
  structure(res, class = "index_summary")
}

print.index_summary <- function(x, ...) {
  cat("Indexes\n")
  print(x$index)
  if (!is.null(x$contrib)) {
    cat("\nContributions\n")
    print(x$contrib)
  }
  invisible(x)
}
