#---- Coerce ----
as.data.frame.index <- function(x, row.names = NULL, ..., type = c("index", "contributions")) {
  type <- match.arg(type)
  res <- lapply(x[[type]], unlist)
  # as.numeric() ensures that the value column shows up with NULL values
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
  list2matrix(x)
}

#---- Extract ----
`[.index` <- function(x, i, j) {
  as.matrix(x)[i, j, drop = FALSE]
}

`[<-.index` <- function(x, i, j, value) {
  res <- as.matrix(x)
  res[i, j] <- as.numeric(value)
  periods <- colnames(res[0, j, drop = FALSE])
  for (t in periods) {
    x$index[[t]][i] <- res[i, t]
    if (x$contrib) x$contributions[[t]][i] <- list(numeric(0))
  }
  x
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

weights.aggregate <- function(object, ...) {
  list2matrix(object$weights)
}

#---- Math ----
cumprod.index <- function(x) {
  x$index[] <- Reduce("*", x$index, accumulate = TRUE)
  as.matrix(x)
}

update.aggregate <- function(object, period = end(object), ...) {
  price_update <- factor_weights(object$r)
  w <- if (length(object$periods)) {
    price_update(object$index[[period]][object$pias$eas],
                 object$weights[[period]])
  } else {
    structure(rep_len(NA_real_, length(object$pias$eas)), 
              names = object$pias$eas)
  }
  aggregate2pias(object, w)
}

#---- Merge/stack ----
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
    # might be useful if this method is extended to aggregated indexes
    # x$weights[[t]] <- c(x$weights[[t]], y$weights[[t]])
  }
  x$levels <- union(x$levels, y$levels)
  x$contrib <- x$contrib || y$contrib
  x
}

stack.aggregate <- function(x, y, ...) {
  if (!inherits(y, "aggregate")) {
    stop(gettext("'y' is not an aggregate index; use aggregate() to make one"))
  }
  if (x$r != y$r) {
    stop(gettext("cannot stack indexes of different orders"))
  }
  if (x$chained != y$chained) {
    stop(gettext("cannot stack a period-over-period and a fixed-base index"))
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
  if (length(intersect(x$periods, y$periods))) {
    warning(gettext("some periods appear in both 'x' and 'y'"))
  }
  if (!length(x$periods)) return(y)
  if (!length(y$periods)) return(x)
  x$index <- c(x$index, y$index)
  x$contributions <- c(x$contributions, y$contributions)
  x$periods <- union(x$periods, y$periods)
  x$weights <- c(x$weights, y$weights)
  x$contrib <- x$contrib || y$contrib
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
    res[[i]]$contrib <- x$contrib
    res[[i]]$weights <- x$weights[i]
    res[[i]]$r <- x$r
    res[[i]]$chained <- x$chained
    res[[i]]$pias <- x$pias
    class(res[[i]]) <- class(x)
  }
  res
}

#---- Print ----
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

summary.index <- function(object, ...) {
  res <- structure(vector("list", 2), names = c("index", "contrib"))
  res$index <- summary(as.matrix(object), ...)
  res$contrib <- if (object$contrib) summary(as.matrix(object, "contributions"), ...)
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
