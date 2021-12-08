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
    if (x$has_contrib) {
      # remove contributions for replaced values
      x$contrib$w[[t]][i] <- x$contrib$r[[t]][i] <- list(numeric(0))
    }
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

contrib <- function(x, ...) {
  UseMethod("contrib")
}

contrib.index <- function(x, level = levels(x), ...) {
  level <- match.arg(level)
  # use decomposition weights and relatives to calculate contributions
  w <- lapply(x$contrib$w, `[[`, level)
  r <- lapply(x$contrib$r, `[[`, level)
  con <- Map(function(w, r) w * (r - 1), w, r)
  products <- unique(nested_names(con))
  out <- structure(vector("list", length(con)), names = names(con))
  # products without a relative in a period have a contribution of 0
  out[] <- list(structure(numeric(length(products)), names = products))
  res <- Map(replace, out, lapply(con, names), con)
  list2matrix(res)
}

#---- Math ----
cumprod.index <- function(x) {
  if (x$chained) {
    x$chained <- FALSE
    x$index[] <- Reduce(`*`, x$index, accumulate = TRUE)
    for (t in seq_along(x$time)[-1]) {
      x$contrib$r[[t]] <- Map(`*`, x$contrib$r[[t]], x$index[[t - 1]])
    }
  }
  x
}

update.aggregate <- function(object, period = end(object), ...) {
  price_update <- factor_weights(object$r)
  w <- if (length(object$time)) {
    price_update(object$index[[period]][object$pias$eas],
                 object$weights[[period]])
  } else {
    # it's possible to have an index with levels and no periods,
    # in which case price updating should return NAs
    rep_len(NA_real_, length(object$pias$eas))
  }
  aggregate2pias(object, w)
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
    x$contrib$w[[t]] <- c(x$contrib$w[[t]], y$contrib$w[[t]])
    x$contrib$r[[t]] <- c(x$contrib$r[[t]], y$contrib$r[[t]])
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
  x$contrib$w <- c(x$contrib$w, y$contrib$w)
  x$contrib$r <- c(x$contrib$r, y$contrib$r)
  x$time <- union(x$time, y$time)
  x$weights <- c(x$weights, y$weights)
  x$has_contrib <- x$has_contrib || y$has_contrib
  x
}

unstack.index <- function(x, ...) {
  if (!length(x$time)) return(x)
  res <- vector("list", length(x$time))
  for (i in seq_along(res)) {
    res[[i]]$index <- x$index[i]
    res[[i]]$contrib$w <- x$contrib$w[i]
    res[[i]]$contrib$r <- x$contrib$r[i]
    res[[i]]$levels <- x$levels
    res[[i]]$time <- x$time[i]
    res[[i]]$has_contrib <- x$has_contrib
    res[[i]]$weights <- x$weights[i]
    res[[i]]$r <- x$r
    res[[i]]$chained <- x$chained
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
  res <- structure(vector("list", 1), names = c("index"))
  res$index <- summary.data.frame(object$index, ...)
  structure(res, class = "index_summary")
}

print.index_summary <- function(x, ...) {
  cat("Indexes\n")
  print(x$index)
  invisible(x)
}
