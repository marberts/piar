#---- Coerce ----
as.data.frame.ind <- function(x, ...) {
  # as.numeric() ensures that the value column shows up with NULL values
  value <- as.numeric(unlist(x$index, use.names = FALSE))
  period <- rep(x$time, each = length(x$levels))
  data.frame(period, level = x$levels, value, ...)
}

as.matrix.ind <- function(x, ...) {
  do.call(cbind, x$index)
}

#---- Extract ----
`[.ind` <- function(x, i, j) {
  # get the row/col names that form the submatrix for extraction
  dnm <- dimnames(as.matrix(x)[i, j, drop = FALSE])
  periods <- dnm[[2L]]
  levels <- dnm[[1L]]
  # it's safe to make a new object here because the class is always 'ind'
  res <- list(index = NULL, contrib = NULL, 
              levels = levels, time = periods, 
              has_contrib = x$has_contrib, chain = x$chain)
  # only loop over periods that have a value extracted
  for (t in periods) {
    res$index[[t]] <- x$index[[t]][levels]
    res$contrib[[t]] <- x$contrib[[t]][levels]
  }
  structure(res, class = "ind")
}

`[<-.ind` <- function(x, i, j, value) {
  res <- as.matrix(x)
  res[i, j] <- as.numeric(value)
  periods <- colnames(res[0L, j, drop = FALSE])
  # only loop over periods that have a value replaced
  for (t in periods) {
    x$index[[t]][i] <- res[i, t]
    # drop contributions for replaced values
    x$contrib[[t]][i] <- list(numeric(0L))
  }
  x
}

levels.ind <- function(x) {
  x$levels
}

time.ind <- function(x, ...) {
  x$time
}

start.ind <- function(x, ...) {
  x$time[1L]
}

end.ind <- function(x, ...) {
  x$time[length(x$time)]
}

#---- Merge ----
merge.agg_ind <- function(x, y, ...) {
  stop(gettext("cannot merge aggregated indexes"))
}

merge.ind <- function(x, y, ...) {
  if (!is_index(y)) {
    stop("'y' is not an index; use elemental_index() to make one")
  }
  if (!identical(x$time, y$time)) {
    stop(gettext("'x' and 'y' must be indexes for the same time periods"))
  }
  if (length(intersect(x$levels, y$levels))) {
    stop(gettext("the same levels appear in both 'x' and 'y'"))
  }
  if (x$chain != y$chain) {
    stop(gettext("cannot merge a fixed-base and period-over-period index"))
  }
  # loop over time periods and combine index values/contributions
  for (t in x$time) {
    x$index[[t]] <- c(x$index[[t]], y$index[[t]])
    x$contrib[[t]] <- c(x$contrib[[t]], y$contrib[[t]])
  }
  # it's safe to use c() and not union() because there can't be duplicate levels
  x$levels <- c(x$levels, y$levels)
  x$has_contrib <- x$has_contrib || y$has_contrib
  x
}

#---- Stack ----
stack.agg_ind <- function(x, y, ...) {
  if (!is_aggregate_index(y)) {
    stop(gettext("'y' is not an aggregate index; use aggregate() to make one"))
  }
  if (x$r != y$r) {
    stop(gettext("cannot stack indexes of different orders"))
  }
  if (!identical(x$pias, y$pias)) {
    stop(gettext("'x' and 'y' must be generated from the same aggregation structure"))
  }
  NextMethod("stack")
}

stack.ind <- function(x, y, ...) {
  if (!is_index(y)) {
    stop(gettext("'y' is not an index; use elemental_index() to make one"))
  }
  if (!identical(x$levels, y$levels)) {
    stop(gettext("'x' and 'y' must be indexes for the same levels"))
  }
  if (length(intersect(x$time, y$time))) {
    stop(gettext("the same periods appear in both 'x' and 'y'"))
  }
  if (x$chain != y$chain) {
    stop(gettext("cannot stack a period-over-period and a fixed-base index"))
  }
  x$index <- c(x$index, y$index)
  x$contrib <- c(x$contrib, y$contrib)
  # it's safe to use c() and not union() because there can't be duplicate periods
  x$time <- c(x$time, y$time)
  x$has_contrib <- x$has_contrib || y$has_contrib
  x
}

unstack.ind <- function(x, ...) {
  res <- vector("list", length(x$time))
  for (i in seq_along(res)) {
    res[[i]]$index <- x$index[i]
    res[[i]]$contrib <- x$contrib[i]
    res[[i]]$levels <- x$levels
    res[[i]]$time <- x$time[i]
    res[[i]]$has_contrib <- x$has_contrib
    res[[i]]$chain <- x$chain
    res[[i]]$r <- x$r
    res[[i]]$pias <- x$pias
    class(res[[i]]) <- class(x)
  }
  res
}

#---- Printing ----
print.ind <- function(x, ...) {
  print(as.matrix(x), ...)
  invisible(x)
}

head.ind <- function(x, ...) {
  head(as.matrix(x), ...)
}

tail.ind <- function(x,  ...) {
  tail(as.matrix(x), ...)
}

#---- Summary ----
summary.ind <- function(object, ...) {
  res <- structure(vector("list", 2L), names = c("index", "contrib"))
  res$index <- summary.data.frame(object$index, ...)
  res$contrib <- if (object$has_contrib) {
    summary.data.frame(lapply(object$contrib, unlist, use.names = FALSE), ...)
  }
  structure(res, class = "ind_summary")
}

print.ind_summary <- function(x, ...) {
  cat("Indexes\n")
  print(x$index)
  if (!is.null(x$contrib)) {
    cat("\nContributions\n")
    print(x$contrib)
  }
  invisible(x)
}

#---- Averaging ----
mean.ind <- function(x, w, window = 3, na.rm = FALSE, r = 1, ...) {
  index <- x$index
  if (!missing(w)) {
    if (length(w) != length(x$time) * length(x$levels)) {
      stop(gettext("'x' and 'w' must be the same length"))
    }
    w <- split(w, gl(length(x$time), length(x$levels)))
  }
  gen_mean <- Vectorize(generalized_mean(r))
  len <- length(x$time) %/% window
  # get the starting location for each window
  loc <- seq(1L, by = window, length.out = len)
  periods <- x$time[loc]
  res <- contrib <- structure(vector("list", len), names = periods)
  # loop over each window and calculate the mean for each level
  for (i in seq_along(loc)) {
    j <- seq(loc[i], length.out = window)
    res[[i]] <- if (missing(w)) {
      # structure() is needed because .mapply doesn't keep names
      gen_mean(structure(.mapply(c, index[j], list()), names = x$levels), na.rm = na.rm)
    } else {
      gen_mean(structure(.mapply(c, index[j], list()), names = x$levels), 
               .mapply(c, w[j], list()), na.rm = na.rm)
    }
  }
  contrib[] <- empty_contrib(x$levels)
  x$index <- res
  x$contrib <- contrib
  x$time <- periods
  x$has_contrib <- FALSE
  x
}
