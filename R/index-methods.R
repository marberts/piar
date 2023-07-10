#---- Coerce ----
as.data.frame.index <- function(x, ..., stringsAsFactors = FALSE) {
  value <- unlist(x$index, use.names = FALSE)
  period <- rep(x$time, each = length(x$levels))
  data.frame(period, level = x$levels, value,
             stringsAsFactors = stringsAsFactors)
}

as.matrix.index <- function(x, ...) {
  do.call(cbind, x$index)
}

# not exported
as.double.index <- function(x, ...) {
  as.double(as.matrix(x))
}

#---- Extract ----
`[.index` <- function(x, i, j) {
  # get the row/col names that form the submatrix for extraction
  dnm <- dimnames(as.matrix(x)[i, j, drop = FALSE])
  if (!all(lengths(dnm) > 0L)) {
    stop("cannot extract no levels/time periods from 'x'")
  }
  levels <- dnm[[1L]]
  periods <- dnm[[2L]]
  # only loop over periods that have a value extracted
  x$index <- lapply(x$index[periods], `[`, levels)
  x$contrib <- lapply(x$contrib[periods], `[`, levels)
  x$levels <- levels
  x$time <- periods
  x
}

`[[.index` <- function(x, i, j) {
  as.matrix(x)[[i, j]]
}

`[.aggregate_index` <- function(x, i, j) {
  res <- NextMethod("[")
  if (!identical(res$levels, x$levels)) {
    res$r <- res$pias <- NULL
    class(res) <- class(res)[-1]
  }
  res
}

`[<-.index` <- function(x, i, j, value) {
  res <- as.matrix(x)
  res[i, j] <- as.numeric(value)
  periods <- colnames(res[0L, j, drop = FALSE])
  levels <- rownames(res[i, 0L, drop = FALSE])
  # only loop over periods that have a value replaced
  for (t in periods) {
    x$index[[t]][levels] <- res[levels, t]
    # drop contributions for replaced values
    x$contrib[[t]][levels] <- list(numeric(0L))
  }
  x
}

`[[<-.index` <- function(x, i, j, value) {
  dim <- c(length(x$levels), length(x$time))
  row_slice <- .row(dim)
  col_slice <- .col(dim)
  dimnames(row_slice) <- dimnames(col_slice) <- list(x$levels, x$time)
  
  i <- row_slice[[i, j]]
  j <- col_slice[[i, j]]
  level <- x$levels[[i]]
  period <- x$time[[j]]
  
  x$index[[period]][[level]] <- as.numeric(value)
  x$contrib[[period]][[level]] <- numeric(0L)
  
  x
}

levels.index <- function(x) {
  x$levels
}

`levels<-.index` <- function(x, value) {
  stop("cannot replace levels attribute")
}

time.index <- function(x, ...) {
  x$time
}

start.index <- function(x, ...) {
  x$time[1L]
}

end.index <- function(x, ...) {
  x$time[length(x$time)]
}

head.index <- function(x, n = 6L, ...) {
  nl <- levels <- length(x$levels)
  np <- periods <- length(x$time)
  if (!is.na(n[1L])) {
    nl <- if (n[1L] < 0L) {
      max(levels + n[1L], 0L)
    } else {
      min(n[1L], levels)
    }
  }
  if (!is.na(n[2L])) {
    np <- if (n[2L] < 0L) {
      max(periods + n[2L], 0L)
    } else {
      min(n[2L], periods)
    }
  }
  x[seq_len(nl), seq_len(np)]
}

tail.index <- function(x, n = 6L, ...) {
  nl <- levels <- length(x$levels)
  np <- periods <- length(x$time)
  if (!is.na(n[1L])) {
    nl <- if (n[1L] < 0L) {
      max(levels + n[1L], 0L)
    } else {
      min(n[1L], levels)
    }
  }
  if (!is.na(n[2L])) {
    np <- if (n[2L] < 0L) {
      max(periods + n[2L], 0L)
    } else {
      min(n[2L], periods)
    }
  }
  i <- seq.int(to = levels, length.out = nl)
  j <- seq.int(to = periods, length.out = np)
  x[i, j]
}

#---- Merge ----
merge.aggregate_index <- function(x, y, ...) {
  x$r <- x$pias <- NULL
  class(x) <- class(x)[-1]
  NextMethod("merge")
}

merge.chainable_index <- function(x, y, ...) {
  if (!is_index(y)) {
    stop("'y' is not an index; use elemental_index() to make one")
  }
  if (!is_chainable_index(y)) {
    stop("cannot merge a fixed-base and period-over-period index")
  }
  NextMethod("merge")
}

merge.direct_index <- function(x, y, ...) {
  if (!is_index(y)) {
    stop("'y' is not an index; use elemental_index() to make one")
  }
  if (is_chainable_index(y)) {
    stop("cannot merge a fixed-base and period-over-period index")
  }
  NextMethod("merge")
}

merge.index <- function(x, y, ...) {
  if (!identical(x$time, y$time)) {
    stop("'x' and 'y' must be indexes for the same time periods")
  }
  if (any(x$levels %in% y$levels)) {
    stop("the same levels cannot appear in both 'x' and 'y'")
  }
  x$index <- Map(c, x$index, y$index)
  x$contrib <- Map(c, x$contrib, y$contrib)
  # it's safe to use c() and not union() because there can't be duplicate levels
  x$levels <- c(x$levels, y$levels)
  x
}

#---- Stack ----
stack.aggregate_index <- function(x, y, ...) {
  if (is_aggregate_index(y)) {
    if (x$r != y$r) {
      stop("cannot stack indexes of different orders")
    }
    if (!identical(x$pias, y$pias)) {
      stop("'x' and 'y' must be generated from the same aggregation structure")
    }
  }
  NextMethod("stack")
}

stack.chainable_index <- function(x, y, ...) {
  if (!is_index(y)) {
    stop("'y' is not an index; use elemental_index() to make one")
  }
  if (!is_chainable_index(y)) {
    stop("cannot stack a period-over-period and a fixed-base index")
  }
  NextMethod("stack")
}

stack.direct_index <- function(x, y, ...) {
  if (!is_index(y)) {
    stop("'y' is not an index; use elemental_index() to make one")
  }
  if (is_chainable_index(y)) {
    stop("cannot stack a period-over-period and a fixed-base index")
  }
  NextMethod("stack")
}

stack.index <- function(x, y, ...) {
  if (!identical(x$levels, y$levels)) {
    stop("'x' and 'y' must be indexes for the same levels")
  }
  if (any(x$time %in% y$time)) {
    stop("the same time periods cannot appear in both 'x' and 'y'")
  }
  x$index <- c(x$index, y$index)
  x$contrib <- c(x$contrib, y$contrib)
  # it's safe to use c() and not union() because there can't be duplicate
  # periods
  x$time <- c(x$time, y$time)
  if (is_aggregate_index(y) && !is_aggregate_index(x)) {
    x$r <- y$r
    x$pias <- y$pias
    class(x) <- c("aggregate_index", class(x))
  }
  x
}

unstack.index <- function(x, ...) {
  res <- vector("list", length(x$time))
  names(res) <- x$time
  for (i in seq_along(res)) {
    res[[i]]$index <- x$index[i]
    res[[i]]$contrib <- x$contrib[i]
    res[[i]]$levels <- x$levels
    res[[i]]$time <- x$time[i]
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

#---- Summary ----
summary.index <- function(object, ...) {
  res <- structure(vector("list", 2L), names = c("index", "contrib"))
  res$index <- summary.data.frame(object$index, ...)
  res$contrib <- if (has_contrib(object)) {
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
