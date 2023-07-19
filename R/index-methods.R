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
    new_index(res$index, res$contrib, res$levels, res$time,
              is_chainable_index(res))
  } else {
    res 
  }
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
  # make a slice matrix that indexes the same as if 'x' were a matrix
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
    if (n[1L] < 0L) {
      nl <- max(levels + n[1L], 0L)
    } else {
      nl <- min(n[1L], levels)
    }
  }
  if (!is.na(n[2L])) {
    if (n[2L] < 0L) {
      np <- max(periods + n[2L], 0L)
    } else {
      np <- min(n[2L], periods)
    }
  }
  x[seq_len(nl), seq_len(np)]
}

tail.index <- function(x, n = 6L, ...) {
  nl <- levels <- length(x$levels)
  np <- periods <- length(x$time)
  if (!is.na(n[1L])) {
    if (n[1L] < 0L) {
      nl <- max(levels + n[1L], 0L)
    } else {
      nl <- min(n[1L], levels)
    }
  }
  if (!is.na(n[2L])) {
    if (n[2L] < 0L) {
      np <- max(periods + n[2L], 0L)
    } else {
      np <- min(n[2L], periods)
    }
  }
  i <- seq.int(to = levels, length.out = nl)
  j <- seq.int(to = periods, length.out = np)
  x[i, j]
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
