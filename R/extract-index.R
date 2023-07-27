dim_indices <- function(x, i) {
  names(x) <- x
  match(x[i], x, incomparables = NA)
}

`[.index` <- function(x, i, j) {
  levels <- dim_indices(x$levels, i)
  periods <- dim_indices(x$time, j)
  x$index <- lapply(x$index[periods], `[`, levels)
  x$contrib <- lapply(x$contrib[periods], `[`, levels)
  x$levels <- x$levels[levels]
  x$time <- x$time[periods]
  validate_index(x)
}

`[[.index` <- function(x, i, j, exact = TRUE) {
  as.matrix(x)[[i, j, exact = exact]]
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
  levels <- dim_indices(x$levels, i)
  periods <- dim_indices(x$time, j)
  res <- as.matrix(x)
  res[levels, periods] <- as.numeric(value)
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

