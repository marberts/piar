head.piar_index <- function(x, n = 6L, ...) {
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

tail.piar_index <- function(x, n = 6L, ...) {
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
