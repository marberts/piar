paste_until <- function(x, i) {
  paste(x[seq_len(i)], collapse = "")
}

expand_classification <- function(x, width = 1L) {
  x <- as.character(x)
  width <- as.integer(width)
  if (any(width <= 0)) {
    stop("'width' must be strictly positive")
  }
  if (anyNA(width)) {
    stop("'width' cannot contain NAs")
  }

  if (length(width) == 1L) {
    longest <- max(nchar(x), 0L, na.rm = TRUE)
    width <- rep.int(width, ceiling(longest / width))
  }
  w <- cumsum(width)
  x <- strsplit(x, character(0L), fixed = TRUE)
  lapply(w, function(i) {
    vapply(x, paste_until, character(1L), i)
  })
}
