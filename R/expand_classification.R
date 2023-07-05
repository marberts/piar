expand_classification <- function(class, width = 1L) {
  class <- as.character(class)
  width <- as.integer(width)
  if (any(width <= 0)) {
    stop("'width' must be strictly positive")
  }
  if (anyNA(width)) {
    stop("'width' cannot contain NAs")
  }
  
  if (length(width) == 1L) {
    longest <- max(nchar(class), 0L, na.rm = TRUE)
    width <- rep.int(width, ceiling(longest / width))
  }
  w <- cumsum(width)
  class <- strsplit(class, character(0L), fixed = TRUE)
  lapply(w, function(i) {
    vapply(class, paste_until, character(1L), i)
  })
}
