expand_classification <- function(x, width = rep(1, max(nchar(x)))) {
  x <- as.character(x)
  width <- as.numeric(width)
  if (anyNA(width)) stop("'width' cannot contain NAs")
  w <- cumsum(width)
  x <- strsplit(x, character(0), fixed = TRUE)
  lapply(w, function(i) {
    vapply(x, paste_until, character(1), i)
  })
}
