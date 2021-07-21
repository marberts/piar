expand_classification <- function(x, width = rep(1, max(nchar(x), 0, na.rm = TRUE))) {
  x <- as.character(x)
  width <- as.numeric(width)
  if (anyNA(width)) {
    stop(gettext("'width' cannot contain NAs"))
  }
  w <- cumsum(width)
  x <- strsplit(x, character(0), fixed = TRUE)
  lapply(w, function(i) {
    vapply(x, paste_until, character(1), i)
  })
}
