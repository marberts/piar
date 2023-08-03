summary.index <- function(object, ...) {
  res <- vector("list", 2L)
  names(res) <- c("index", "contrib")
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

str.index <- function(object, ...) {
  str(as.list(object), ...)
}

print.index <- function(x, ...) {
  print(as.matrix(x), ...)
  invisible(x)
}
