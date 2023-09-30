summary.piar_index <- function(object, ...) {
  res <- vector("list", 2L)
  names(res) <- c("index", "contrib")
  res$index <- summary(as.matrix(object), ...)
  res$contrib <- if (has_contrib(object)) {
    contrib <- lapply(object$contrib, unlist, use.names = FALSE)
    names(contrib) <- object$time
    summary(do.call(cbind, contrib, ...))
  }
  structure(res, class = "piar_index_summary")
}

print.piar_index_summary <- function(x, ...) {
  cat("Indexes\n")
  print(x$index, ...)
  if (!is.null(x$contrib)) {
    cat("\nContributions\n")
    print(x$contrib, ...)
  }
  invisible(x)
}
