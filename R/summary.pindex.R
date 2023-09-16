summary.pindex <- function(object, ...) {
  res <- vector("list", 2L)
  names(res) <- c("index", "contrib")
  res$index <- summary.data.frame(object$index, ...)
  res$contrib <- if (has_contrib(object)) {
    summary.data.frame(lapply(object$contrib, unlist, use.names = FALSE), ...)
  }
  structure(res, class = "pindex_summary")
}

print.pindex_summary <- function(x, ...) {
  cat("Indexes\n")
  print(x$index, ...)
  if (!is.null(x$contrib)) {
    cat("\nContributions\n")
    print(x$contrib, ...)
  }
  invisible(x)
}

str.pindex <- function(object, ...) {
  str(unclass(object), ...)
}

pindex_string <- function(x) {
  res <- c(aggregate_pindex = "aggregate",
           chainable_pindex = "period-over-period",
           direct_pindex = "fixed-base",
           pindex = "price index")[class(x)]
  res <- paste(res, collapse = " ")
  substr(res, 1, 1) <- toupper(substr(res, 1, 1))
  res
}

print.pindex <- function(x, ...) {
  cat(pindex_string(x), "for", length(x$levels), "levels over", length(x$time), 
      "time periods", "\n")
  print(as.matrix(x), ...)
  invisible(x)
}
