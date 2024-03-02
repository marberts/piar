#' Summarize a price index
#'
#' Summarize an index as a matrix of index values (i.e., the five-number
#' summary for each period). If there are percent-change contributions, then
#' these are also summarized as a matrix.
#'
#' @param object A price index, as made by, e.g., [elemental_index()].
#' @param ... Additional arguments passed to [`summary.matrix()`].
#'
#' @returns
#' A list of five-number summaries.
#'
#' @note
#' This function is still experimental and may change in the future.
#'
#' @examples
#' prices <- data.frame(
#'   rel = 1:8,
#'   period = rep(1:2, each = 4),
#'   ea = rep(letters[1:2], 4)
#' )
#'
#' epr <- with(prices, elemental_index(rel, period, ea))
#'
#' summary(epr)
#'
#' @export
summary.piar_index <- function(object, ...) {
  res <- vector("list", 2L)
  names(res) <- c("index", "contrib")
  res$index <- summary(as.matrix(object), ...)
  res$contrib <- if (has_contrib(object)) {
    contrib <- lapply(object$contrib, unlist, use.names = FALSE)
    names(contrib) <- object$time
    summary(do.call(cbind, contrib), ...)
  }
  structure(res, class = "piar_index_summary")
}

#' @export
print.piar_index_summary <- function(x, ...) {
  cat("Indexes\n")
  print(x$index, ...)
  if (!is.null(x$contrib)) {
    cat("\nContributions\n")
    print(x$contrib, ...)
  }
  invisible(x)
}
