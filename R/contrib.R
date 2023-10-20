#---- Extract contributions ----


#' Extract percent-change contributions
#' 
#' Extract a matrix of percent-change contributions from a price index.
#' 
#' 
#' @aliases contrib contrib.piar_index
#' @param x A price index, as made by, e.g.,
#' [`elemental_index()`][elemental_index].
#' @param level The level of an index for which percent-change contributions
#' are desired, defaulting to the first level (usually the top-level for an
#' aggregate index).
#' @param ... Further arguments passed to or used by methods.
#' @return A matrix of percent-change contributions with a column for each
#' `period` and a row for each product for which there are contributions
#' in `level`. Contributions are padded with 0 to fit into a rectangular
#' array when products differ over time.
#' @examples
#' 
#' prices <- data.frame(
#'   rel = 1:8,
#'   period = rep(1:2, each = 4),
#'   ea = rep(letters[1:2], 4)
#' )
#' 
#' epr <- with(
#'   prices,
#'   elemental_index(rel, period, ea, contrib = TRUE)
#' )
#' 
#' pias <- aggregation_structure(
#'   list(c("top", "top", "top"), c("a", "b", "c")), 1:3
#' )
#' 
#' index <- aggregate(epr, pias, na.rm = TRUE)
#' 
#' # Percent-change contributions for the top-level index
#' 
#' contrib(index)
#' 
#' # Calculate EA contributions for the chained index
#' 
#' library(gpindex)
#' 
#' arithmetic_contributions(
#'   as.matrix(chain(index))[c("a", "b", "c"), 2], 
#'   weights(pias, ea_only = TRUE)
#' )
#' 
#' @export contrib
contrib <- function(x, ...) {
  UseMethod("contrib")
}

contrib.piar_index <- function(x, level = levels(x), ...) {
  level <- as.character(level)
  con <- lapply(x$contrib, `[[`, match(match.arg(level), x$levels))
  products <- unique(unlist(lapply(con, names), use.names = FALSE))
  out <- vector("list", length(con))
  names(out) <- x$time
  # initialize 0 contributions for all products in all time periods, then 
  # replace with the actual values so products that didn't sell have 0 and 
  # not NA contributions
  out[] <- list(structure(numeric(length(products)), names = products))
  res <- Map(replace, out, lapply(con, names), con)
  do.call(cbind, res)
}
