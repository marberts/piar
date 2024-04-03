#' Extract percent-change contributions
#'
#' Extract a matrix of percent-change contributions from a price index.
#'
#' @param x A price index, as made by, e.g., [elemental_index()].
#' @param level The level of an index for which percent-change contributions
#' are desired, defaulting to the first level (usually the top-level for an
#' aggregate index).
#' @param period The time periods for which percent-change contributions are
#' desired, defaulting to all time periods.
#' @param pad A numeric value to pad contributions so that they fit into a
#' rectangular array when products differ over time. The default is 0.
#' @param ... Further arguments passed to or used by methods.
#'
#' @returns
#' A matrix of percent-change contributions with a column for each
#' `period` and a row for each product (sorted) for which there are
#' contributions in `level`. Contributions are padded with `pad` to fit into a
#' rectangular array when products differ over time.
#'
#' @examples
#' prices <- data.frame(
#'   rel = 1:8,
#'   period = rep(1:2, each = 4),
#'   ea = rep(letters[1:2], 4)
#' )
#'
#' index <- with(
#'   prices,
#'   elemental_index(rel, period, ea, contrib = TRUE)
#' )
#'
#' pias <- aggregation_structure(
#'   list(c("top", "top", "top"), c("a", "b", "c")), 1:3
#' )
#'
#' index <- aggregate(index, pias, na.rm = TRUE)
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
#'   weights(pias)
#' )
#'
#' @export contrib
contrib <- function(x, ...) {
  UseMethod("contrib")
}

#' @rdname contrib
#' @family index methods
#' @export
contrib.piar_index <- function(x, level = levels(x)[1L], period = time(x), ...,
                               pad = 0) {
  level <- match.arg(as.character(level), x$levels)
  period <- match.arg(as.character(period), x$time, several.ok = TRUE)
  pad <- as.numeric(pad)
  if (length(pad) != 1L) {
    stop("'pad' must be a length 1 numeric value")
  }
  con <- lapply(x$contrib[match(period, x$time)], `[[`, match(level, x$levels))

  con_names <- lapply(con, names)
  products <- sort.int(unique(unlist(con_names, use.names = FALSE)))

  out <- vector("list", length(con))
  names(out) <- period

  # Initialize 0 contributions for all products in all time periods, then
  # replace with the actual values so products that didn't sell have 0 and
  # not NA contributions.
  out[] <- list(structure(rep.int(pad, length(products)), names = products))
  res <- Map(replace, out, con_names, con)
  do.call(cbind, res)
}
