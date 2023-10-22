#' Coerce a price index aggregation structure into a tabular form
#'
#' Coerce a price index aggregation structure into an aggregation matrix, or a
#' data frame.
#'
#' @param x A price index aggregation structure, as made by
#' [aggregation_structure()].
#' @param stringsAsFactors See [as.data.frame()].
#' @param ... Further arguments passed to or used by methods.
#'
#' @returns
#' `as.matrix()` represents an aggregation structure as a matrix,
#' such that multiplying with a (column) vector of elemental indexes gives the
#' aggregated index.
#'
#' `as.data.frame()` takes an aggregation structure and returns a data
#' frame that could have generated it, with columns `level1`,
#' `level2`, ..., `ea`, and `weight`.
#'
#' @seealso
#' [as_aggregation_structure()] for coercing into an aggregation structure.
#'
#' @examples
#' # A simple aggregation structure
#' #            1
#' #      |-----+-----|
#' #      11          12
#' #  |---+---|       |
#' #  111     112     121
#' #  (1)     (3)     (4)
#'
#' aggregation_weights <- data.frame(
#'   level1 = c("1", "1", "1"),
#'   level2 = c("11", "11", "12"),
#'   ea     = c("111", "112", "121"),
#'   weight = c(1, 3, 4)
#' )
#'
#' pias <- as_aggregation_structure(aggregation_weights)
#'
#' as.matrix(pias)
#'
#' all.equal(as.data.frame(pias), aggregation_weights)
#'
#' @export
as.matrix.piar_aggregation_structure <- function(x, ...) {
  nea <- length(x$eas)
  if (x$height == 1L) {
    return(matrix(numeric(0), ncol = nea, dimnames = list(NULL, x$eas)))
  }
  loc <- seq_len(nea)
  # don't need the eas
  lev <- lapply(as.list(x)[-x$height], as.factor)
  rows <- vector("list", length(lev))
  # generate the rows for each level of the matrix and rbind together
  for (i in seq_along(rows)) {
    mat <- matrix(0,
      nrow = nlevels(lev[[i]]), ncol = nea,
      dimnames = list(levels(lev[[i]]), x$eas)
    )
    # splitting orders the rows of the matrix the same as the aggregation
    # structure
    cols <- split(loc, lev[[i]])
    w <- split(x$weights, lev[[i]])
    for (r in seq_len(nrow(mat))) {
      mat[r, cols[[r]]] <- gpindex::scale_weights(w[[r]])
    }
    rows[[i]] <- mat
  }
  do.call(rbind, rows)
}

#' @rdname as.matrix.piar_aggregation_structure
#' @export
as.data.frame.piar_aggregation_structure <- function(x, ...,
                                                     stringsAsFactors = FALSE) {
  colnames <- c(paste0("level", seq_along(x$child), recycle0 = TRUE), "ea")
  res <- as.data.frame(as.list(x),
    col.names = colnames,
    stringsAsFactors = stringsAsFactors
  )
  res$weight <- x$weight
  res
}

#' @export
as.list.piar_aggregation_structure <- function(x, ...) {
  if (x$height == 1L) {
    return(list(x$eas))
  }
  res <- vector("list", length(x$parent))
  res[[1L]] <- x$parent[[1L]]
  # walk up the parent nodes to reconstruct the inputs that generated 'x'
  for (i in seq_along(x$parent)[-1L]) {
    res[[i]] <- x$parent[[i]][res[[i - 1L]]]
  }
  top <- names(x$child[[length(x$child)]])[res[[length(res)]]]
  c(list(top), lapply(rev(res), names))
}
