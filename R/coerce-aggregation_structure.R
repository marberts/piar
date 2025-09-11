#' Coerce an aggregation structure into a tabular form
#'
#' Coerce a price index aggregation structure into an aggregation matrix, or a
#' data frame.
#'
#' @param x A price index aggregation structure, as made by
#'   [aggregation_structure()].
#' @param sparse Should the result be a sparse matrix from \pkg{Matrix}? This
#'   is faster for large aggregation structures. The default returns an ordinary
#'   dense matrix.
#' @param row.names See [as.data.frame()].
#' @param optional Not currently used.
#' @param ... Not currently used for the matrix method. Extra arguments to
#'   [as.data.frame.list()] for the data frame method.
#'
#' @returns
#' `as.matrix()` represents an aggregation structure as a matrix,
#' such that multiplying with a (column) vector of elementary indexes gives the
#' aggregated index.
#'
#' `as.data.frame()` takes an aggregation structure and returns a data
#' frame that could have generated it.
#'
#' @seealso
#' [as_aggregation_structure()] for coercing into an aggregation structure.
#'
#' [treemap::treemap()] and [data.tree::as.Node()] for visualizing an
#' aggregation structure.
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
#' \dontrun{
#' # Visualize as a treemap.
#' treemap::treemap(
#'   aggregation_weights,
#'   index = names(aggregation_weights)[-4],
#'   vSize = "weight",
#'   title = "aggregation structure"
#' )
#'
#' # Or turn into a more genereal tree object and plot.
#' aggregation_weights$pathString <- do.call(
#'   \(...) paste(..., sep = "/"),
#'   aggregation_weights[-4]
#' )
#' plot(data.tree::as.Node(aggregation_weights))
#' }
#'
#' @family aggregation structure methods
#' @export
as.matrix.piar_aggregation_structure <- function(x, ..., sparse = FALSE) {
  chkDots(...)
  nea <- length(x$weights)
  height <- nlevels(x)
  if (height == 1L) {
    res <- matrix(
      numeric(0L),
      ncol = nea,
      dimnames = list(NULL, x$levels[[1L]])
    )
    if (sparse) {
      return(Matrix::Matrix(res, sparse = TRUE))
    } else {
      return(res)
    }
  }
  cols <- seq_len(nea)
  # Don't need the eas.
  lev <- lapply(as.list(x)[-height], \(z) factor(z, unique(z)))
  res <- vector("list", length(lev))
  # Generate the rows for each level of the matrix and rbind together.
  for (i in seq_along(res)) {
    w <- unsplit(
      lapply(split(x$weights, lev[[i]]), gpindex::scale_weights),
      lev[[i]]
    )
    if (sparse) {
      mat <- Matrix::sparseMatrix(lev[[i]], cols, x = w)
    } else {
      mat <- matrix(0, nlevels(lev[[i]]), nea)
      mat[cbind(lev[[i]], cols)] <- w
    }
    dimnames(mat) <- list(levels(lev[[i]]), x$levels[[height]])
    res[[i]] <- mat
  }
  res <- do.call(rbind, res)
  names(dimnames(res)) <- c("levels", "")
  res
}

#' @rdname as.matrix.piar_aggregation_structure
#' @export
as.data.frame.piar_aggregation_structure <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  ...
) {
  res <- as.data.frame(
    as.list(x),
    row.names = row.names,
    optional = optional,
    ...
  )
  res$weight <- x$weights
  res
}

#' @export
as.list.piar_aggregation_structure <- function(x, ...) {
  chkDots(...)
  if (nlevels(x) == 1L) {
    return(x$levels[1L])
  }
  res <- vector("list", length(x$parent))
  res[[1L]] <- x$parent[[1L]]
  # Walk up the parent nodes to reconstruct the inputs that generated 'x'.
  for (i in seq_along(x$parent)[-1L]) {
    res[[i]] <- x$parent[[i]][res[[i - 1L]]]
  }
  top <- names(last(x$child))[last(res)]
  res <- c(list(top), lapply(rev(res), names))
  names(res) <- names(x$levels)
  res
}
