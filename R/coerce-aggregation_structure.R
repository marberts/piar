#' Coerce a price index aggregation structure into a tabular form
#'
#' Coerce a price index aggregation structure into an aggregation matrix, or a
#' data frame.
#'
#' @param x A price index aggregation structure, as made by
#' [aggregation_structure()].
#' @param sparse Should the result be a sparse matrix from \pkg{Matrix}? This
#' is faster for large aggregation structures. The default returns an ordinary
#' dense matrix.
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
#' @family aggregation structure methods
#' @export
as.matrix.piar_aggregation_structure <- function(x, sparse = FALSE, ...) {
  nea <- length(x$eas)
  if (x$height == 1L) {
    res <- matrix(numeric(0), ncol = nea, dimnames = list(NULL, x$eas))
    if (sparse) {
      return(Matrix::Matrix(res, sparse = TRUE))
    } else {
      return(res)
    }
  }
  cols <- seq_len(nea)
  # don't need the eas
  lev <- lapply(as.list(x)[-x$height], \(z) factor(z, unique(z)))
  res <- vector("list", length(lev))
  # generate the rows for each level of the matrix and rbind together
  for (i in seq_along(res)) {
    w <- unsplit(
      lapply(split(x$weights, lev[[i]]), gpindex::scale_weights), lev[[i]]
    )
    if (sparse) {
      mat <- Matrix::sparseMatrix(lev[[i]], cols, x = w)
    } else {
      mat <- matrix(0, nlevels(lev[[i]]), nea)
      mat[cbind(lev[[i]], cols)] <- w
    }
    dimnames(mat) <- list(levels(lev[[i]]), x$eas)
    res[[i]] <- mat
  }
  do.call(rbind, res)
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
