#' Combine hierarchical classifications
#'
#' Combine hierarchical classifications by stacking one classification after
#' another.
#'
#' @param x,y A list with a entry for each level in classification giving the
#'   "digits" that represent each level in the hierarchy, as made
#'   by [expand_classification()] or [split_classification()].
#' @param sep A character used to separate `x` and `y`. The
#'   default separates levels across classifications by `"."`.
#' @returns
#' A list with a entry for each level in the combined classification.
#'
#' @examples
#' # Combine an unbalanced industry classification with a balanced
#' # geographic classification
#'
#' industry <- c("111", "112", "12")
#' region <- c("11", "21", "22")
#'
#' combine_classifications(
#'   expand_classification(industry, pad = "0"),
#'   expand_classification(region)
#' )
#' @export
combine_classifications <- function(x, y, sep = ".") {
  x <- lapply(x, as.character)
  lx <- lengths(x)
  if (length(lx) == 0L) {
    return(y)
  }
  y <- lapply(y, as.character)
  ly <- lengths(y)
  if (length(ly) == 0L) {
    return(x)
  }
  if (any(lx != lx[1L]) || any(ly != lx[1L])) {
    stop("each component of 'x' and 'y' must have the same number of elements")
  }

  c(x, lapply(y, \(y) paste(last(x), y, sep = sep)))
}
