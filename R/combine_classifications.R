#' Combine hierarchical classifications
#'
#' Combine hierarchical classifications by stacking one classification after
#' another.
#'
#' @param ... A collection of lists, one for each classification, each giving
#'   the "digits" that represent each level in the hierarchy, as made
#'   by [expand_classification()] or [split_classification()].
#' @param sep A character used to separate the classifications in `...`. The
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
combine_classifications <- function(..., sep = ".") {
  dots <- lapply(list(...), \(x) lapply(x, as.character))
  Reduce(
    \(x, y) combine_classifications_(x, y, sep = sep),
    dots,
    simplify = FALSE
  )
}

#' Combine classifications (internal)
#' @noRd
combine_classifications_ <- function(x, y, sep) {
  lx <- lengths(x)
  if (length(lx) == 0L) {
    return(y)
  }
  ly <- lengths(y)
  if (length(ly) == 0L) {
    return(x)
  }
  if (any(lx != lx[1L]) || any(ly != lx[1L])) {
    stop("each component of '...' must have the same number of elements")
  }

  c(x, lapply(y, \(y) paste(last(x), y, sep = sep)))
}
