#' Split a hierarchical classification
#'
#' Expand a character representation of a hierarchical classification to make a
#' price index aggregation structure by splitting along a delimiter.
#'
#' @param x A character vector, or something that can be coerced into one, of
#'   codes/labels for a specific level in a classification (e.g., 5-digit
#'   COICOP).
#' @param split A regular expression to delineate and split the levels in `x`.
#'   See [strsplit()].
#' @param ... Additional argument to pass to [strsplit()].
#' @param sep A character used to delineate levels in `x` in the result. The
#'   default separates levels by '.'.
#' @param pad A string used to pad the shorter labels for an unbalanced
#'   classification. The default pads with NA.
#'
#' @returns
#' A list with a entry for each level in `x` giving the "digits" that
#' represent each level in the hierarchy.
#'
#' @seealso
#' [aggregation_structure()] to make a price-index aggregation structure.
#'
#' [expand_classification()] to expand a classification by the width of the
#' levels.
#'
#' @examples
#' #' # A simple classification structure
#' #            1
#' #      |-----+-----|
#' #      11          12
#' #  |---+---|       |
#' #  111     112     121
#'
#' split_classification(c("111", "112", "121"), "")
#'
#' # Useful if there are delimiters in the classification (like COICOP)
#'
#' split_classification(c("01.1.1", "01.1.2", "01.2.1"), ".", fixed = TRUE)
#'
#' @export
split_classification <- function(x, split, ..., sep = ".", pad = NA) {
  x <- as.character(x)
  if (length(x) == 0L) {
    return(list())
  }
  x <- strsplit(x, split = split, ...)
  n <- max(lengths(x))
  if (n == 0L) {
    return(list(rep.int("", length(x))))
  }
  res <- do.call(rbind, lapply(x, \(z) padded_extract(z, n, pad)))
  Reduce(
    \(...) paste(..., sep = sep),
    split(res, gl(ncol(res), nrow(res))),
    accumulate = TRUE,
    simplify = FALSE
  )
}
