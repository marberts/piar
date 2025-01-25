#' Expand a hierarchical classification
#'
#' Expand a character representation of a hierarchical classification to make a
#' price index aggregation structure. Expanded classifications be interacted
#' together to get all combinations of aggregation structures.
#'
#' @param x A character vector, or something that can be coerced into one, of
#'   codes/labels for a specific level in a classification (e.g., 5-digit
#'   COICOP, 5-digit NAICS, 4-digit SIC).
#' @param width An integer vector that gives the width of each digit in
#'   `x`. A single value is recycled to span the longest element in
#'   `x`. This cannot contain NAs. The default assumes each digit has a
#'   width of 1, as in the NAICS, NAPCS, and SIC classifications.
#' @param ... Lists of character vectors that give the codes/labels for each
#'   level of the classification, ordered so that moving down the list goes down
#'   the hierarchy (as made by `expand_classification()`).
#' @param sep A character used to combine codes/labels across elements of `...`.
#'   The default uses ":".
#'
#' @returns
#' `expand_classification()` returns a list with a entry for each level
#' in `x` giving the "digits" that represent each level in the hierarchy.
#'
#' `interact_classfications()` returns a list of lists with the same structure
#' as `expand_classification()`.
#'
#' @seealso
#' [aggregation_structure()] to make a price-index aggregation structure.
#' 
#' [split_classification()] to expand a classification by splitting along
#' a delimiter.
#'
#' @examples
#' # A simple classification structure
#' #            1
#' #      |-----+-----|
#' #      11          12
#' #  |---+---|       |
#' #  111     112     121
#'
#' expand_classification(c("111", "112", "121"))
#'
#' # Expanding more complex classifications
#' # ... if last 'digit' is either TA or TS
#'
#' expand_classification(
#'   c("111TA", "112TA", "121TS"),
#'   width = c(1, 1, 1, 2)
#' )
#'
#' # ... if first 'digit' is either 11 or 12
#'
#' expand_classification(c("111", "112", "121"), width = c(2, 1))
#'
#' # ...if there are delimiters in the classification (like COICOP)
#'
#' expand_classification(c("01.1.1", "01.1.2", "01.2.1"), width = 2)
#'
#' @export
expand_classification <- function(x, width = 1L) {
  x <- as.character(x)
  width <- as.integer(width)
  if (anyNA(width)) {
    stop("'width' cannot contain NAs")
  }
  if (any(width <= 0L)) {
    stop("'width' must be strictly positive")
  }

  if (length(width) == 1L) {
    longest <- max(nchar(x), 0L, na.rm = TRUE)
    width <- rep.int(width, ceiling(longest / width))
  }
  w <- cumsum(width)
  x <- strsplit(x, character(0L), fixed = TRUE)
  lapply(w, function(i) {
    vapply(x, \(x) paste(x[seq_len(i)], collapse = ""), character(1L))
  })
}

#' @rdname expand_classification
#' @export
interact_classifications <- function(..., sep = ":") {
  dots <- list(...)
  if (length(dots) == 0L) {
    return(list())
  }
  len <- unlist(lapply(dots, lengths), use.names = FALSE)
  n <- len[1L]
  atomics <- unlist(lapply(dots, \(x) lapply(x, is.atomic)), use.names = FALSE)
  if (any(len != n) || n == 0L || !all(atomics)) {
    stop(
      "each element in '...' must contain a list representing an ",
      "aggregation structure"
    )
  }
  interact <- function(x, y) {
    rapply(x, \(ix) lapply(y, \(iy) paste(ix, iy, sep = sep)), how = "replace")
  }
  res <- unlist(Reduce(interact, dots), use.names = FALSE)
  m <- length(dots[[length(dots)]])
  k <- m * n
  res <- unname(split(res, gl(length(res) / k, k)))
  lapply(res, \(x) unname(split(x, gl(m, n))))
}
