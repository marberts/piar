paste_until <- function(x, i) {
  paste(x[seq_len(i)], collapse = "")
}



#' Expand a hierarchical classification
#' 
#' Expand a character representation of a hierarchical classification to make a
#' price index aggregation structure.
#' 
#' 
#' @param x A character vector, or something that can be coerced into one, of
#' codes/labels for a specific level in a classification (e.g., 5-digit COICOP,
#' 5-digit NAICS, 4-digit SIC).
#' @param width An integer vector that gives the width of each digit in
#' `x`. A single value is recycled to span the longest element in
#' `x`. This cannot contain NAs. The default assumes each digit has a
#' width of 1, as in the NAICS, NAPCS, and SIC classifications.
#' @return A list with a entry for each level in `x` giving the "digits"
#' that represent each level in the hierarchy.
#' @seealso [aggregation_structure()] to make a price-index
#' aggregation structure.
#' @examples
#' 
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
#'   c("111TA", "112TA", "121TS"), width = c(1, 1, 1, 2)
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
#' @export expand_classification
expand_classification <- function(x, width = 1L) {
  x <- as.character(x)
  width <- as.integer(width)
  if (any(width <= 0)) {
    stop("'width' must be strictly positive")
  }
  if (anyNA(width)) {
    stop("'width' cannot contain NAs")
  }

  if (length(width) == 1L) {
    longest <- max(nchar(x), 0L, na.rm = TRUE)
    width <- rep.int(width, ceiling(longest / width))
  }
  w <- cumsum(width)
  x <- strsplit(x, character(0L), fixed = TRUE)
  lapply(w, function(i) {
    vapply(x, paste_until, character(1L), i)
  })
}
