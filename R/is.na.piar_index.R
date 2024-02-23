#' Missing values in a price index
#' 
#' Identify and replace missing values in price index.
#' 
#' @param x A price index, as made by, e.g., [elemental_index()].
#' @param recursive Ignored.
#' @param value A numeric vector, or something that can be coerced to one, of
#' replacement values.
#' 
#' @returns
#' `is.na()` returns a logical matrix, with a row for each level of `x` and a
#' columns for each time period, that indicates which index values are missing.
#' The replacement method replaces these values with `value`.
#' 
#' `anyNA()` returns `TRUE` if any index values are missing.
#' 
#' @examples
#' index <- as_index(matrix(c(1, 2, 3, NA, 5, NA), 2))
#' 
#' anyNA(index)
#' is.na(index)
#' 
#' # Carry forward imputation
#' 
#' is.na(index) <- 1
#' index
#' 
#' @family index methods
#' @export
is.na.piar_index <- function(x) {
  is.na(as.matrix(x))
}

#' @rdname is.na.piar_index
#' @export
anyNA.piar_index <- function(x, recursive) {
  anyNA(as.matrix(x), recursive = FALSE)
}

#' @rdname is.na.piar_index
#' @export
`is.na<-.piar_index` <- function(x, value) {
  res <- as.matrix(x)
  nas <- which(is.na(res), arr.ind = TRUE)
  if (length(nas) == 0L) {
    return(x)
  }
  levels <- dim_indices(x$levels, nas[, 1L])
  periods <- dim_indices(x$time, nas[, 2L])
  res[cbind(levels, periods)] <- as.numeric(value)
  # only loop over periods that have a value replaced
  for (i in seq_along(levels)) {
    x$index[[periods[i]]][levels[i]] <- res[levels[i], periods[i]]
    # drop contributions for replaced values
    x$contrib[[periods[i]]][levels[i]] <- list(numeric(0L))
  }
  validate_piar_index(x)
}
