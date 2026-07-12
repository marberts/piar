#' Scale weights
#'
#' Scale a vector of weights so that they sum to 1.
#'
#' @inheritParams gmean
#'
#' @returns
#' A numeric vector that sums to 1. If there are `NA`s in `x` then the result
#' sums 1 to if these values are removed.
#'
#' @examples
#' scale_weights(1:5)
#'
#' scale_weights(c(1:5, NA))
#'
#' @family math functions
#' @export
scale_weights <- function(x) {
  x / sum(x, na.rm = TRUE)
}
