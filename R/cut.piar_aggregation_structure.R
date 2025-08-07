#' Cut an aggregation structure
#'
#' Keep only the part of an aggregation structure above or below a certain
#' level.
#'
#' @param x A price index aggregation structure, as made by
#' [aggregation_structure()].
#' @param ... Not currently used.
#' @param level A positive integer, or something that can be coerced into one,
#'   giving the level at which to cut `x`.
#' @param na.rm Should missing values be removed when aggregating the
#'   weights? By default, missing values are not removed.
#' @param upper Keep only the part of `x` above `level` (the default);
#'   otherwise, return the part of `x` below `level`.
#'
#' @returns A price index aggregation structure.
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
#' pias <- aggregation_structure(
#'   aggregation_weights[1:3],
#'   weights = aggregation_weights[[4]]
#' )
#'
#' # Turn it into
#' #            1
#' #      |-----+-----|
#' #      11          12
#' #     (4)         (4)
#'
#' cut(pias, 2)
#'
#' @family aggregation structure methods
#' @export
cut.piar_aggregation_structure <- function(
  x,
  level,
  ...,
  na.rm = FALSE,
  upper = TRUE
) {
  chkDots(...)
  level <- as.integer(level)
  n <- length(x$levels)
  if (level < 1L) {
    stop("'level' must be greater than or equal to 1")
  }
  if (level > n) {
    stop("'level' must be smaller than the number of levels in 'x'")
  }
  if ((!upper && level == 1L) || (upper && level == n)) {
    return(x)
  }

  i <- seq.int(to = n - 1L, length.out = level - 1L)
  if (upper) {
    weights <- weights(x, ea_only = FALSE, na.rm = na.rm)
    piar_aggregation_structure(
      x$child[i],
      x$parent[i],
      x$levels[seq_len(level)],
      weights[[level]]
    )
  } else {
    piar_aggregation_structure(
      x$child[-i],
      x$parent[-i],
      x$levels[-seq_len(level - 1L)],
      x$weights
    )
  }
}
