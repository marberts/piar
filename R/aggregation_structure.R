#' Make a price index aggregation structure
#'
#' Create a price index aggregation structure from a hierarchical
#' classification and aggregation weights that can be used to aggregate
#' elemental indexes.
#'
#' @aliases piar_aggregation_structure
#' @param x A list of character vectors that give the codes/labels for each
#'   level of the classification, ordered so that moving down the list goes down
#'   the hierarchy. The last vector gives the elemental aggregates, which should
#'   have no duplicates. All vectors should be the same length, without
#'   `NA`s, and there should be no duplicates across different levels of
#'   `x`. Names for `x` are used as level names; otherwise, levels are named
#'   level1, level2, ..., ea.
#' @param weights A numeric vector of aggregation weights for the elemental
#'   aggregates (i.e., the last vector in `x`), or something that can be coerced
#'   into one. The default is to give each elemental aggregate the same weight.
#'
#' @returns
#' A price index aggregation structure of class `piar_aggregation_structure`.
#' This is a list-S3 class with the following components.
#'
#' \item{child}{A nested list that gives the positions of the immediate
#' children for each node in each level of the aggregation structure above the
#' terminal nodes.}
#' \item{parent}{A list that gives the position of the
#' immediate parent for each node of the aggregation structure below the
#' initial nodes.}
#' \item{levels}{A named list of character vectors that give the levels of `x`.}
#' \item{weights}{A vector giving the weight for each elemental
#' aggregate.}
#'
#' @section Warning: The `aggregation_structure()` function does its best
#' to check its arguments, but there should be no expectation that the result
#' of `aggregation_structure()` will make any sense if `x` does not
#' represent a nested hierarchy.
#'
#' @seealso
#' [`aggregate()`][aggregate.piar_index] to aggregate price indexes made
#' with [`elemental_index()`][elemental_index].
#'
#' [expand_classification()] to make `x` from a character
#' representation of a hierarchical aggregation structure.
#'
#' [as_aggregation_structure()] to coerce tabular data into an
#' aggregation structure.
#'
#' [`as.data.frame()`][as.data.frame.piar_aggregation_structure] and
#' [`as.matrix()`][as.matrix.piar_aggregation_structure] to coerce an
#' aggregation structure into a tabular form.
#'
#' [`weights()`][weights.piar_aggregation_structure] to get the
#' weights for an aggregation structure.
#'
#' [`update()`][update.piar_aggregation_structure] for updating a
#' price index aggregation structure with an aggregated index.
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
#' aggregation_structure(
#'   aggregation_weights[1:3],
#'   weights = aggregation_weights[[4]]
#' )
#'
#' # The aggregation structure can also be made by expanding the
#' # elemental aggregates
#'
#' with(
#'   aggregation_weights,
#'   aggregation_structure(expand_classification(ea), weight)
#' )
#'
#' @export
aggregation_structure <- function(x, weights = NULL) {
  x <- lapply(x, \(z) factor(z, unique(z)))
  len <- length(x)
  ea <- as.character(unlist(x[len], use.names = FALSE))
  if (length(ea) == 0L) {
    stop("cannot make an aggregation structure with no elemental aggregates")
  }
  if (anyNA(x, recursive = TRUE)) {
    stop("'x' cannot contain NAs")
  }

  if (is.null(weights)) {
    weights <- rep.int(1, length(ea))
  } else {
    weights <- as.numeric(weights)
    if (any(weights <= 0, na.rm = TRUE)) {
      warning("some elements of 'w' are less than or equal to 0")
    }
  }

  # Basic argument checking to make sure inputs can make an
  # aggregation structure.
  if (any(lengths(x) != length(weights))) {
    stop("all arguments must be the same length")
  }
  if (anyDuplicated(ea)) {
    stop(
      "there are duplicated elemental aggregates; the last vector in 'x' ",
      "should not have duplicates"
    )
  }
  if (anyDuplicated(unlist(lapply(x, unique), use.names = FALSE))) {
    stop(
      "there are duplicated nodes in the aggregation structure; the same ",
      "value cannot appear across multiple levels of 'x'"
    )
  }
  upper <- x[-len] # nodes above eas
  lower <- x[-1L] # nodes below initial nodes
  child <- parent <- vector("list", len)[-1L]
  # Produce a list for each level with all the parent and child nodes.
  for (i in seq_along(upper)) {
    child[[i]] <- lapply(
      split(as.integer(lower[[len - i]]), upper[[len - i]]), unique
    )
    parent[[i]] <- lapply(
      split(as.integer(upper[[len - i]]), lower[[len - i]]), unique
    )
  }
  if (any(lengths(unlist(parent, recursive = FALSE)) > 1L)) {
    stop(
      "some nodes in the price index aggregation structure have ",
      "multiple parent nodes; the aggregation structure does not ",
      "represent a nested hierarchy"
    )
  }
  parent <- lapply(parent, unlist)
  levels <- lapply(x, levels)
  piar_aggregation_structure(child, parent, levels, weights)
}
