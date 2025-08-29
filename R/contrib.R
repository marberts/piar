#' Extract percent-change contributions
#'
#' Extract a matrix or data frame of percent-change contributions from a price
#' index.
#'
#' @param x A price index, as made by, e.g., [elemental_index()].
#' @param level The level of an index for which percent-change contributions
#'   are desired, defaulting to the first level (usually the top-level for an
#'   aggregate index). `contrib2DF()` can accept multiple levels.
#' @param period The time periods for which percent-change contributions are
#'   desired, defaulting to all time periods.
#' @param pad A numeric value to pad contributions so that they fit into a
#'   rectangular array when products differ over time. The default is 0.
#' @param ... Further arguments passed to or used by methods.
#' @param value A numeric matrix of replacement contributions with a row for
#'   each product and a column for each time period. Recycling occurs along time
#'   periods.
#'
#' @returns
#' `contrib()` returns a matrix of percent-change contributions with a column
#' for each `period` and a row for each product (sorted) for which there are
#' contributions in `level`. Contributions are padded with `pad` to fit into a
#' rectangular array when products differ over time. The replacement methods
#' returns a copy of `x` with contributions given by the matrix `value`.
#' (`set_contrib()` is an alias that's easier to use with pipes.)
#' `set_contrib_from_index()` is a helper to return a copy of `x` with all
#' contributions set to the corresponding index value minus 1.
#'
#' `contrib2DF()` returns a data frame of contributions with four
#' columns: `period`, `level`, `product`, and `value`.
#'
#' @examples
#' prices <- data.frame(
#'   rel = 1:8,
#'   period = rep(1:2, each = 4),
#'   ea = rep(letters[1:2], 4)
#' )
#'
#' index <- elemental_index(prices, rel ~ period + ea, contrib = TRUE)
#'
#' pias <- aggregation_structure(
#'   list(c("top", "top", "top"), c("a", "b", "c")),
#'   weights = 1:3
#' )
#'
#' index <- aggregate(index, pias, na.rm = TRUE)
#'
#' # Percent-change contributions for the top-level index
#'
#' contrib(index)
#'
#' contrib2DF(index)
#'
#' # Calculate EA contributions for the chained index
#'
#' library(gpindex)
#'
#' arithmetic_contributions(
#'   as.matrix(chain(index))[c("a", "b", "c"), 2],
#'   weights(pias)
#' )
#'
#' @export contrib
#' @family index methods
contrib <- function(x, ...) {
  UseMethod("contrib")
}

#' @rdname contrib
#' @export
contrib.piar_index <- function(
  x,
  level = levels(x)[1L],
  period = time(x),
  ...,
  pad = 0
) {
  chkDots(...)
  level <- match_levels(as.character(level), x)
  period <- match_time(as.character(period), x, several = TRUE)
  pad <- as.numeric(pad)
  if (length(pad) != 1L) {
    stop("'pad' must be a length 1 numeric value")
  }
  con <- lapply(x$contrib[period], `[[`, level)

  con_names <- lapply(con, names)
  products <- sort.int(unique(unlist(con_names, use.names = FALSE)))

  out <- vector("list", length(con))
  names(out) <- x$time[period]

  # Initialize 0 contributions for all products in all time periods, then
  # replace with the actual values so products that didn't sell have 0 and
  # not NA contributions.
  out[] <- list(structure(rep.int(pad, length(products)), names = products))
  res <- do.call(cbind, Map(replace, out, con_names, con))
  names(dimnames(res)) <- c("product", "time")
  res
}

#' @rdname contrib
#' @export
contrib2DF <- function(x, ...) {
  UseMethod("contrib2DF")
}

#' @rdname contrib
#' @export
contrib2DF.piar_index <- function(
  x,
  level = levels(x)[1L],
  period = time(x),
  ...
) {
  chkDots(...)
  level <- match_levels(as.character(level), x, several = TRUE)
  period <- match_time(as.character(period), x, several = TRUE)

  con <- lapply(x$contrib[period], `[`, level)

  products <- lapply(con, lengths)

  levels <- x$levels[level]
  levels <- unlist(
    lapply(products, \(z) rep.int(levels, z)),
    use.names = FALSE
  )

  periods <- rep.int(x$time[period], vapply(products, sum, numeric(1L)))

  contributions <- unlist(con)
  data.frame(
    period = periods,
    level = levels,
    # NULL if there are no contributions.
    product = as.character(names(contributions)),
    value = unname(contributions)
  )
}

#' @rdname contrib
#' @export
`contrib<-` <- function(x, ..., value) {
  UseMethod("contrib<-")
}

#' @rdname contrib
#' @export
`contrib<-.piar_index` <- function(
  x,
  level = levels(x)[1L],
  period = time(x),
  ...,
  value
) {
  chkDots(...)
  level <- match_levels(as.character(level), x)
  period <- match_time(as.character(period), x, several = TRUE)

  value <- as.matrix(value)
  if (ncol(value) == 0L) {
    stop("replacement has length zero")
  } else if (length(period) %% ncol(value) != 0) {
    warning(
      "number of items to replace is not a multiple of replacement length"
    )
  }

  if (nrow(value) > 0L) {
    if (is.null(rownames(value))) {
      products <- as.character(seq_len(nrow(value)))
    } else {
      products <- valid_product_names(rownames(value))
    }
  } else {
    products <- NULL
  }

  j <- 0
  for (t in period) {
    j <- j %% ncol(value) + 1
    con <- as.numeric(value[, j])
    names(con) <- products
    x$contrib[[t]][level] <- list(con)
  }
  validate_piar_index(x)
}

#' @rdname contrib
#' @export
set_contrib <- `contrib<-`

#' @rdname contrib
#' @export
set_contrib_from_index <- function(x) {
  x$contrib <- index2contrib(x$index, x$levels, x$time)
  x
}
