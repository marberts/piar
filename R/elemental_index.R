#--- Helpers ----
duplicate_products <- function(x) {
  any(vapply(x, anyDuplicated, numeric(1L), incomparables = NA) > 0)
}

sequential_names <- function(...) {
  f <- interaction(...)
  unsplit(Map(seq_len, tabulate(f)), f)
}

valid_product_names <- function(x, period) {
  x <- as.character(x)
  period <- as.factor(period)
  if (anyNA(x) || any(x == "")) {
    stop("each product must have a non-missing name")
  }
  x <- split(x, period)
  if (duplicate_products(x)) {
    warning("product names are not unique in each time period")
  }
  unsplit(lapply(x, make.unique), period)
}

different_length <- function(...) {
  res <- lengths(Filter(Negate(is.null), list(...)))
  any(res != res[1L])
}

#' Make elemental price indexes
#'
#' Compute period-over-period (chainable) or fixed-base (direct) elemental
#' price indexes, with optional percent-change contributions.
#'
#' When supplied with a numeric vector, `elemental_index()` is a simple
#' wrapper that applies [`generalized_mean(r)()`][gpindex::generalized_mean]
#' and [`contributions(r)()`][gpindex::contributions] (if `contrib = TRUE`) to
#' `x` and `weights` grouped by `ea` and `period`. That
#' is, for every combination of elemental aggregate and time period,
#' `elemental_index()` calculates an index based on a generalized mean of
#' order `r` and, optionally, percent-change contributions. The default
#' (\code{r = 0} and no weights) makes Jevons elemental indexes. See chapter 8
#' (pp. 175--190) of the CPI manual (2020) for more detail about making
#' elemental indexes, and chapter 5 of Balk (2008).
#'
#' The default method simply coerces `x` to a numeric vector prior to
#' calling the method above.
#'
#' Names for `x` are used as product names when calculating percent-change
#' contributions. Product names should be unique within each time period, and,
#' if not, are passed to [make.unique()] with a
#' warning. If `x` has no names then elements of `x` are given
#' sequential names within each elemental aggregate.
#'
#' The interpretation of the index depends on how the price relatives in
#' `x` are made. If these are period-over-period relatives, then the
#' result is a collection of period-over-period (chainable) elemental indexes;
#' if these are fixed-base relatives, then the result is a collection of
#' fixed-base (direct) elemental indexes. For the latter, `chainable`
#' should be set to `FALSE` so that no subsequent methods assume that a
#' chained calculation should be used.
#'
#' By default, missing price relatives in `x` will propagate throughout
#' the index calculation. Ignoring missing values with `na.rm = TRUE` is
#' the same as overall mean (parental) imputation, and needs to be explicitly
#' set in the call to `elemental_index()`. Explicit imputation of missing
#' relatives, and especially imputation of missing prices, should be done prior
#' to calling `elemental_index()`.
#'
#' Indexes based on nested generalized means, like the Fisher index (and
#' superlative quadratic mean indexes more generally), can be calculated by
#' supplying the appropriate weights with [gpindex::nested_transmute()]; see the
#' example below. It is important to note that there are several ways to
#' make these weights, and this affects how percent-change contributions
#' are calculated.
#'
#' @param x Period-over-period or fixed-base price relatives. Currently there
#' is only a method for numeric vectors; these can be made with
#' [price_relative()].
#' @param period A factor, or something that can be coerced into one, giving
#' the time period associated with each price relative in `x`. The
#' ordering of time periods follows of the levels of `period`, to agree
#' with [`cut()`][cut.Date]. The default assumes that all price
#' relatives belong to one time period.
#' @param ea A factor, or something that can be coerced into one, giving the
#' elemental aggregate associated with each price relative in `x`. The
#' default assumes that all price relatives belong to one elemental aggregate.
#' @param weights A numeric vector of weights for the price relatives in `x`.
#' The default is equal weights.
#' @param contrib Should percent-change contributions be calculated? The
#' default does not calculate contributions.
#' @param chainable Are the price relatives in `x` period-over-period
#' relatives for a chained calculation (the default)? This should be
#' `FALSE` when `x` contains fixed-base relatives.
#' @param na.rm Should missing values be removed? By default, missing values
#' are not removed. Setting `na.rm = TRUE` is equivalent to overall mean
#' imputation.
#' @param r Order of the generalized mean to aggregate price relatives. 0 for a
#' geometric index (the default for making elemental indexes), 1 for an
#' arithmetic index (the default for aggregating elemental indexes and
#' averaging indexes over subperiods), or -1 for a harmonic index (usually for
#' a Paasche index). Other values are possible; see
#' [gpindex::generalized_mean()] for details.
#' @param ... Further arguments passed to or used by methods.
#'
#' @returns
#' A price index that inherits from [`piar_index`]. If
#' `chainable = TRUE` then this is a period-over-period index that also
#' inherits from [`chainable_piar_index`]; otherwise, it is a
#' fixed-based index that inherits from [`direct_piar_index`].
#'
#' @seealso
#' [price_relative()] for making price relatives for the same products over
#' time, and [carry_forward()] and [shadow_price()] for
#' imputation of missing prices.
#'
#' [as_index()] to turn pre-computed (elemental) index values into an
#' index object.
#'
#' [chain()] for chaining period-over-period indexes, and
#' [rebase()] for rebasing an index.
#'
#' [`as.matrix()`][as.matrix.piar_index] and
#' [`as.data.frame()`][as.data.frame.piar_index] for coercing an index
#' into a tabular form.
#'
#' @references
#' Balk, B. M. (2008). *Price and Quantity Index Numbers*.
#' Cambridge University Press.
#'
#' ILO, IMF, OECD, Eurostat, UN, and World Bank. (2020).
#' *Consumer Price Index Manual: Theory and Practice*.
#' International Monetary Fund.
#'
#' @examples
#' library(gpindex)
#'
#' prices <- data.frame(
#'   rel = 1:8,
#'   period = rep(1:2, each = 4),
#'   ea = rep(letters[1:2], 4)
#' )
#'
#' # Calculate Jevons elemental indexes
#'
#' (epr <- with(prices, elemental_index(rel, period, ea)))
#'
#' # Same as using lm() or tapply()
#'
#' exp(coef(lm(log(rel) ~ ea:factor(period) - 1, prices)))
#'
#' with(
#'   prices,
#'   t(tapply(rel, list(period, ea), geometric_mean, na.rm = TRUE))
#' )
#'
#' # A general function to calculate weights to turn the geometric
#' # mean of the arithmetic and harmonic mean (i.e., Fisher mean)
#' # into an arithmetic mean
#'
#' fw <- grouped(nested_transmute(0, c(1, -1), 1))
#'
#' # Calculate a CSWD index (same as the Jevons in this example)
#' # as an arithmetic index by using the appropriate weights
#'
#' with(
#'   prices,
#'   elemental_index(
#'     rel, period, ea,
#'     fw(rel, group = interaction(period, ea)),
#'     r = 1
#'   )
#' )
#'
#' @family index methods
#' @export
elemental_index <- function(x, ...) {
  UseMethod("elemental_index")
}

#' @rdname elemental_index
#' @export
elemental_index.default <- function(x, ...) {
  elemental_index(as.numeric(x), ...)
}

#' @rdname elemental_index
#' @export
elemental_index.numeric <- function(x,
                                    period = gl(1, length(x)),
                                    ea = gl(1, length(x)),
                                    weights = NULL,
                                    contrib = FALSE,
                                    chainable = TRUE,
                                    na.rm = FALSE,
                                    r = 0,
                                    ...) {
  if (different_length(x, period, ea, weights)) {
    stop("input vectors must be the same length")
  }
  if (any(x <= 0, na.rm = TRUE) || any(weights <= 0, na.rm = TRUE)) {
    warning("some elements of 'x or 'weights' are less than or equal to 0")
  }

  period <- as.factor(period)
  ea <- as.factor(ea) # ensures elemental aggregates are balanced
  time <- levels(period)
  levels <- levels(ea)

  if (contrib) {
    if (is.null(names(x))) {
      names(x) <- paste(ea, sequential_names(period, ea), sep = ".")
    } else {
      names(x) <- valid_product_names(names(x), period)
    }
  }
  # splitting 'x' into a nested list by period then ea is the same as
  # using interaction(), but makes it easier to get the results as
  # a list
  ea <- split(ea, period)
  x <- Map(split, split(x, period), ea)
  if (is.null(weights)) {
    weights <- list(list(NULL))
  } else {
    weights <- Map(split, split(as.numeric(weights), period), ea)
  }

  index_fun <- Vectorize(gpindex::generalized_mean(r), USE.NAMES = FALSE)
  contrib_fun <- Vectorize(gpindex::contributions(r),
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  )

  index <- Map(index_fun, x, weights, na.rm = na.rm, USE.NAMES = FALSE)
  if (contrib) {
    contributions <- Map(contrib_fun, x, weights, USE.NAMES = FALSE)
  } else {
    # mimic contributions structure instead of a NULL
    contributions <- contrib_skeleton(levels, time)
  }

  piar_index(index, contributions, levels, time, chainable)
}
