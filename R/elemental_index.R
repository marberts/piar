#' Make elemental/elementary price indexes
#'
#' Compute period-over-period (chainable) or fixed-base (direct) elemental
#' price indexes, with optional percent-change contributions for each
#' product.
#'
#' When supplied with a numeric vector, `elemental_index()` is a simple
#' wrapper that applies
#' [`gpindex::generalized_mean(r)()`][gpindex::generalized_mean] and
#' [`gpindex::contributions(r)()`][gpindex::contributions] (if `contrib = TRUE`)
#' to `x` and `weights` grouped by `ea` and `period`. That
#' is, for every combination of elemental aggregate and time period,
#' `elemental_index()` calculates an index based on a generalized mean of
#' order `r` and, optionally, percent-change contributions. Product names should
#' be unique within each time period when making contributions, and, if not, are
#' passed to [make.unique()] with a warning. The default
#' (\code{r = 0} and no weights) makes Jevons elemental indexes. See chapter 8
#' (pp. 175--190) of the CPI manual (2020) for more detail about making
#' elemental indexes, or chapter 9 of the PPI manual (2004), and chapter 5 of
#' Balk (2008).
#'
#' The default method simply coerces `x` to a numeric vector prior to
#' calling the method above. The data frame method provides a formula interface
#' to specify columns of price relatives, time periods, and elemental
#' aggregates and call the method above.
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
#' `elementary_index()` is an alias for `elemental_index()` as this is more
#' common in the literature.
#'
#' @param x Period-over-period or fixed-base price relatives. Currently there
#' are methods for numeric vectors (which can be made with
#' [price_relative()]) and data frames.
#' @param period A factor, or something that can be coerced into one, giving
#' the time period associated with each price relative in `x`. The
#' ordering of time periods follows of the levels of `period`, to agree
#' with [`cut()`][cut.Date]. The default makes an index for one time period.
#' @param ea A factor, or something that can be coerced into one, giving the
#' elemental aggregate associated with each price relative in `x`. The
#' default makes an index for one elemental aggregate.
#' @param weights A numeric vector of weights for the price relatives in `x`,
#' or something that can be coerced into one. The default is equal weights.
#' This is evaluated in `x` for the data frame method.
#' @param product A character vector of product names, or something that can
#' be coerced into one, for each price relative in `x` when making
#' percent-change contributions. The default uses the names of `x`, if any;
#' otherwise, elements of `x` are given sequential names within each elemental
#' aggregate. This is evaluated in `x` for the data frame method.
#' @param contrib Should percent-change contributions be calculated? The
#' default does not calculate contributions.
#' @param chainable Are the price relatives in `x` period-over-period
#' relatives that are suitable for a chained calculation (the default)? This
#' should be `FALSE` when `x` contains fixed-base relatives.
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
#' @param formula A two-sided formula with price relatives on the left-hand
#' side, and time periods and elemental aggregates (in that order) on the
#' right-hand side.
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
#' [`aggregate()`][aggregate.piar_index] to aggregate elemental indexes
#' according to an aggregation structure.
#'
#' [`as.matrix()`][as.matrix.piar_index] and
#' [`as.data.frame()`][as.data.frame.piar_index] for coercing an index
#' into a tabular form.
#'
#' @references
#' Balk, B. M. (2008). *Price and Quantity Index Numbers*.
#' Cambridge University Press.
#' 
#' ILO, IMF, UNECE, OECD, and World Bank. (2004).
#' *Producer Price Index Manual: Theory and Practice*.
#' International Monetary Fund.
#'
#' IMF, ILO, OECD, Eurostat, UNECE, and World Bank. (2020).
#' *Consumer Price Index Manual: Concepts and Methods*.
#' International Monetary Fund.
#' 
#' von der Lippe, P. (2007). *Index Theory and Price Statistics*. Peter Lang.
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
#' elemental_index(prices, rel ~ period + ea)
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
#' elemental_index(
#'   prices,
#'   rel ~ period + ea,
#'   weights = fw(rel, group = interaction(period, ea)),
#'   r = 1
#' )
#'
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
                                    ...,
                                    period = gl(1, length(x)),
                                    ea = gl(1, length(x)),
                                    weights = NULL,
                                    product = NULL,
                                    chainable = TRUE,
                                    na.rm = FALSE,
                                    contrib = FALSE,
                                    r = 0) {
  chkDots(...)
  if (!is.null(weights)) {
    weights <- as.numeric(weights)
  }
  period <- as.factor(period)
  ea <- as.factor(ea) # ensures elemental aggregates are balanced
  
  time <- levels(period)
  levels <- levels(ea)
  
  if (different_length(x, period, ea, weights)) {
    stop("input vectors must be the same length")
  }
  if (any(x <= 0, na.rm = TRUE) || any(weights <= 0, na.rm = TRUE)) {
    warning("some elements of 'x' or 'weights' are less than or equal to 0")
  }
  
  if (contrib) {
    if (!is.null(product)) {
      names(x) <- as.character(product)
    }
    if (is.null(names(x))) {
      names(x) <- paste(ea, sequential_names(period, ea), sep = ".")
    } else {
      names(x) <- valid_product_names(names(x), period)
    }
  }
  # Splitting 'x' into a nested list by period then ea is the same as
  # using interaction(), but makes it easier to get the results as
  # a list.
  ea <- split(ea, period)
  x <- Map(split, split(x, period), ea)
  if (is.null(weights)) {
    weights <- list(list(NULL))
  } else {
    weights <- Map(split, split(weights, period), ea)
  }
  
  index_fun <- Vectorize(gpindex::generalized_mean(r), USE.NAMES = FALSE)
  contrib_fun <- Vectorize(
    gpindex::contributions(r),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )
  
  index <- Map(index_fun, x, weights, na.rm = na.rm, USE.NAMES = FALSE)
  if (contrib) {
    contributions <- Map(contrib_fun, x, weights, USE.NAMES = FALSE)
  } else {
    # Mimic contributions structure instead of a NULL.
    contributions <- contrib_skeleton(levels, time)
  }
  
  piar_index(index, contributions, levels, time, chainable)
}


#' @rdname elemental_index
#' @export
elemental_index.data.frame <- function(x,
                                       formula,
                                       ...,
                                       weights = NULL,
                                       product = NULL) {
  vars <- formula_vars(formula, x)
  weights <- eval(substitute(weights), x, parent.frame())
  product <- eval(substitute(product), x, parent.frame())
  
  elemental_index(
    vars[[1L]],
    period = vars[[2L]],
    ea = vars[[3L]],
    weights = weights,
    product = product,
    ...
  )
}

#' @rdname elemental_index
#' @export
elementary_index <- elemental_index
