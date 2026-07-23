#' GEKS index
#'
#' Calculate a generalized inter-temporal GEKS price index over a rolling
#' window.
#'
#' @export
#'
#' @param index_formula `[function]` A function giving the index-number formula
#'   in the GEKS
#'   index. Usually a Törnqvist, Fisher (the default), or Walsh index.
#'   It must have arguments `p1`, `p0`, `q1`, and `q0`, and satisfy the
#'   time-reversal test. See `vignette("index-number-formulas")` for details.
#' @param order `[numeric(1)]` A finite number giving the order of the
#'   generalized mean used to
#'   average price indexes over the rolling window. The default uses a
#'   geometric mean.
#' @param price `[numeric > 0]` A numeric vector of prices, the same length
#'   as `quantity`.
#' @param quantity `[numeric >= 0]` A numeric vector of quantities, the same
#'   length as `price`.
#' @param period `[factor]` A factor, or something that can be coerced into one,
#'   that gives the corresponding time period for each element in `price` and
#'   `quantity`. The ordering of time periods follows the levels of `period`
#'   to agree with [`cut()`][cut.Date].
#' @param product `[factor]` A factor, or something that can be coerced into
#'   one, that
#'   gives the corresponding product identifier for each element in `price` and
#'   `quantity`.
#' @param window `[integer(1) > 0]` A positive integer giving the length of the
#'   rolling window.
#'   The default is a window that encompasses all periods in `period`.
#'   Non-integers are truncated towards zero.
#' @param n `[integer(1) > 0]` A positive integer giving the length of the index
#'   series for each
#'   window, starting from the end of the window. For example, if there are 13
#'   periods in `window`, setting `n = 1` gives the index for period 13. The
#'   default gives an index for each period in `window`. Non-integers are
#'   truncated towards zero.
#' @param match_method `[character(1)]` Either `"all"` to match all products
#'   against each other
#'   (the default) or `"back-price"` to match only back prices. The later can be
#'   faster when there is lots of product imbalanced.
#'
#' @returns
#' A list with a named numeric vector giving the value of the respective
#' period-over-period GEKS index for each window.
#'
#' @note
#' Like [back_period()], if multiple prices
#' correspond to a period-product pair, then the back price at a point in time
#' is always the first price for that product in the previous period. Unlike a
#' bilateral index, however, duplicated period-product pairs can have more
#' subtle implications for a multilateral index.
#'
#' @seealso
#' [`splice_index()`] to splice the rolling-window indexes together.
#'
#' `GEKSIndex()` in the \pkg{IndexNumR} package for an implementation of the
#' GEKS index with more options.
#'
#' The \pkg{rsmatrix} package for multilateral repeat-sales indexes.
#'
#' @references
#' Balk, B. M. (2008). *Price and Quantity Index Numbers*.
#' Cambridge University Press.
#'
#' IMF, ILO, Eurostat, UNECE, OECD, and World Bank. (2020).
#' *Consumer Price Index Manual: Concepts and Methods*.
#' International Monetary Fund.
#'
#' Ivancic, L., Diewert, W. E., and Fox, K. J. (2011). Scanner data, time
#' aggregation and the construction of price indexes.
#' *Journal of Econometrics*, 161(1): 24--35.
#'
#' @examples
#' price <- 1:10
#' quantity <- 10:1
#' period <- rep(1:5, 2)
#' product <- rep(letters[1:2], each = 5)
#'
#' cumprod(geks_index(price, quantity, period, product)[[1]])
#'
#' # Calculate the index over a rolling window.
#' (geks <- geks_index(price, quantity, period, product, window = 3))
#'
#' # Use a movement splice to combine the indexes in each window.
#' splice_index(geks, 2)
#'
#' # ... or use a mean splice.
#' splice_index(geks)
#'
#' # Make a Jevons GEKS index.
#' geks_index(
#'   price,
#'   quantity,
#'   period,
#'   product,
#'   index_formula = \(p1, p0, ...) gmean(p1 / p0, na.rm = TRUE, order = 0)
#' )
geks_index <- function(
  price,
  quantity,
  period,
  product,
  index_formula = \(p1, p0, q1, q0) {
    nested_gmean(p1 / p0, list(p0 * q0, p1 * q1), na.rm = TRUE)
  },
  window = nlevels(period),
  n = window - 1L,
  order = 0,
  match_method = c("all", "back-price")
) {
  period <- as.factor(period)
  product <- as.factor(product)
  attributes(product) <- NULL # faster to match on numeric codes

  if (different_length(price, quantity, period, product)) {
    stop(
      "`price`, `quantity`, `period`, and `product` must be the same length"
    )
  }

  match_method <- match.arg(match_method)

  if (nlevels(period) == 0L) {
    return(list())
  }

  window <- as.integer(window)
  if (window < 2L) {
    stop("`window` must be greater than or equal to 2")
  }
  if (window > nlevels(period)) {
    stop(
      "`window` must be less than or equal to the number of levels in",
      " `period`"
    )
  }

  n <- as.integer(n)
  if (n < 1L) {
    stop("`n` must be greater than or equal to 1")
  }
  if (n > window - 1L) {
    stop("`n` must be less than or equal to `window` minus 1")
  }

  mat <- geks_matrix(
    index_formula,
    price,
    quantity,
    period,
    product,
    window,
    n,
    match_method
  )
  rows <- seq_len(window) - 1L
  # Only the last n + 1 indexes in each window need to be kept.
  cols <- seq.int(window - n, window) - 1L
  res <- vector("list", nlevels(period) - window + 1L)
  # Move down the diagonal to make the geks index.
  for (i in seq_along(res)) {
    index <- apply(
      mat[rows + i, cols + i, drop = FALSE],
      2L,
      \(x) gmean(x, order = order, na.rm = TRUE)
    )
    res[[i]] <- index[-1L] / index[-length(index)]
  }
  res
}

#' Make the GEKS matrix
#' @noRd
geks_matrix <- function(
  index_formula,
  price,
  quantity,
  period,
  product,
  window,
  n,
  method
) {
  p <- split(price, period)
  q <- split(quantity, period)

  if (method == "all") {
    product <- balance_products(product, period)
    p <- Map(`[`, p, product)
    q <- Map(`[`, q, product)
  } else {
    product <- split(product, period)
  }

  lt <- vector("list", nlevels(period))
  for (i in seq_along(lt)) {
    if (i < max(window - n, 2L)) {
      # Only the last n + 1 rows are needed for each window,
      # so pad the top rows left of the diagonal with NA.
      ans <- rep_len(NA_real_, i - 1L)
    } else {
      # Matching is only done for the lower-triangular part of the matrix.
      # Match products for window - 1 periods left of the diagonal
      # to minimize the number of back prices to find.
      js <- seq.int(to = i - 1L, length.out = min(window, i) - 1L)
      if (method == "all") {
        ans <- Map(index_formula, p1 = p[js], p0 = p[i], q1 = q[js], q0 = q[i])
      } else {
        m <- Map(match, product[js], product[i])
        bp <- Map(`[`, p[i], m)
        bq <- Map(`[`, q[i], m)
        ans <- Map(index_formula, p1 = p[js], p0 = bp, q1 = q[js], q0 = bq)
      }
    }
    # Add the diagonal at the end and pad with NAs.
    ans <- c(
      unlist(ans, use.names = FALSE),
      index_formula(p[[i]], p[[i]], q[[i]], q[[i]])
    )
    front_pad <- rep_len(NA_real_, max(i - window, 0L))
    back_pad <- rep_len(NA_real_, length(lt) - length(ans) - length(front_pad))
    lt[[i]] <- c(front_pad, ans, back_pad)
  }
  res <- do.call(rbind, lt)
  rownames(res) <- colnames(res) <- names(p) # time periods
  # Exploit time reversal.
  ut <- upper.tri(res)
  res[ut] <- 1 / t(res)[ut]
  res
}
