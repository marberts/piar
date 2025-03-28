% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/impute_prices.R
\name{impute_prices}
\alias{impute_prices}
\alias{shadow_price}
\alias{shadow_price.default}
\alias{shadow_price.data.frame}
\alias{carry_forward}
\alias{carry_forward.default}
\alias{carry_forward.data.frame}
\alias{carry_backward}
\alias{carry_backward.default}
\alias{carry_backward.data.frame}
\title{Impute missing prices}
\usage{
shadow_price(x, ...)

\method{shadow_price}{default}(
  x,
  ...,
  period,
  product,
  ea,
  pias = NULL,
  weights = NULL,
  r1 = 0,
  r2 = 1
)

\method{shadow_price}{data.frame}(x, formula, ..., weights = NULL)

carry_forward(x, ...)

\method{carry_forward}{default}(x, ..., period, product)

\method{carry_forward}{data.frame}(x, formula, ...)

carry_backward(x, ...)

\method{carry_backward}{default}(x, ..., period, product)

\method{carry_backward}{data.frame}(x, formula, ...)
}
\arguments{
\item{x}{Either a numeric vector (or something that can be coerced into one)
or data frame of prices.}

\item{...}{Further arguments passed to or used by methods.}

\item{period}{A factor, or something that can be coerced into one, giving
the time period associated with each price in \code{x}. The ordering of time
periods follows of the levels of \code{period}, to agree with
\code{\link[=cut.Date]{cut()}}.}

\item{product}{A factor, or something that can be coerced into one, giving
the product associated with each price in \code{x}.}

\item{ea}{A factor, or something that can be coerced into one, giving the
elemental aggregate associated with each price in \code{x}.}

\item{pias}{A price index aggregation structure, or something that can be
coerced into one, as made with \code{\link[=aggregation_structure]{aggregation_structure()}}. The default
imputes from elemental indexes only (i.e., not recursively).}

\item{weights}{A numeric vector of weights for the prices in \code{x} (i.e.,
product weights), or something that can be coerced into one. The default is
to give each price equal weight. This is evaluated in \code{x} for the data
frame method.}

\item{r1}{Order of the generalized-mean price index used to calculate the
elemental price indexes: 0 for a geometric index (the default), 1 for an
arithmetic index, or -1 for a harmonic index. Other values are possible;
see \code{\link[gpindex:generalized_mean]{gpindex::generalized_mean()}} for details.}

\item{r2}{Order of the generalized-mean price index used to aggregate the
elemental price indexes: 0 for a geometric index, 1 for an arithmetic index
(the default), or -1 for a harmonic index. Other values are possible; see
\code{\link[gpindex:generalized_mean]{gpindex::generalized_mean()}} for details.}

\item{formula}{A two-sided formula with prices on the left-hand
side. For \code{carry_forward()} and \code{carry_backward()}, the right-hand side
should have time periods and products (in that order); for
\code{shadow_price()}, the right-hand side should have time period, products,
and elemental aggregates (in that order).}
}
\value{
A numeric vector of prices with missing values replaced (where possible).
}
\description{
Impute missing prices using the carry forward or shadow price method.
}
\details{
The carry forward method replaces a missing price for a product by the price
for the same product in the previous period. It tends to push an index value
towards 1, and is usually avoided; see paragraph 6.61 in the CPI manual
(2020). The carry backwards method does the opposite, but this is rarely
used in practice.

The shadow price method recursively imputes a missing price by the value of
the price for the same product in the previous period multiplied by the
value of the period-over-period elemental index for the elemental aggregate
to which that product belongs. This requires computing and aggregating an
index (according to \code{pias}, unless \code{pias} is not supplied) for
each \code{period}, and so these imputations can take a while. The index
values used to do the imputations are not returned because the index needs
to be recalculated to get correct percent-change contributions.

Shadow price imputation is referred to as self-correcting overall mean
imputation in chapter 6 of the CPI manual (2020). It is identical to simply
excluding missing price relatives in the index calculation, except in the
period that a missing product returns. For this reason care is needed when
using this method. It is sensitive to the assumption that a product does not
change over time, and in some cases it is safer to simply omit the missing
price relatives instead of imputing the missing prices.
}
\examples{
prices <- data.frame(
  price = c(1:7, NA),
  period = rep(1:2, each = 4),
  product = 1:4,
  ea = rep(letters[1:2], 4)
)

carry_forward(prices, price ~ period + product)

shadow_price(prices, price ~ period + product + ea)

}
\references{
IMF, ILO, OECD, Eurostat, UNECE, and World Bank. (2020).
\emph{Consumer Price Index Manual: Concepts and Methods}.
International Monetary Fund.
}
\seealso{
\code{\link[=price_relative]{price_relative()}} for making price relatives for the
same products over time.
}
