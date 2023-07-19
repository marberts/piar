\name{impute_prices}
\alias{carry_forward}
\alias{shadow_price}

\title{Impute missing prices}

\description{
Impute missing prices using the carry forward or shadow price method.
}

\usage{
carry_forward(x, period, product)

shadow_price(x, period, product, ea, pias = NULL, w = NULL, r1 = 0, r2 = 1)
}

\arguments{
\item{x}{A numeric vector of prices.}

\item{period}{A factor, or something that can be coerced into one, giving the time period associated with each price in \code{x}. The ordering of time periods follows of the levels of \code{period}, to agree with \code{\link[=cut.Date]{cut()}}.}

\item{product}{A factor, or something that can be coerced into one, giving the product associated with each price in \code{x}.}

\item{ea}{A factor, or something that can be coerced into one, giving the elemental aggregate associated with each price in \code{x}.}

\item{pias}{A price index aggregation structure, as made with \code{\link[=aggregation_structure]{aggregation_structure()}}. The default imputes from elemental indexes only (i.e., not recursively).}

\item{w}{A numeric vector of weights for the prices in \code{x} (i.e., product weights). The default is to give each price equal weight.}

\item{r1}{Order of the price index used to calculate the elemental price indexes: 0 for a geometric index (the default), 1 for an arithmetic index, or -1 for a harmonic index. Other values are possible; see \code{\link[=generalized_mean]{generalized_mean()}} for details.}

\item{r2}{Order of the price index used to aggregate the elemental price indexes: 0 for a geometric index, 1 for an arithmetic index (the default), or -1 for a harmonic index. Other values are possible; see \code{\link[=generalized_mean]{generalized_mean()}} for details.}
}

\details{
The carry forward method replaces a missing price for a product by the price for the same product in the previous period. It tends to push an index value towards 1, and is usually avoided; see paragraph 6.61 in the CPI manual (2020).

The shadow price method recursively imputes a missing price by the value of the price for the same product in the previous period multiplied by the value of the period-over-period elemental index for the elemental aggregate to which that product belongs. This requires computing and aggregating an index (according to \code{pias}, unless \code{pias} is not supplied) for each \code{period}, and so these imputations can take a while. The index values used to do the imputations are not returned because the index needs to be recalculated to get correct percent-change contributions.

Shadow price imputation is referred to as self-correcting overall mean imputation in chapter 6 of the CPI manual (2020). It is identical to simply excluding missing price relatives in the index calculation, except in the period that a missing product returns. For this reason care is needed when using this method. It is sensitive to the assumption that a product does not change over time, and in some cases it is safer to simply omit the missing price relatives instead of imputing the missing prices.
}

\value{
A copy of \code{x} with missing values replaced (where possible).
}

\references{
ILO, IMF, OECD, Eurostat, UN, and World Bank. (2020). \emph{Consumer Price Index Manual: Theory and Practice}. International Monetary Fund.
}

\seealso{
\code{\link{price_relative}} for making price relatives for the same products over time.

\code{\link{elemental_index}} for making elemental price indexes.
}

\examples{
prices <- data.frame(price = c(1:7, NA), period = rep(1:2, each = 4), 
                     product = 1:4, ea = rep(letters[1:2], 4))
                     
with(prices, carry_forward(price, period, product))

with(prices, shadow_price(price, period, product, ea))
}