\name{shadow price}
\alias{shadow_price}

\title{Shadow price imputation}

\description{
Impute missing prices using the shadow price method.
}

\usage{
shadow_price(x, period, product, ea, pias = NULL,
             w = rep(1, length(x)), r1 = 0, r2 = 1)
}

\arguments{
\item{x}{A numeric vector of prices.}

\item{period}{A factor, or something that can be coerced into one, giving the time period associated with each price in \code{x}. The ordering of time periods follows of the levels of \code{period}, to agree with \code{\link[=cut.Date]{cut()}}.}

\item{product}{A factor, or something that can be coerced into one, giving the product associated with each price in \code{x}.}

\item{ea}{A factor, or something that can be coerced into one, giving the elemental aggregate associated with each price in \code{x}.}

\item{pias}{A price index aggregation structure. This can be made with \code{\link[=aggregation_structure]{aggregation_structure()}}.}

\item{w}{A numeric vector of weights for the prices in \code{x} (i.e., quote/product weights). The default is to give each price equal weight.}

\item{r1}{Order of the price index used to calculate the elemental price indexes: 0 for a geometric index (the default), 1 for an arithmetic index, or -1 for a harmonic index. Other values are possible; see \code{\link[=generalized_mean]{generalized_mean()}} for details.}

\item{r2}{Order of the price index used to aggregate the elemental price indexes: 0 for a geometric index, 1 for an arithmetic index (the default), or -1 for a harmonic index. Other values are possible; see \code{\link[=generalized_mean]{generalized_mean()}} for details.}
}

\details{
The shadow price method recursively imputes a missing price by the value of the price for the same product in the previous period multiplied by the value of the elemental index for the elemental aggregate to which that product belongs. This requires computing and aggregating an index (according to \code{pias}, unless \code{pias = NULL}) for each \code{period}, and so these imputations can take a while. The index values used to do the imputations are not returned because the index needs to be recalculated to get correct quote contributions.
}

\value{
A copy of \code{x} with missing values replaced with shadow prices (where possible).
}

\seealso{
\code{\link{price_relative}} for making price relatives for the same products over time.

\code{\link{aggregation_structure}} for making a price index aggregation structure.
}

\examples{
prices <- data.frame(price = c(1:7, NA), period = rep(1:2, each = 4), 
                     product = 1:4, ea = rep(letters[1:2], 4))

with(prices, shadow_price(price, period, product, ea))
}