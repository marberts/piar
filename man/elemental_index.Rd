\name{elemental_index}
\alias{elemental_index}
\alias{elemental_index.default}
\alias{elemental_index.numeric}

\title{Make elemental price indexes}

\description{
Compute period-over-period (chainable) or fixed-base (direct) elemental price indexes, with optional percent-change contributions.
}

\usage{
elemental_index(x, ...)

\method{elemental_index}{default}(x, ...)

\method{elemental_index}{numeric}(
  x,
  period = gl(1, length(x)), 
  ea = gl(1, length(x)),
  w = NULL,
  contrib = FALSE, 
  chainable = TRUE, 
  na.rm = FALSE, 
  r = 0, 
  ...
)
  
}

\arguments{
\item{x}{Period-over-period or fixed-base price relatives. Currently there is only a method for numeric vectors; these can be made with \code{\link[=price_relative]{price_relative()}}.}

\item{period}{A factor, or something that can be coerced into one, giving the time period associated with each price relative in \code{x}. The ordering of time periods follows of the levels of \code{period}, to agree with \code{\link[=cut.Date]{cut()}}. The default assumes that all price relatives belong to one time period.}

\item{ea}{A factor, or something that can be coerced into one, giving the elemental aggregate associated with each price relative in \code{x}. The default assumes that all price relatives belong to one elemental aggregate.}

\item{w}{A numeric vector of weights for the price relatives in \code{x}. The default is equal weights.}

\item{contrib}{Should percent-change contributions be calculated? The default does not calculate contributions.}

\item{chainable}{Are the price relatives in \code{x} period-over-period relatives for a chained calculation (the default)? This should be \code{FALSE} when \code{x} are fixed-base relatives.}

\item{na.rm}{Should missing values be removed? By default, missing values are not removed. Setting \code{na.rm = TRUE} is equivalent to overall mean imputation.}

\item{r}{Order of the generalized mean to aggregate price relatives. 0 for a geometric index (the default for making elemental indexes), 1 for an arithmetic index (the default for aggregating elemental indexes and averaging indexes over subperiods), or -1 for a harmonic index (usually for a Paasche index). Other values are possible; see \code{\link[=generalized_mean]{generalized_mean()}} for details.}

\item{...}{Further arguments passed to or used by methods.}
}

\details{
When supplied with a numeric vector, \code{elemental_index()} is a simple wrapper that applies \code{\link[=generalized_mean]{generalized_mean(r)}} and \code{\link[=contributions]{contributions(r)}} (if \code{contrib = TRUE}) to \code{x} and \code{w} grouped by \code{ea} and \code{period}. That is, for every combination of elemental aggregate and time period, \code{elemental_index()} calculates an index based on a generalized mean of order \code{r} and, optionally, percent-change contributions (using names for \code{x} as product names). The default (\code{r = 0} and no weights) makes Jevons elemental indexes. See chapter 8 (pp. 175--190) of the CPI manual (2020) for more detail about making elemental indexes, and chapter 5 of Balk (2008).

The default method simply coerces \code{x} to a numeric vector prior to calling the method above.

The interpretation of the index depends on how the price relatives in \code{x} are made. If these are period-over-period relatives, then the result is a collection of period-over-period (chainable) elemental indexes; if these are fixed-base relatives, then the result is a collection of fixed-base (direct) elemental indexes. For the latter, \code{chainable} should be set to \code{FALSE} so that no subsequent methods assume that a chained calculation should be used.

By default, missing price relatives in \code{x} will propagate throughout the index calculation. Ignoring missing values with \code{na.rm = TRUE} is the same as overall mean (parental) imputation, and needs to be explicitly set in the call to \code{elemental_index()}. Explicit imputation of missing relatives, and especially imputation of missing prices, should be done prior to calling \code{elemental_index()}.

Indexes based on nested generalized means, like the Fisher index (and superlative quadratic mean indexes more generally), can be calculated by supplying the appropriate weights with \code{\link[=nested_transmute]{nested_transmute()}}; see the example below. It is important to note that there are several ways to make these weights, and this affects how percent-change contributions are calculated.
}

\value{
A price index. This is an object that inherits from \code{abstract_index} with the following components.

\item{index}{A named list with an entry for each \code{period} that gives a named vector of index values for each level in \code{ea}.}
\item{contrib}{A named list with an entry for each \code{period}, which itself contains a named list with an entry for each level in \code{ea} with a named vector that gives the additive contribution for each price relative. If \code{contrib = FALSE}, then each of these vectors is of length 0.}
\item{levels}{The levels for \code{ea}.}
\item{time}{The levels for \code{period}.}


If \code{chainable == TRUE} then this is a period-over-period index that also inherits from \code{chainable_index}; otherwise, it is a fixed-based index that inherits from \code{direct_index}.
}

\references{
Balk, B. M. (2008). \emph{Price and Quantity Index Numbers}. Cambridge University Press.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2020). \emph{Consumer Price Index Manual: Theory and Practice}. International Monetary Fund.
}

\seealso{
\code{\link{price_relative}} for making price relatives for the same products over time, and \code{\link{carry_forward}} and \code{\link{shadow_price}} for imputation of missing prices.

\code{\link[=aggregate.abstract_index]{aggregate}} to aggregate elemental indexes according to a price index aggregation structure.

\code{\link{as_index}} to turn pre-computed index values into an index object.

\code{\link{chain}} for chaining period-over-period indexes, and \code{\link{rebase}} for rebasing an index.

\code{\link{contrib}} for getting the percent-change contributions of an index, \code{\link[=levels.abstract_index]{levels}} for getting the levels, and \code{\link[=time.abstract_index]{time}} for getting the time periods.

\code{\link[=merge.abstract_index]{merge}} and \code{\link[=stack.abstract_index]{stack}} to combine index values.

\code{\link[=[.abstract_index]{[}} and \code{\link[=[<-.abstract_index]{[<-}} to extract and replace index values.

\code{\link[=as.matrix.abstract_index]{as.matrix}} and \code{\link[=as.data.frame.abstract_index]{as.data.frame}} for coercing an index into a tabular form.
}

\examples{
library(gpindex)

prices <- data.frame(
  rel = 1:8,
  period = rep(1:2, each = 4),
  ea = rep(letters[1:2], 4)
)

# Calculate Jevons elemental indexes

(epr <- with(prices, elemental_index(rel, period, ea)))

# Same as using lm() or tapply()

exp(coef(lm(log(rel) ~ ea:factor(period) - 1, prices)))

with(
  prices,
  t(tapply(rel, list(period, ea), geometric_mean, na.rm = TRUE))
)

# A general function to calculate weights to turn the geometric
# mean of the arithmetic and harmonic mean (i.e., Fisher mean)
# into an arithmetic mean

fw <- grouped(nested_transmute(0, c(1, -1), 1))

# Calculate a CSWD index (same as the Jevons in this example) 
# as an arithmetic index by using the appropriate weights

with(
  prices, 
  elemental_index(
    rel, period, ea,
    fw(rel, group = interaction(period, ea)), r = 1
  )
)
}