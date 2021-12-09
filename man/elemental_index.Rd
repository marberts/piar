\name{elemental_index}
\alias{elemental_index}
\alias{superlative_elemental_index}
\alias{as_elemental_index}
\alias{as_elemental_index.default}
\alias{as_elemental_index.matrix}
\alias{as_elemental_index.aggregate}

\title{Elemental index}

\description{
Compute period-over-period or fixed-base elemental price indexes (with optional quote contributions), or coerce pre-computed index values to an elemental index object.
}

\usage{
elemental_index(rel, period = rep(1, length(rel)), ea = rep(1, length(rel)), 
                w, contrib = FALSE, chained = TRUE, na.rm = FALSE, r = 0)
                
superlative_elemental_index(rel, period = rep(1, length(rel)), ea = rep(1, length(rel)), 
                            w1, w2, contrib = FALSE, chained = TRUE, na.rm = FALSE, s = 2)
                            
as_elemental_index(x, ...)

\method{as_elemental_index}{default}(x, ...)

\method{as_elemental_index}{matrix}(x, chained = TRUE, ...)

\method{as_elemental_index}{aggregate}(x, ...)
}

\arguments{
\item{rel}{A numeric vector of period-over-period or fixed-base price relatives. The former can be made with \code{\link[=price_relative]{price_relative()}}. Names for this vector are used as product names for quote contributions.}

\item{period}{A factor, or something that can be coerced into one, giving the time period associated with each price relative in \code{rel}. The ordering of time periods follows of the levels of \code{period}, to agree with \code{\link[=cut.Date]{cut()}}. The default assumes that all price relatives belong to one time period.}

\item{ea}{A factor, or something that can be coerced into one, giving the elemental aggregate associated with each price relative in \code{rel}. The default assumes that all price relatives belong to one elemental aggregate.}

\item{w, w1, w2}{A numeric vector of weights for the price relatives in \code{rel} (i.e., quote/product weights). The default is to give each price relative equal weight.}

\item{contrib}{Should quote contributions be calculated? The default does not calculate contributions, as this can take quite a bit longer.}

\item{chained}{Are the price relatives in \code{rel} period-over-period relatives for a chained calculation (the default)? This should be \code{FALSE} when \code{rel} are fixed-base relatives.}

\item{na.rm}{Should missing values in \code{rel} and \code{w} be removed? By default, missing values are not removed. Setting \code{na.rm = TRUE} is equivalent to parental imputation.}

\item{r}{Order of the generalized mean used to calculate the elemental price indexes: 0 for a geometric index (the default), 1 for an arithmetic index, or -1 for a harmonic index. Other values are possible; see \code{\link[=generalized_mean]{generalized_mean()}} for details.}

\item{s}{Order of the superlative quadratic mean elemental price indexes. The default (\code{s = 2}) makes a Fisher index. Other values are possible; see \code{\link[=nested_mean]{nested_mean()}} for details.}

\item{x}{An object to coerce into an elemental index.}

\item{...}{Further arguments passed to or used by methods.}
}

\details{
\code{elemental_index()} is a simple wrapper that applies \code{\link[=generalized_mean]{generalized_mean(r)}} and \code{\link[=contributions]{contributions(r)}} (if \code{contrib = TRUE}) to \code{rel} and \code{w} grouped by \code{ea} and \code{period}. That is, for every combination of elemental aggregate and time period, \code{elemental_index()} calculates an index based on a generalized mean of order \code{r} and, optionally, quote contributions. The default (\code{r = 0} and no weights) makes Jevons elemental indexes. See chapter 8 (pp. 175--190) of the CPI manual (2020) for more detail about making elemental indexes, and chapter 5 of Balk (2008).

\code{superlative_elemental_index()} works the same, except that it wraps \code{\link[=nested_mean]{nested_mean(0, c(s, -s) / 2)}} and \code{\link[=nested_contributions]{nested_contributions(0, c(s, -s) / 2)}} (if \code{contrib = TRUE}) to compute a superlative quadratic mean index of order \code{s}. The default (\code{s = 2} and no weights) makes CSWD elemental indexes.

The interpretation of the index depends on how the price relatives in \code{rel} are made. If these are period-over-period relatives, then the result is a collection of period-over-period elemental indexes; if these are fixed-base relatives, then the result is a collection of fixed-base elemental indexes. For the latter, \code{chained} should be \code{FALSE} so that no subsequent methods assume that a chained calculation should be used.

By default, missing price relatives in \code{rel} will propagate throughout the index calculation. Ignoring missing values with \code{na.rm = TRUE} is the same as parental (or overall mean) imputation, and needs to be explicitly set in the call to \code{elemental_index()} or \code{superlative_elemental_index()}. Explicit imputation of missing relatives, and especially imputation of missing prices, should be done prior to calling \code{elemental_index()} or \code{superlative_elemental_index()}.

In some cases \code{rel} are actually price indexes from elsewhere (e.g., a hedonic model), so there is one value for each period-elemental aggregate pair. In this case, \code{elemental_index()} will leave the values unaffected, and simply put them into a suitable form for, e.g., aggregation. But it may be easier to use \code{{as_elemental_index()}}.

Numeric matrices are coerce into an elemental index object by treating each column as a separate time period, and each row as an elemental aggregate. Column names are used to denote time periods, and row names are used to denote elemental aggregates (so they must be unique). If a dimension is unnamed, then it is given a sequential label from 1 to the size of that dimension. The default method coerces \code{x} to a matrix prior to using the matrix method. An aggregated index is coerced to an elemental index by simply extracting the elemental indexes (discarding the higher-level indexes).
}

\value{
Each of these functions return an object of class \code{elemental}, inheriting from class \code{index}, which has the following components.

\item{index}{A named list with an entry for each \code{period} that gives a named vector of index values for each level in \code{ea}.}
\item{contrib}{A named list with an entry for each \code{period}, which itself contains a named list with an entry for each level in \code{ea} with a named vector that gives the additive contribution for each price relative. If \code{contrib = FALSE}, then each of these vectors is of length 0.}
\item{levels}{The levels for \code{ea}.}
\item{time}{The levels for \code{period}.}
\item{has_contrib}{The value of \code{contrib}.}
\item{chained}{The value of \code{chained}.}
}

\references{
Balk, B. M. (2008). \emph{Price and Quantity Index Numbers}. Cambridge University Press.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2020). \emph{Consumer Price Index Manual: Theory and Practice}. International Monetary Fund.
}

\seealso{
\code{\link{price_relative}} for making price relatives for the same products over time, and \code{\link{carry_forward}} and \code{\link{shadow_price}} for imputations for missing prices.

\code{\link{aggregation_structure}} for making a price index aggregation structure.

\code{\link[=aggregate.index]{aggregate}} to aggregate elemental price indexes according to a \code{\link[=aggregation_structure]{pias}}.

\code{\link[=index_methods]{index methods}} for methods to extract index values/quote contributions, chain an index, and merge indexes together.
}

\examples{
prices <- data.frame(rel = 1:8, period = rep(1:2, each = 4), ea = rep(letters[1:2], 4))

# A simple elemental price structure using a Jevons index

epr <- with(prices, elemental_index(rel, period, ea))

# Extract the indexes like a matrix

epr["a", ]

epr[, 1:2]

# The index calculation can be done with a bit of matrix algebra
# But it's slower and more memory intensive

mm <- model.matrix(~ ea:factor(period) - 1, prices)
exp(solve(crossprod(mm), crossprod(mm, log(prices$rel))))

# Can also be done with tapply(), but it's less general

with(prices, t(tapply(rel, list(period, ea), gpindex::geometric_mean, na.rm = TRUE)))

epr

# Works with any kind of generalized price index, such as a Coggeshall

with(prices, elemental_index(rel, period, ea, r = -1))

# Make Fisher indexes

prices$base_weights <- 1:8
prices$current_weights <- 8:1

with(prices, superlative_elemental_index(rel, period, ea, base_weights, current_weights))

# Coerece to a matrix, and reverse this operation

epr2 <- as_elemental_index(as.matrix(epr))

all.equal(epr, epr2)
}