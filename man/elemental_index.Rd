\name{elemental index}
\alias{elemental_index}
\alias{superlative_elemental_index}

\title{Elemental price index}

\description{
Compute period-over-period elemental price indexes/relatives and, optionally, quote contributions.
}

\usage{
elemental_index(x, period = rep(1, length(x)), ea = rep(1, length(x)), 
                w = rep(1, length(x)), contrib = FALSE, na.rm = FALSE, r = 0)
                
superlative_elemental_index(x, period = rep(1, length(x)), ea = rep(1, length(x)), 
                            w1 = rep(1, length(x)), w2 = rep(1, length(x)),
                            contrib = FALSE, na.rm = FALSE, s = 2)
}

\arguments{
\item{x}{A numeric vector of period-over-period price relatives. This can be made with \code{\link[=price_relative]{price_relative()}}. Names for this vector are used as product names for quote contributions.}

\item{period}{A factor, or something that can be coerced into one, giving the time period associated with each price relative in \code{x}. The ordering of time periods follows of the levels of \code{period}, to agree with \code{\link[=cut.Date]{cut()}}. The default assumes that all price relatives belong to one time period.}

\item{ea}{A factor, or something that can be coerced into one, giving the elemental aggregate associated with each price relative in \code{x}. The default assumes that all price relatives belong to one elemental aggregate.}

\item{w, w1, w2}{A numeric vector of weights for the price relatives in \code{x} (i.e., quote/product weights). The default is to give each price relative equal weight.}

\item{contrib}{Should quote contributions be calculated? The default does not calculate contributions, as this can take quite a bit longer.}

\item{na.rm}{Should missing values in \code{x} and \code{w} be removed? By default, missing values are not removed. Setting to \code{TRUE} is equivalent to parental imputation.}

\item{r}{Order of the generalized mean used to calculate the elemental price indexes: 0 for a geometric index (the default), 1 for an arithmetic index, or -1 for a harmonic index. Other values are possible; see \code{\link[=generalized_mean]{generalized_mean()}} for details.}

\item{s}{Order of the superlative quadratic mean elemental price indexes. The default (\code{s = 2}) makes a Fisher index. Other values are possible; see \code{\link[=nested_mean]{nested_mean()}} for details.}
}

\details{
\code{elemental_index()} is a simple wrapper that applies \code{\link[=generalized_mean]{generalized_mean()}} and \code{\link[=contributions]{contributions()}} (if \code{contrib = TRUE}) to \code{x} and \code{w} grouped by \code{ea} and \code{period}. That is, for every combination of elemental aggregate and time period, \code{elemental_index()} calculates an index based on a generalized mean of order \code{r} and, optionally, quote contributions.

\code{superlative_elemental_index()} works the same, except that it wraps \code{\link[=nested_mean]{nested_mean()}} and \code{\link[=nested_contributions]{nested_contributions()}} (if \code{contrib = TRUE}) to compute a superlative quadratic mean index of order \code{s}.
}

\value{
\code{elemental_index()} and \code{superlative_elemental_index()} return an object of class \code{'elemental'}, inheriting from class \code{'index'}, which has the following components.

\item{index}{A named list with an entry for each \code{period} that gives a named vector of index values for each level in \code{ea}.}
\item{contributions}{A named list with an entry for each \code{period}, which itself contains a named list with an entry for each level in \code{ea} with a named vector that gives the additive contribution for each price relative. If \code{contrib = FALSE}, then each of these vectors is of length 0.}
\item{levels}{The levels for \code{ea}.}
\item{periods}{The levels for \code{period}.}
}

\note{
By default, missing price relatives in \code{x} will propagate throughout the index calculation. Ignoring missing values with \code{na.rm = TRUE} is the same as parental imputation, and needs to be explicitly set in the call to \code{elemental_index()} or \code{superlative_elemental_index()}. Non-parental imputation of missing relatives should be done prior to calling \code{elemental_index()} or \code{superlative_elemental_index()}.
}

\section{Warning}{
There is nothing stopping you from passing a vector fixed-base price relatives as \code{x} to get a fixed-base index, but you should only do this if you know what you're doing, and are very careful. Other functions (namely \link[=aggregate.index]{aggregate()}) assume that elemental indexes are calculated period-over-period, not with a fixed base period.
}

\seealso{
\code{\link{price_relative}} for making price relatives for the same products over time.

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
}