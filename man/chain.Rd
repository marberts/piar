\name{chain}
\alias{chain}
\alias{chain.default}
\alias{chain.ind}
\alias{unchain}
\alias{unchain.default}
\alias{unchain.ind}
\alias{cumprod.ind}
\alias{is_chain_index}

\title{
Chain a price index
}

\description{
Chain a period-over-period index by taking the cumulative product of its values to turn it into a fixed-base (direct) index. Unchain a fixed-base index by dividing its values for successive periods to get a period-over-period index.
}

\usage{
chain(x, ...)

\method{chain}{default}(x, link = rep(1, length(levels(x))), ...)

\method{chain}{ind}(x, link = rep(1, length(levels(x))), ...)

\method{cumprod}{ind}(x) # deprecated

unchain(x, ...)

\method{unchain}{default}(x, ...)

\method{unchain}{ind}(x, ...)

is_chain_index(x)
}

\arguments{
\item{x}{A price index, as made by, e.g., \code{\link[=elemental_index]{elemental_index()}}.}

\item{link}{A numeric vector of link values for each level in \code{x}. The default is a vector of 1s so that no linking is done.}

\item{...}{Further arguments passed to or used by methods.}
}

\details{
The default methods attempts to coerce \code{x} into an index with \code{\link[=as_index]{as_index()}} prior to chaining/unchaining.

Chaining an index takes the cumulative product of the index values for each level, and returns the same type of index. This is roughly the same as \code{t(apply(as.matrix(x), 1, cumprod)) * link}. Unchaining does the opposite, so these are inverse operations. Quote contributions are removed when chaining/unchaining an index, as it's not usually possible to chain them. Note that unchaining a period-over-period index (i.e., when \code{is_chain_index(x) == TRUE}) does nothing, as does chaining a fixed-base index (i.e., when \code{is_chain_index(x) == FALSE}).
}

\value{
Each of these functions returns an index of the same type as \code{x}, except \code{is_chain_index} which returns \code{TRUE} when \code{x} is a period-over-period index (so that chaining makes sense).
}

\note{
The \code{cumprod()} method is deprecated, and just calls \code{chain.ind()}. It will be removed in a future release.
}

\seealso{
\code{\link{elemental_index}} for making elemental price indexes.
}

\examples{
prices <- data.frame(rel = 1:8, period = rep(1:2, each = 4), ea = rep(letters[1:2], 4))

# A simple period-over-period elemental index

(epr <- with(prices, elemental_index(rel, period, ea)))

# Make period 0 the fixed base period

chain(epr)

# Chaining and unchaining reverse each other

all.equal(epr, unchain(chain(epr)))

# It is easy to rebase an index because columns are time periods,
# but note the loss of information about period 0

as.matrix(chain(epr)) / as.matrix(chain(epr))[, 2]
}
