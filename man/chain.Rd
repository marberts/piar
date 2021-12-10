\name{chain}
\alias{chain}
\alias{chain.default}
\alias{chain.ind}
\alias{unchain}
\alias{unchain.default}
\alias{unchain.ind}
\alias{cumprod.ind}

\title{
Chain an index
}

\description{
Chain a period-over-period index by taking the cumulative product its values to turn it into a fixed-base index. Unchain a fixed-base index by dividing its values for successive periods to get a period-over-period index.
}

\usage{
chain(x, ...)

\method{chain}{default}(x, ...)

\method{chain}{ind}(x, ...)

\method{cumprod}{ind}(x)

unchain(x, ...)

\method{chain}{default}(x, ...)

\method{unchain}{ind}(x, ...)
}

\arguments{
\item{x}{An object to chain/unchain.}

\item{...}{Further arguments passed to or used by methods.}
}

\details{
Chaining an index object takes the cumulative product of the values for each level, and returns the same type of index (elemental or aggregate). This is roughly the same as \code{t(apply(as.matrix(x), 1, cumprod))}. Unchaining does the opposite. Quote contributions are removed when chaining/unchaining an index as it's not usually possible to chain them. Note that unchaining a period-over-period index (i.e., \code{x$chain == TRUE}) does nothing, as does chaining a fixed-base index (i.e., \code{x$chain == FALSE}).

The default methods attempts to coerce \code{x} into an elemental index with \code{\link[=as_elemental_index]{as_elemental_index()}} prior to chaining/unchaining.
}

\note{
The \code{cumprod()} method is deprecated, and just calls \code{chain.ind()}.
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

# # It is easy to rebase an index because columns are time periods

as.matrix(chain(epr)) / as.matrix(chain(epr))[, 2]
}
