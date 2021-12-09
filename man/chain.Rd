\name{chain}
\alias{chain}
\alias{chain.index}
\alias{unchain}
\alias{unchain.index}

\title{
Chain an index
}

\description{
Chain a period-over-period index by taking the cumulative product its values to turn it into a fixed-base index. Unchain a fixed-base index by dividing its values for successive periods to get a period-over-period index.
}

\usage{
chain(x, ...)

\method{chain}{default}(x, ...)

\method{chain}{index}(x, ...)

unchain(x, ...)

\method{chain}{default}(x, ...)

\method{unchain}{index}(x, ...)
}

\arguments{
\item{x}{An object to chain/unchain.}

\item{...}{Further arguments passed to or used by methods.}
}

\details{
Chaining an index object takes the cumulative product of the values for each level, and returns the same type of index. Unchaining does the opposite. Quote contributions are removed when chaining/unchaining an index as it's not usually possible to chain them. Note that unchaining a period-over-period (i.e., \code{x$chain == TRUE}) index does nothing, as does chaining a fixed-base index (i.e., \code{x$chain == FALSE}).

The default method attempts to coerce \code{x} into an elemental index with \code{\link[=as_elemental_index]{as_elemental_index()}} prior to calling the next method.
}

\value{
An object that inherits from the \code{index} class.
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
}
