\name{chain}
\alias{chain}
\alias{chain.default}
\alias{chain.pindex}
\alias{unchain}
\alias{unchain.default}
\alias{unchain.pindex}
\alias{rebase}
\alias{rebase.default}
\alias{rebase.pindex}
\alias{is_chainable_index}

\title{
Chain and rebase a price index
}

\description{
Chain a period-over-period index by taking the cumulative product of its values to turn it into a fixed-base (direct) index. Unchain a fixed-base index by dividing its values for successive periods to get a period-over-period index. Rebase a fixed-base index by dividing its values by the value of the index in the new base period.
}

\usage{
chain(x, ...)

\method{chain}{default}(x, ...)

\method{chain}{pindex}(x, link = rep(1, nlevels(x)), ...)

unchain(x, ...)

\method{unchain}{default}(x, ...)

\method{unchain}{pindex}(x, ...)

rebase(x, ...)

\method{rebase}{default}(x, ...)

\method{rebase}{pindex}(x, base = rep(1, nlevels(x)), ...)

is_chainable_index(x)
}

\arguments{
\item{x}{A price index, as made by, e.g., \code{\link[=elemental_index]{elemental_index()}}.}

\item{link}{A numeric vector, or something that can coerced into one, of link values for each level in \code{x}. The default is a vector of 1s so that no linking is done.}

\item{base}{A numeric vector, or something that can coerced into one, of base-period index values for each level in \code{x}. The default is a vector of 1s so that the base period remains the same.}

\item{...}{Further arguments passed to or used by methods.}
}

\details{
The default methods attempts to coerce \code{x} into an index with \code{\link[=as_index]{as_index()}} prior to chaining/unchaining/rebasing.

Chaining an index takes the cumulative product of the index values for each level; this is roughly the same as \code{t(apply(as.matrix(x), 1, cumprod)) * link}. Unchaining does the opposite, so these are inverse operations. Note that unchaining a period-over-period index (i.e., when \code{is_chainable_index(x) == TRUE}) does nothing, as does chaining a fixed-base index (i.e., when \code{is_chainable_index(x) == FALSE}).

Rebasing a fixed-base index divides the values for each level of this index by the corresponding values for each level in the new base period. It's roughly the same as \code{as.matrix(x) / base}. Like unchaining, rebasing a period-over-period index does nothing.

Quote contributions are removed when chaining/unchaining/rebasing an index, as it's not usually possible to update them correctly.
}

\value{
Each of these functions returns an index of the same type as \code{x}, except \code{is_chainable_index()} which returns \code{TRUE} when \code{x} is a period-over-period index (so that chaining makes sense).
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

# Change the base period to period 2 (note the loss of information for period 0)

epr <- chain(epr)
rebase(epr, epr[, 2])
}
