\name{stack.piar_index}
\alias{stack.aggregate_piar_index}
\alias{stack.chainable_piar_index}
\alias{stack.direct_piar_index}
\alias{stack.piar_index}
\alias{unstack.piar_index}

\title{Stack price indexes}

\description{
\code{stack()} combines two price indexes with common levels, stacking index values and percent-change contributions for one index after the other.

\code{unstack()} breaks up a price index into a list of indexes for each time period. 

These methods can be used in a map-reduce to make an index with multiple aggregation structures (like a Paasche index).
}

\usage{
\method{stack}{aggregate_piar_index}(x, y, ...)

\method{stack}{chainable_piar_index}(x, y, ...)

\method{stack}{direct_piar_index}(x, y, ...)

\method{unstack}{piar_index}(x, ...)
}

\arguments{
\item{x}{A price index, as made by, e.g., \code{\link[=elemental_index]{elemental_index()}}.}

\item{y}{A price index, or something that can coerced into one. If \code{x} is a period-over-period index then \code{y} is coerced into a chainable index; otherwise, \code{y} is coerced into a direct index.}

\item{...}{Further arguments passed to or used by methods.}
}

\value{
\code{stack()} returns a price index with the same class as \code{x}. If \code{y} is an aggregate index then the result will also inherit from \code{\link[=piar_index]{aggregate_piar_index}}.

\code{unstack()} returns a list of price indexes with the same class as \code{x}.
}

\seealso{
\code{\link[=merge.piar_index]{merge}} to combine indexes for different levels over the same time periods.
}

\examples{
prices <- data.frame(
  rel = 1:8,
  period = rep(1:2, each = 4),
  ea = rep(letters[1:2], 4)
)

prices2 <- data.frame(
  rel = 1:8,
  period = rep(3:4, each = 4),
  ea = rep(letters[1:2], 4)
)

epr <- with(prices, elemental_index(rel, period, ea))

epr2 <- with(prices2, elemental_index(rel, period, ea))

stack(epr, epr2)

# Unstack does the reverse

all.equal(c(unstack(epr), unstack(epr2)),
          unstack(stack(epr, epr2)))
}