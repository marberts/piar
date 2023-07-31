\name{stack.index}
\alias{stack.index}
\alias{unstack.index}

\title{Stack price indexes}

\description{
\code{stack()} combines two index objects with common levels, stacking index values and percent-change contributions for one index after the other.

\code{unstack()} breaks up an index into a list of indexes for each time period. 

These methods can be used in a map-reduce to make an index with multiple aggregation structures (like a Paasche index).
}

\usage{
\method{stack}{index}(x, y, ...)

\method{unstack}{index}(x, ...)
}

\arguments{
\item{x, y}{A price index, as made by, e.g., \code{\link[=elemental_index]{elemental_index()}}.}

\item{...}{Further arguments passed to or used by methods.}
}

\value{
\code{stack()} returns an index object. If at least one of \code{x} or \code{y} is an aggregate index then the result of stacking these indexes is also an aggregate index; otherwise, it is the same class as \code{x}.

\code{unstack()} returns a list of index objects.
}

\seealso{
\code{\link[=merge.index]{merge}} to combine indexes for different levels over the same time periods.
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