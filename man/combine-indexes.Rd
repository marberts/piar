\name{combine-indexes}
\alias{combine-indexes}
\alias{merge.index}
\alias{stack.index}
\alias{unstack.index}

\title{Combine price indexes}

\description{
Merge indexes to combine index values for different levels over the same time periods. Stack indexes to combine index values for the same levels over different time periods.
}

\usage{
\method{merge}{index}(x, y, ...)

\method{stack}{index}(x, y, ...)

\method{unstack}{index}(x, ...)
}

\arguments{
\item{x, y}{A price index, usually made by \code{\link[=elemental_index]{elemental_index()}}.}

\item{...}{Further arguments passed to or used by methods.}
}

\details{
The \code{merge()} method combines two index objects with common time periods, merging together the index values and percent-change contributions for each time period in \code{x} and \code{y}. This is useful for building up an index when different elemental aggregates come from different sources of data, or use different index-number formulas. 

The \code{stack()} method combines two index objects with common levels, stacking index values and percent-change contributions for each level in \code{y} after those in \code{x}. The \code{unstack()} method breaks up \code{x} into a list of indexes, one for each period in \code{x}. These methods can be used in a map-reduce to make an index with multiple aggregation structures (like a Paasche index).

It is not generally possible to merge aggregated indexes, as this would change the aggregation structure, so merging does not return aggregated index. If at least one of \code{x} or \code{y} is an aggregate index then the result of stacking these indexes is also an aggregate index; otherwise, it is the same class as \code{x}.
}

\value{
\code{merge()} and \code{stack()} return an index object. \code{unstack()} returns a list of index objects.
}

\seealso{
\code{\link{elemental_index}} to make an elemental index.

\code{\link{as_index}} to coerce into an index object.

\code{\link{chain}} for chaining period-over-period indexes, and \code{\link{rebase}} for rebasing an index.

\code{\link{contrib}} for extracting percent-change contributions.
}

\examples{
prices <- data.frame(rel = 1:8,
                     period = rep(1:2, each = 4),
                     ea = rep(letters[1:2], 4))

# Calculate Jevons elemental indexes

epr <- with(prices, elemental_index(rel, period, ea))

# Merge two indexes

prices2 <- data.frame(rel = 1:8,
                      period = rep(1:2, each = 4),
                      ea = rep(letters[3:4], 4))
epr2 <- with(prices2, elemental_index(rel, period, ea))
merge(epr, epr2)

# Stack two indexes

prices3 <- data.frame(rel = 1:8,
                      period = rep(3:4, each = 4),
                      ea = rep(letters[1:2], 4))
epr3 <- with(prices3, elemental_index(rel, period, ea))
stack(epr, epr3)

# Unstack does the reverse

all.equal(c(unstack(epr), unstack(epr3)), unstack(stack(epr, epr3)))
}