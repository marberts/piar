\name{merge.pindex}
\alias{merge.pindex}

\title{Merge price indexes}

\description{
Combine two price indexes with common time periods, merging together the index values and percent-change contributions for each time period.

This is useful for building up an index when different elemental aggregates come from different sources of data, or use different index-number formulas.
}

\usage{
\method{merge}{pindex}(x, y, ...)
}

\arguments{
\item{x}{A price index, as made by, e.g., \code{\link[=elemental_index]{elemental_index()}}.}

\item{y}{A price index, or something that can coerced into one. If \code{x} is a period-over-period index then \code{y} is coerce into a chainable index; otherwise, \code{y} is coerced into a direct index.}

\item{...}{Further arguments passed to or used by methods.}
}

\value{
A price index that inherits from \code{chainable_pindex} if \code{x} is a period-over-period index, or \code{direct_pindex} if \code{x} is a fixed-base index. It is not generally possible to merge aggregated indexes, as this would change the aggregation structure, so merging does not return an aggregated index. 
}

\seealso{
\code{\link[=stack.pindex]{stack}} to combine indexes for the same levels over different time periods.
}

\examples{
prices <- data.frame(
  rel = 1:8,
  period = rep(1:2, each = 4),
  ea = rep(letters[1:2], 4)
)

prices2 <- data.frame(
  rel = 1:8,
  period = rep(1:2, each = 4),
  ea = rep(letters[3:4], 4)
)

epr <- with(prices, elemental_index(rel, period, ea))

epr2 <- with(prices2, elemental_index(rel, period, ea))

merge(epr, epr2)
}