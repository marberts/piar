\name{merge.index}
\alias{merge.index}

\title{Merge price indexes}

\description{
Combine two index objects with common time periods, merging together the index values and percent-change contributions for each time period.

This is useful for building up an index when different elemental aggregates come from different sources of data, or use different index-number formulas.
}

\usage{
\method{merge}{index}(x, y, ...)
}

\arguments{
\item{x, y}{A price index, as made by, e.g., \code{\link[=elemental_index]{elemental_index()}}.}

\item{...}{Further arguments passed to or used by methods.}
}

\value{
An index object that inherits from \code{x}. It is not generally possible to merge aggregated indexes, as this would change the aggregation structure, so merging does not return aggregated index. 
}

\seealso{
\code{\link[=stack.index]{stack}} to combine indexes for the same levels over different time periods.
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