\name{summary.index}
\alias{summary.index}

\title{Summarize a price index}

\description{
Summarize an index as a matrix of index values (i.e., the five-number summary for each period). If there are percent-change contributions, then these are also summarized as a matrix.
}

\usage{
\method{summary}{index}(object, ...)
}

\arguments{
\item{object}{A price index, usually made by \code{\link[=elemental_index]{elemental_index()}}.}

\item{...}{Further arguments passed to or used by methods.}
}

\value{
A list of five-number summaries.
}

\seealso{
\code{\link{elemental_index}} to make an elemental index.

\code{\link{as_index}} to coerce into an index object.

\code{\link{chain}} for chaining period-over-period indexes, and \code{\link{rebase}} for rebasing an index.

\code{\link{contrib}} for extracting percent-change contributions.

\code{\link{combine-indexes}} to merge and stack index values.
}

\examples{
prices <- data.frame(
  rel = 1:8,
  period = rep(1:2, each = 4),
  ea = rep(letters[1:2], 4)
)

epr <- with(prices, elemental_index(rel, period, ea))

summary(epr)
}