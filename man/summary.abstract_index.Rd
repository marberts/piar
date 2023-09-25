\name{summary.abstract_index}
\alias{summary.abstract_index}

\title{Summarize a price index}

\description{
Summarize an index as a matrix of index values (i.e., the five-number summary for each period). If there are percent-change contributions, then these are also summarized as a matrix.
}

\usage{
\method{summary}{abstract_index}(object, ...)
}

\arguments{
\item{object}{A price index, as made by, e.g., \code{\link[=elemental_index]{elemental_index()}}.}

\item{...}{Further arguments passed to or used by methods.}
}

\value{
A list of five-number summaries.
}

\note{
This function is still experimental and may change in the future.
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