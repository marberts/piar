\name{coerce-index}
\alias{as.matrix.index}
\alias{as.data.frame.index}

\title{Coerce an index into a tabular form}

\description{
Turn an index into a matrix with a row for each level and a column for each period, or a data frame with three columns: period, level, and value. 
}

\usage{
\method{as.matrix}{index}(x, ...)

\method{as.data.frame}{index}(x, ..., stringsAsFactors = FALSE)
}

\arguments{
\item{x}{A price index, usually made by \code{\link[=elemental_index]{elemental_index()}}.}

\item{stringsAsFactors}{See \code{\link{as.data.frame}}.}

\item{...}{Further arguments passed to or used by methods.}
}

\value{
\code{as.data.frame()} returns a data frame.

\code{as.matrix()} returns a matrix.
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

as.data.frame(epr)
as.matrix(epr)
}