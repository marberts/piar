\name{coerce-aggregation-structure}
\alias{as.matrix.aggregation_structure}
\alias{as.data.frame.aggregation_structure}

\title{
Coerce a price index aggregation structure into a tabular form
}

\description{
Coerce a price index aggregation structure into an aggregation matrix, or a data frame.
}

\usage{
\method{as.matrix}{aggregation_structure}(x, ...)

\method{as.data.frame}{aggregation_structure}(x, ..., stringsAsFactors = FALSE)
}

\arguments{
\item{x}{A price index aggregation structure, as made by \code{\link[=aggregation_structure]{aggregation_structure()}}.}

\item{stringsAsFactors}{See \code{\link{as.data.frame}}.}

\item{...}{Further arguments passed to or used by methods.}
}

\value{
\code{as.matrix()} represents an aggregation structure as a matrix, such that multiplying with a (column) vector of elemental indexes gives the aggregated index.

\code{as.data.frame()} takes an aggregation structure and returns a data frame that could have generated it, with columns \code{level1}, \code{level2}, ..., \code{ea}, and \code{weight}.
}

\examples{
# A simple aggregation structure
#            1
#      |-----+-----|
#      11          12
#  |---+---|       |
#  111     112     121
#  (1)     (3)     (4)

aggregation_weights <- data.frame(
  level1 = c("1",   "1",   "1"),
  level2 = c("11",  "11",  "12"),
  ea     = c("111", "112", "121"),
  weight = c(1,     3,     4)
)

pias <- as_aggregation_structure(aggregation_weights)

as.data.frame(pias)
as.matrix(pias)
}