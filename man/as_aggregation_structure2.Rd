\name{as_aggregation_structure}
\alias{as_aggregation_structure}
\alias{as_aggregation_structure.default}
\alias{as_aggregation_structure.data.frame}
\alias{as_aggregation_structure.matrix}
\alias{as_aggregation_structure.aggregate_piar_index}
\alias{as_aggregation_structure.piar_aggregation_structure}
\alias{is_aggregation_structure}

\title{
Coerce to an aggregation structure
}

\description{
Coerce an object into an aggregation structure object, and test if an object is an aggregation structure.
}

\usage{
as_aggregation_structure(x, ...)

\method{as_aggregation_structure}{default}(x, w = NULL, ...)

\method{as_aggregation_structure}{data.frame}(x, ...)

\method{as_aggregation_structure}{matrix}(x, ...)

\method{as_aggregation_structure}{aggregate_piar_index}(x, w = NULL, ...)

\method{as_aggregation_structure}{piar_aggregation_structure}(x, ...)

is_aggregation_structure(x)
}

\arguments{
\item{x}{An object to test or coerce into an aggregation structure.}

\item{w}{A numeric vector of aggregation weights for the elemental aggregates. The default is to give each elemental aggregate the same weight.}

\item{...}{Further arguments passed to or used by methods.}
}

\details{
The default method attempts to coerce \code{x} into a list prior to calling \code{\link[=aggregation_structure]{aggregation_structure()}}.

The data frame and matrix methods treat \code{x} as a table with a row for each elemental aggregate, a column of labels for each level in the aggregation structure, and a column of weights for the elemental aggregates.

The method for aggregate indexes reconstructs the aggregation structure used to generate the index (with optional weights).
}

\value{
A price index aggregation structure that inherits from \code{piar_aggregation_structure}, except for \code{is_aggregate_index()} that returns \code{TRUE} if \code{x} inherits from \code{piar_aggregation_structure}.
}

\seealso{
\code{\link[=as.matrix.piar_aggregation_structure]{as.matrix}} and \code{\link[=as.data.frame.piar_aggregation_structure]{as.data.frame}} for coercing an aggregation structure into a tabular form.
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
                                  
pias <- aggregation_structure(aggregation_weights[1:3],
                              w = aggregation_weights[[4]])
                              
all.equal(
  pias,
  as_aggregation_structure(aggregation_weights)
)

all.equal(
  pias,
  as_aggregation_structure(as.matrix(aggregation_weights))
)
}
