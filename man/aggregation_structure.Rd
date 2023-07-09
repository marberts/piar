\name{aggregation_structure}
\alias{aggregation_structure}

\title{
Make a price index aggregation structure
}

\description{
Create a price index aggregation structure from a hierarchical classification and aggregation weights that can be used to aggregate elemental indexes.
}

\usage{
aggregation_structure(x, w = NULL, ...)

method{aggregation_structure}{default}(x, w = NULL, ...)

method{aggregtion_structure}{data.frame}(x, ...)

method{aggregtion_structure}{matrix}(x, ...)

method{aggregation_structure}{agg_pindex}(x, w = NULL, ...)
}

\arguments{
\item{x}{An object representing a hierarchical classification.}

\item{w}{A numeric vector of aggregation weights for the elemental aggregates (i.e., the last vector in \code{x}). The default is to give each elemental aggregate the same weight.}

\item{...}{Further arguments passed to or used by methods.}
}

\details{
The default method takes a list of factors (or vectors to be coerced to factors) that give the codes/labels for each level of the classification, ordered so that moving down the list goes down the hierarchy. The last vector gives the elemental aggregates, which should have no duplicates. All vectors should be the same length, without \code{NA}s, and there should be no duplicates across different levels of \code{x}.

The \code{data.frame} and \code{matrix} methods are convenient wrapper around the default method. They assume the input is a table that gives the aggregation weight for each elemental aggregate in the last column, and the labels for each level of the hierarchy in the preceeding columns (one label per column, ordered left to right).

The \code{agg_pindex} method constructs the aggregation structure used to aggregate an index.
}

\value{
A price index aggregation structure. This is an object of class \code{pias}, which has the following components.

\item{child}{A nested list that gives the positions of the immediate children for each node in each level of the aggregation structure above the terminal nodes.}
\item{parent}{A list that gives the position of the immediate parent for each node of the aggregation structure below the initial nodes.}
\item{levels}{A character vector that gives the levels of \code{x}.}
\item{eas}{A character vector that gives the subset of \code{levels} that are elemental aggregates.}
\item{weights}{A named vector giving the weight for each elemental aggregate.}
\item{height}{The length of \code{x}.}
}

\section{Warning}{
The \code{aggregation_structure()} function does its best to check its arguments, but there should be no expectation that the result of \code{aggregation_structure()} will make any sense if \code{x} does not represent a nested hierarchy.
}

\seealso{
\code{\link[=aggregate.pindex]{aggregate}} to aggregate price indexes made with \code{\link[=elemental_index]{elemental_index()}}.

\code{\link{expand_classification}} to make \code{x} from a character representation of a hierarchical aggregation structure.

\code{\link{pias-methods}} for methods for \code{pias} objects.
}

\examples{
# A simple aggregation structure
#            1
#      |-----+-----|
#      11          12
#  |---+---|       |
#  111     112     121
#  (1)     (3)     (4)

aggregation_weights <- data.frame(level1 = c("1",   "1",   "1"),
                                  level2 = c("11",  "11",  "12"),
                                  ea     = c("111", "112", "121"),
                                  weight = c(1,     3,     4))
                                  
pias <- aggregation_structure(aggregation_weights)

# The same as the "old fashioned" way
all.equal(
  aggregation_structure(as.list(aggregation_weights[1:3]),
                        w = aggregation_weights[[4]]),
  pias
)

# The aggregation structure can also be made by expanding the
# elemental aggregates

all.equal(
  with(aggregation_weights,
       aggregation_structure(expand_classification(ea)), weight),
  pias
)
}
