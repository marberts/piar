\name{aggregation_structure}
\alias{aggregation_structure}
\alias{weights.pias}
\alias{update.pias}

\title{
Aggregation structure
}

\description{
Construct a price index aggregation structure from a hierarchical classification.
}

\usage{
aggregation_structure(x, w)

\method{weights}{pias}(object, ea_only = FALSE, na.rm = FALSE, ...)

\method{update}{pias}(object, index, period = last(index), ...)
}

\arguments{
\item{x}{A list of character vectors that give the codes/labels for each level of the classification, ordered so that moving down the list goes down the hierarchy. The last vector gives the elemental aggregates, which should have no duplicates. All vectors should be the same length, without \code{NA}s, and there should be no duplicates across different levels of \code{x}.}

\item{w}{A numeric vector of weights for the elemental aggregates (i.e., the last vector in \code{x}). The default is to give each elemental aggregate the same weight.}

\item{object}{An object of class \code{pias}, as made by \code{aggregation_structure()}.}

\item{ea_only}{Should weights be returned for only the elemental aggregates, or the entire aggregation structure (the default)?}

\item{na.rm}{Should missing values be removed from \code{w} when aggregating the weights (i.e., when \code{ea_only = FALSE})? By default, missing values are not removed.}

\item{index}{An object of class \code{aggregate}, as made by \code{\link[=aggregate.index]{aggregate()}}.}

\item{period}{The time period used to price update the weights. The default uses last period in \code{object}.}

\item{...}{Further arguments passed to or used by methods.}
}

\value{
\code{aggregation_structure()} returns an object of class \code{pias}, which has the following components.

\item{child}{A nested list that gives the positions of the immediate children for each node in each level of the aggregation structure above the terminal nodes.}
\item{parent}{A list that gives the position of the immediate parent for each node of the aggregation structure below the initial nodes.}
\item{levels}{A character vector that gives the levels of \code{x}.}
\item{eas}{A character vector that gives the subset of \code{levels} that are elemental aggregates.}
\item{weights}{A named vector giving the weight for each elemental aggregate.}
\item{height}{The length of \code{x}.}

\code{weights()} returns a list with a named vector of weights for each level in the aggregation structure, unless \code{ea_only = TRUE}; in this case the return value is just a named vector.

\code{update()} returns a copy of \code{object} with price-updated weights from the index values in \code{index}.
}

\section{Warning}{
The \code{aggregation_structure()} function does its best to check its arguments, but there should be no expectation that the result of \code{aggregation_structure()} will make any sense if \code{x} does not represent a nested hierarchy.
}

\seealso{
\code{\link[=aggregate.index]{aggregate}} to aggregate price indexes made with \code{\link[=elemental_index]{elemental_index()}}.

\code{\link{expand_classification}} to help make \code{x}.
}

\examples{
# A simple example

x1 <- c("1", "1", "1")
x2 <- c("11", "11", "12")
x3 <- c("111", "112", "121")

aggregation_structure(list(x1, x2, x3))

# The aggregation structure can also be made by expanding 'x3'

expand_classification(x3)

all.equal(aggregation_structure(list(x1, x2, x3)), 
          aggregation_structure(expand_classification(x3)))

# Unequal weights

aggregation_structure(list(x1, x2, x3), 1:3)
          
# Extract the weights

weights(aggregation_structure(list(x1, x2, x3)))
}