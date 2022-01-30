\name{aggregation_structure}
\alias{aggregation_structure}
\alias{weights.pias}
\alias{update.pias}
\alias{expand_classification}

\title{
Aggregation structure
}

\description{
Create a price index aggregation structure from a hierarchical classification and aggregation weights.
}

\usage{
aggregation_structure(x, w)

\method{weights}{pias}(object, ea_only = FALSE, na.rm = FALSE, ...)

\method{update}{pias}(object, index, period = end(index), ...)

expand_classification(class, width)
}

\arguments{
\item{x}{A list of character vectors (or vectors to be coerced to character) that give the codes/labels for the aggregation weights for each level of the classification, ordered so that moving down the list goes down the hierarchy. The last vector gives the elemental aggregates, which should have no duplicates. All vectors should be the same length, without \code{NA}s, and there should be no duplicates across different levels of \code{x}. This is usually a collection of columns in a data frame with a row for each aggregation weight; \code{expand_classification()} can be used when the classification is represented as a string.}

\item{w}{A numeric vector of aggregation weights for the elemental aggregates (i.e., the last vector in \code{x}). The default is to give each elemental aggregate the same weight.}

\item{object}{A price index aggregation structure, as made by \code{aggregation_structure()}.}

\item{ea_only}{Should weights be returned for only the elemental aggregates? The default gives the weights for the entire aggregation structure.}

\item{na.rm}{Should missing values be removed from \code{w} when aggregating the weights (i.e., when \code{ea_only = FALSE})? By default, missing values are not removed.}

\item{index}{An aggregate price index, as made by \code{\link[=aggregate.ind]{aggregate()}}.}

\item{period}{The time period used to price update the weights. The default uses the last period in \code{object}.}

\item{class}{A character vector, or something that can be coerced into one, of codes/labels for a specific level in a classification (e.g., 5-digit COICOP, 5-digit NAICS, 4-digit SIC).}

\item{width}{A numeric vector that gives the width of each digit in \code{x}. This cannot contain NAs. The default assumes each digit has a width of 1, as in the NAICS, NAPCS, and SIC classifications.}

\item{...}{Further arguments passed to or used by methods.}
}

\value{
\code{aggregation_structure()} returns a price index aggregation structure. This is an object of class \code{pias}, which has the following components.

\item{child}{A nested list that gives the positions of the immediate children for each node in each level of the aggregation structure above the terminal nodes.}
\item{parent}{A list that gives the position of the immediate parent for each node of the aggregation structure below the initial nodes.}
\item{levels}{A character vector that gives the levels of \code{x}.}
\item{eas}{A character vector that gives the subset of \code{levels} that are elemental aggregates.}
\item{weights}{A named vector giving the weight for each elemental aggregate.}
\item{height}{The length of \code{x}.}

\code{weights()} returns a list with a named vector of weights for each level in the aggregation structure, unless \code{ea_only = TRUE}; in this case the return value is just a named vector.

\code{update()} returns a copy of \code{object} with price-updated weights using the index values in \code{index}.

\code{expand_classification()} returns a list with a entry for each level in \code{x} giving the "digits" that represent each level in the hierarchy.
}

\section{Warning}{
The \code{aggregation_structure()} function does its best to check its arguments, but there should be no expectation that the result of \code{aggregation_structure()} will make any sense if \code{x} does not represent a nested hierarchy.
}

\seealso{
\code{\link[=aggregate.ind]{aggregate}} to aggregate price indexes made with \code{\link[=elemental_index]{elemental_index()}}.
}

\examples{
# A simple example
#            1
#      |-----+-----|
#      11          12
#  |---+---|       |
#  111     112     121

x1 <- c("1",   "1",   "1")
x2 <- c("11",  "11",  "12")
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

# Expanding more complex classifications
# ... if last 'digit' is either TA or TS

expand_classification(c("111TA", "112TA", "121TS"), width = c(1, 1, 1, 2))

# ... if first 'digit' is either 11 or 12

expand_classification(c("111", "112", "121"), width = c(2, 1))

# ...if there are delimiters in the classification (like COICOP)

expand_classification(c("01.1.1", "01.1.2", "01.2.1"), width = c(2, 2, 2))
}