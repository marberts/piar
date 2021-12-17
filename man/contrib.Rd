\name{contrib}
\alias{contrib}
\alias{contrib.ind}

\title{
Extract quote contributions
}

\description{
Extract quote contributions from an index object.
}

\usage{
contrib(x, ...)

\method{contrib}{ind}(x, level = levels(x), ...)

}

\arguments{
\item{x}{A price index, as made by, e.g., \code{\link[=elemental_index]{elemental_index()}}.}

\item{level}{The level of an index for which quote contributions are desired, defaulting to the first level (usually the top-level for an aggregate index).}

\item{...}{Further arguments passed to or used by methods.}
}

\value{
A matrix of quote contributions with a column of each \code{period} and a row for each product in \code{level}. Contributions are padded with 0 to fit into a rectangular array when products differ over time.
}

\seealso{
\code{\link{elemental_index}} for making elemental price indexes.
}

\examples{
prices <- data.frame(rel = 1:8, period = rep(1:2, each = 4), ea = rep(letters[1:2], 4))

epr <- with(prices, elemental_index(rel, period, ea, contrib = TRUE))

pias <- aggregation_structure(list(c("top", "top", "top"), c("a", "b", "c")), 1:3)

index <- aggregate(epr, pias, na.rm = TRUE)

# Quote contributions for the top-level index
contrib(index)

# Calculate EA contributions for the chained index
gpindex::arithmetic_contributions(as.matrix(chain(index))[c("a", "b", "c"), 2], 
                                  weights(pias, ea_only = TRUE))
}
