\name{contrib}
\alias{contrib}
\alias{contrib.index}

\title{
Extract percent-change contributions
}

\description{
Extract percent-change contributions from a price index.
}

\usage{
contrib(x, ...)

\method{contrib}{index}(x, level = levels(x), ...)

}

\arguments{
\item{x}{A price index, as made by, e.g., \code{\link[=elemental_index]{elemental_index()}}.}

\item{level}{The level of an index for which percent-change contributions are desired, defaulting to the first level (usually the top-level for an aggregate index).}

\item{...}{Further arguments passed to or used by methods.}
}

\value{
A matrix of percent-change contributions with a column for each \code{period} and a row for each product in \code{level}. Contributions are padded with 0 to fit into a rectangular array when products differ over time.

If \code{x} does not have contributions then the result is \code{NULL}.
}

\examples{
prices <- data.frame(
  rel = 1:8,
  period = rep(1:2, each = 4),
  ea = rep(letters[1:2], 4)
)

epr <- with(
  prices,
  elemental_index(rel, period, ea, contrib = TRUE)
)

pias <- aggregation_structure(
  list(c("top", "top", "top"), c("a", "b", "c")), 1:3
)

index <- aggregate(epr, pias, na.rm = TRUE)

# Percent-change contributions for the top-level index

contrib(index)

# Calculate EA contributions for the chained index

library(gpindex)

arithmetic_contributions(
  as.matrix(chain(index))[c("a", "b", "c"), 2], 
  weights(pias, ea_only = TRUE)
)
}
