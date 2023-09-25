\name{aggregate.abstract_index}
\alias{aggregate.chainable_index}
\alias{aggregate.direct_index}
\alias{aggregate.abstract_index}

\title{Aggregate elemental price indexes}

\description{
Aggregate elemental price indexes with a price index aggregation structure.
}

\usage{
\method{aggregate}{chainable_index}(x, pias, na.rm = FALSE, r = 1, ...)

\method{aggregate}{direct_index}(x, pias, na.rm = FALSE, r = 1, ...)
}

\arguments{
\item{x}{A price index, usually made by \code{\link[=elemental_index]{elemental_index()}}.}

\item{pias}{A price index aggregation structure or something that can be coerced into one. This can be made with \code{\link[=aggregation_structure]{aggregation_structure()}}.}

\item{na.rm}{Should missing values be removed? By default, missing values are not removed. Setting \code{na.rm = TRUE} is equivalent to overall mean imputation.}

\item{r}{Order of the generalized mean to aggregate index values. 0 for a geometric index (the default for making elemental indexes), 1 for an arithmetic index (the default for aggregating elemental indexes and averaging indexes over subperiods), or -1 for a harmonic index (usually for a Paasche index). Other values are possible; see \code{\link[=generalized_mean]{generalized_mean()}} for details.}

\item{...}{Further arguments passed to or used by methods.}
}

\details{
The \code{aggregate()} method loops over each time period in \code{x} and
\enumerate{
\item aggregates the elemental indexes with \code{\link[=generalized_mean]{generalized_mean(r)}} for each level of \code{pias};
\item aggregates percent-change contributions for each level of \code{pias} (if there are any);
\item price updates the weights in \code{pias} with \code{\link[=factor_weights]{factor_weights(r)}} (only for period-over-period elemental indexes, i.e., \code{is_chainable_index(x) == TRUE}).
}
The result is a collection of aggregated period-over-period indexes that can be chained together to get a fixed-base index when \code{x} are period-over-period elemental indexes. Otherwise, when \code{x} are fixed-base elemental indexes, the result is a collection of aggregated fixed-base (direct) indexes.

By default, missing elemental indexes will propagate when aggregating the index. Missing elemental indexes can be due to both missingness of these values in \code{x}, and the presence of elemental aggregates in \code{pias} that are not part of \code{x}. Setting \code{na.rm = TRUE} ignores missing values, and is equivalent to parental (or overall mean) imputation. As an aggregated price index generally cannot have missing values (for otherwise it can't be chained over time), any missing values for a level of \code{pias} are removed and recursively replaced by the value of its immediate parent.

In most cases aggregation is done with an arithmetic mean (the default), and this is detailed in chapter 8 (pp. 190--198) of the CPI manual (2020). Aggregating with a non-arithmetic mean follows the same steps, except that the elemental indexes are aggregated with a mean of a different order (e.g., harmonic for a Paasche index), and the method for price updating the weights is slightly different.

Aggregating percent-change contributions uses the method in chapter 9 of the CPI manual (equations 9.26 and 9.28) when aggregating with an arithmetic mean. With a non-arithmetic mean, arithmetic weights are constructed using \code{\link[=transmute_weights]{transmute_weights(r, 1)}} in order to apply this method.

There may not be contributions for all prices relatives in an elemental aggregate if the elemental indexes are built from several sources (as with \code{\link[=merge.abstract_index]{merge()}}). In this case the contribution for a price relative in the aggregated index will be correct, but the sum of all contributions will not equal the change in the value of the index. This can also happen when aggregating an already aggregated index in which missing index values have been imputed (i.e., when \code{na.rm = TRUE}).
}

\value{
An aggregate price index. This is an object that inherits from \code{aggregate_index} and the class of \code{x}, with the following components.

\item{index}{A named list with an entry for each \code{period} in \code{x} that gives a named vector of index values for each level in \code{pias}.}
\item{contrib}{A named list with an entry for each \code{period}, which itself contains a list with an entry for each level in \code{pias} with a named vector that gives the additive contribution for each price relative.}
\item{levels}{The levels for \code{pias}.}
\item{time}{The levels for \code{period} from \code{x}.}
\item{r}{The value for \code{r}.}
\item{pias}{A list containing the \code{child}, \code{parent}, \code{eas}, and \code{height} components of \code{pias}.}
}

\references{
Balk, B. M. (2008). \emph{Price and Quantity Index Numbers}. Cambridge University Press.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2020). \emph{Consumer Price Index Manual: Theory and Practice}. International Monetary Fund.
}

\seealso{
\code{\link{elemental_index}} for making elemental price indexes.

\code{\link{aggregation_structure}} for making a price index aggregation structure.

\code{\link[=mean.abstract_index]{mean}} for aggregating over subperiods.
}

\examples{
prices <- data.frame(
  rel = 1:8,
  period = rep(1:2, each = 4),
  ea = rep(letters[1:2], 4)
)

# A two-level aggregation structure

pias <- aggregation_structure(
  list(c("top", "top", "top"), c("a", "b", "c")), 1:3
)

# Calculate Jevons elemental indexes

(epr <- with(prices, elemental_index(rel, period, ea)))

# Aggregate (note the imputation for elemental index 'c')

(index <- aggregate(epr, pias, na.rm = TRUE))

# Aggregation can equivalently be done as matrix multiplication

as.matrix(pias) \%*\% as.matrix(chain(index[letters[1:3]]))
}