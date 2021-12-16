\name{price_indexes}
\alias{elemental_index}
\alias{elemental_index.default}
\alias{elemental_index.numeric}
\alias{aggregate.ind}
\alias{vcov.agg_ind}
\alias{mean.ind}
\alias{merge.ind}
\alias{stack.ind}
\alias{unstack.ind}
\alias{[.ind}
\alias{[<-.ind}
\alias{levels.ind}
\alias{time.ind}
\alias{start.ind}
\alias{end.ind}
\alias{head.ind}
\alias{tail.ind}
\alias{summary.ind}
\alias{as.matrix.ind}
\alias{as.data.frame.ind}
\alias{as_elemental_index}
\alias{as_elemental_index.default}
\alias{as_elemental_index.matrix}
\alias{as_elemental_index.agg_ind}
\alias{is_index}
\alias{is_elemental_index}
\alias{is_aggregate_index}

\title{Price indexes}

\description{
Compute period-over-period or fixed-base elemental price indexes (with optional quote contributions), or coerce pre-computed index values into an elemental index object.
There are methods to aggregate these indexes with a price index aggregation structure or average them over time, combine indexes together, extract useful information, and coerce into a tabular form.
}

\usage{
# Elemental indexes
elemental_index(rel, ...)

\method{elemental_index}{default}(rel, ...)

\method{elemental_index}{numeric}(rel, period = gl(1, length(rel)), ea = gl(1, length(rel)), 
                w, contrib = FALSE, chain = TRUE, na.rm = FALSE, r = 0, ...)
                            
# Aggregate
\method{aggregate}{ind}(x, pias, na.rm = FALSE, r = 1, ...)

\method{vcov}{agg_ind}(object, repweights, mse = TRUE, ...)

\method{mean}{ind}(x, window = 3, na.rm = FALSE, r = 1, ...)

# Combine
\method{merge}{ind}(x, y, ...)

\method{stack}{ind}(x, y, ...)

\method{unstack}{ind}(x, ...)

# Extract
\method{[}{ind}(x, i, j)

\method{[}{ind}(x, i, j) <- value

\method{levels}{ind}(x)

\method{time}{ind}(x, ...)

\method{start}{ind}(x, ...)

\method{end}{ind}(x, ...)

\method{head}{ind}(x, ...)

\method{tail}{ind}(x, ...)

# Coerce
\method{as.matrix}{ind}(x, ...)

\method{as.data.frame}{ind}(x, ...)
                            
as_elemental_index(x, ...)

\method{as_elemental_index}{default}(x, ...)

\method{as_elemental_index}{matrix}(x, chain = TRUE, ...)

\method{as_elemental_index}{agg_ind}(x, ...)

is_index(x)

is_elemental_index(x)

is_aggregate_index(x)
}

\arguments{
\item{rel}{Period-over-period or fixed-base price relatives, usually a numeric vector. The former can be made with \code{\link[=price_relative]{price_relative()}}.}

\item{period}{A factor, or something that can be coerced into one, giving the time period associated with each price relative in \code{rel}. The ordering of time periods follows of the levels of \code{period}, to agree with \code{\link[=cut.Date]{cut()}}. The default assumes that all price relatives belong to one time period.}

\item{ea}{A factor, or something that can be coerced into one, giving the elemental aggregate associated with each price relative in \code{rel}. The default assumes that all price relatives belong to one elemental aggregate.}

\item{w}{A numeric vector of weights for the price relatives in \code{rel} (i.e., quote/product weights). The default is to give each price relative equal weight.}

\item{contrib}{Should quote contributions be calculated? The default does not calculate contributions.}

\item{chain}{Are the price relatives in \code{rel} period-over-period relatives for a chained calculation (the default)? This should be \code{FALSE} when \code{rel} are fixed-base relatives.}

\item{na.rm}{Should missing values be removed? By default, missing values are not removed. Setting \code{na.rm = TRUE} is equivalent to overall mean imputation.}

\item{r}{Order of the generalized mean to aggregate price relatives/index values. 0 for a geometric index (the default for making elemental indexes), 1 for an arithmetic index (the default for aggregating elemental indexes and averaging indexes over time), or -1 for a harmonic index (usually for a Paasche index). Other values are possible; see \code{\link[=generalized_mean]{generalized_mean()}} for details.}

\item{x, y}{A price index object, as made by \code{elemental_index()} or \code{aggregate()}.}

\item{pias}{A price index aggregation structure. This can be made with \code{\link[=aggregation_structure]{aggregation_structure()}}.}

\item{object}{An aggregate price index, as made by \code{aggregate()}.}

\item{repweights}{A matrix, or something that can be coerced into one, of bootstrap replicate weights with a row for each elemental aggregate and a column for each replicate.}

\item{mse}{Should covariances be centered off the value of the index in \code{object} (the default), or the mean of the replicates?}

\item{window}{The size of the rolling-window used to average index values across time periods. The default (3) turns a monthly index into into a quarterly one.}

\item{i, j, value}{See \link{Extract}, with \code{value} being a numeric vector (or something that can coerced into one.)}

\item{...}{Further arguments passed to or used by methods.}
}

\details{
\subsection{Elemental indexes}{
When supplied with a numeric vector, \code{elemental_index()} is a simple wrapper that applies \code{\link[=generalized_mean]{generalized_mean(r)}} and \code{\link[=contributions]{contributions(r)}} (if \code{contrib = TRUE}) to \code{rel} and \code{w} grouped by \code{ea} and \code{period}. That is, for every combination of elemental aggregate and time period, \code{elemental_index()} calculates an index based on a generalized mean of order \code{r} and, optionally, quote contributions (using names for \code{rel} as product names). The default (\code{r = 0} and no weights) makes Jevons elemental indexes. See chapter 8 (pp. 175--190) of the CPI manual (2020) for more detail about making elemental indexes, and chapter 5 of Balk (2008).

The default method simply coerces \code{rel} to a numeric prior to calling the method above.

The interpretation of the index depends on how the price relatives in \code{rel} are made. If these are period-over-period relatives, then the result is a collection of period-over-period elemental indexes; if these are fixed-base relatives, then the result is a collection of fixed-base elemental indexes. For the latter, \code{chain} should be set to \code{FALSE} so that no subsequent methods assume that a chained calculation should be used.

By default, missing price relatives in \code{rel} will propagate throughout the index calculation. Ignoring missing values with \code{na.rm = TRUE} is the same as parental (or overall mean) imputation, and needs to be explicitly set in the call to \code{elemental_index()}. Explicit imputation of missing relatives, and especially imputation of missing prices, should be done prior to calling \code{elemental_index()}.
}

\subsection{Aggregate}{
The \code{aggregate()} method aggregates elemental indexes by looping over each time period in \code{x} and
\enumerate{
\item aggregates the elemental indexes with \code{\link[=generalized_mean]{generalized_mean(r)}} for each level of \code{pias};
\item aggregates quote contributions for each level of \code{pias} (if there are any);
\item price updates the weights in \code{pias} with \code{\link[=factor_weights]{factor_weights(r)}} (only for period-over-period elemental indexes, i.e., \code{is_chain_index(x) == TRUE}).
}
The result is a collection of aggregated period-over-period indexes that can be chained together to get a fixed-base index when \code{x} are period-over-period elemental indexes. Otherwise, when \code{x} are fixed-base elemental indexes, the result is a collection of aggregated fixed-base indexes.

By default, missing elemental indexes will propagate when aggregating the index. Missing elemental indexes can be due to both missingness of these values in \code{x}, and the presence of elemental aggregates in \code{pias} that are not part of \code{x}. Setting \code{na.rm = TRUE} ignores missing values, and is equivalent to parental (or overall mean) imputation. As an aggregated price index generally cannot have missing values (for otherwise it can't be chained over time), any missing values for a level of \code{pias} are removed and recursively replaced by the value of its immediate parent.

In most cases aggregation is done with an arithmetic mean (the default), and this is detailed in chapter 8 (pp. 190--198) of the CPI manual (2020). Aggregating with a non-arithmetic mean follows the same steps, except that the elemental indexes are aggregated with a mean of a different order (e.g., harmonic for a Paasche index), and the method for price updating the weights is slightly different.

Aggregating quote contributions uses the method in chapter 9 of the CPI manual (equations 9.26 and 9.28) when aggregating with an arithmetic mean. With a non-arithmetic mean, arithmetic weights are constructed using \code{\link[=transmute_weights]{transmute_weights(r, 1)}} in order to apply this method.

There may not be contributions for all prices relatives in an elemental aggregate if the elemental indexes are built from several sources (as with \code{merge()}). In this case the contribution for a price relative in the aggregated index will be correct, but the sum of all contributions will not equal the change in the value of the index. This can also happen when aggregating an already aggregated index in which missing index values have been imputed (i.e., when \code{na.rm = TRUE}).

The \code{vcov()} method is a simple wrapper to calculate the covariance matrix for an aggregated index when bootstrap replicate weights are available for the elemental aggregates. This approach ignores any sampling variance from the elemental indexes (which often use judgmental sampling), and ultimately depends on the method of generating replicate weights. It returns an array of covariance matrices for each \code{period}.

Indexes can be aggregated over time by taking the (usually arithmetic) mean of index values for each level over a rolling window. The \code{mean()} method constructs a set of rolling windows of length \code{window}, starting in the first period of the index, and takes the unweighted mean of each index value in these windows for each level of the index. The last window is discarded if it is incomplete, so that index values are always averaged over \code{window} periods. Note that quote contributions are discarded when aggregating over time.
}

\subsection{Combine}{
The \code{merge()} method combines two index objects with common time periods, merging together the index values and quote contributions for each time period in \code{x} and \code{y}. This is useful for building up an index when different elemental aggregates come from different sources of data, or use different index-number formulas. Currently this method only works for elemental indexes.

The \code{stack()} method combines two index objects with common levels, stacking index values and quote contributions for each level in \code{y} after those in \code{x}. The \code{unstack()} method breaks up \code{x} into a list of indexes, one for each period in \code{x}. These methods can be used in a map-reduce to make an index with multiple aggregation structures (like a Paasche index).

These methods return an object of the same class as \code{x}.
}

\subsection{Extract}{
The extraction method treats \code{x} as a matrix of index values with (named) rows for each \code{level} and columns for each \code{period} in \code{x}. Unlike a matrix, dimensions are never dropped; indexing \code{x} always returns a matrix. This means that indexing with a matrix is not possible, and only a submatrix can be extracted. As \code{x} is not an atomic vector, indexing with a single index like \code{x[1]} is taken to be the same as \code{x[1, ]}. The replacement method similarly treats \code{x} as a matrix, and behaves the same as replacing values in a matrix (except that \code{value} is coerced to numeric). Note that replacing the values of an index will remove the corresponding quote contributions (if any).

The \code{levels()} method extracts the levels of an index, and the \code{time()} method extracts the time periods of the index. The \code{start()} and \code{end()} methods extract the first and last time period.

The \code{head()} and \code{tail()} methods act as if \code{x} is a matrix of index values, and by default extract the time series for the first/last six levels of \code{x}.

The \code{summary()} method summarizes \code{x} as a matrix of index values (i.e., the five-number summary for each period). If there are quote contributions, then these are also summarized as a matrix.
}

\subsection{Coerce}{
The \code{as.matrix()} method turns an index into a matrix with a row for each level and a column for each period. The \code{as.data.frame()} method turns an index into a data frame with three columns: period, level, and value. 

\code{as_elemental_index()} is useful to form an elemental index from pre-computed values. Numeric matrices are coerce into an elemental index object by treating each column as a separate time period, and each row as an elemental aggregate. Column names are used to denote time periods, and row names are used to denote elemental aggregates (so they must be unique). If a dimension is unnamed, then it is given a sequential label from 1 to the size of that dimension. The default method coerces \code{x} to a matrix prior to using the matrix method. An aggregated index is coerced to an elemental index by simply extracting the elemental indexes (discarding the higher-level indexes).
}
}

\value{
Most of these functions return index objects, of class \code{ind}, which often behaves like a matrix with a row for each level of the index and a column for each time period.

\code{elemental_index()} returns an object of class \code{elem_ind}, inheriting from class \code{ind}, which has the following components.

\item{index}{A named list with an entry for each \code{period} that gives a named vector of index values for each level in \code{ea}.}
\item{contrib}{A named list with an entry for each \code{period}, which itself contains a named list with an entry for each level in \code{ea} with a named vector that gives the additive contribution for each price relative. If \code{contrib = FALSE}, then each of these vectors is of length 0.}
\item{levels}{The levels for \code{ea}.}
\item{time}{The levels for \code{period}.}
\item{has_contrib}{The value of \code{contrib}.}
\item{chain}{The value of \code{chain}, usually \code{TRUE}.}

\code{aggregate()} returns an aggregate index. This is an object of class \code{agg_ind}, inheriting from class \code{ind}, which has the following components.

\item{index}{A named list with an entry for each \code{period} in \code{x} that gives a named vector of index values for each level in \code{pias}.}
\item{contrib}{A named list with an entry for each \code{period}, which itself contains a list with an entry for each level in \code{pias} with a named vector that gives the additive contribution for each price relative.}
\item{levels}{The levels for \code{pias}.}
\item{time}{The levels for \code{period} from \code{x}.}
\item{has_contrib}{The value of \code{has_contrib} from \code{x}.}
\item{chain}{The value for \code{chain} from \code{x}, usually \code{TRUE.}}
\item{r}{The value for \code{r}, usually \code{1}.}
\item{pias}{A list containing the \code{child}, \code{parent}, \code{eas}, and \code{height} components of \code{pias}.}
}

\references{
Balk, B. M. (2008). \emph{Price and Quantity Index Numbers}. Cambridge University Press.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2020). \emph{Consumer Price Index Manual: Theory and Practice}. International Monetary Fund.
}

\author{
The \code{vcov()} method is based on a SAS routine by Justin Francis that was first ported to R by Ambuj Dewan.
}

\seealso{
\code{\link{price_relative}} for making price relatives for the same products over time, and \code{\link{carry_forward}} and \code{\link{shadow_price}} for imputations for missing prices.

\code{\link{aggregation_structure}} for making a price index aggregation structure.

\code{\link{aggregation_structure}} for making a price index aggregation structure.

\code{\link{chain}} for chaining period-over-period indexes.

\code{\link{contrib}} for extracting quote contributions.

The \code{sps_repweights()} function in the \pkg{sps} package to generate replicates weights when elemental aggregates are sampled using sequential Poisson sampling.
}

\examples{
prices <- data.frame(rel = 1:8, period = rep(1:2, each = 4), ea = rep(letters[1:2], 4))

# A two-level aggregation structure

pias <- aggregation_structure(list(c("top", "top", "top"), c("a", "b", "c")), 1:3)

# Calculate Jevons elemental indexes

(epr <- with(prices, elemental_index(rel, period, ea)))

# Same as using lm() or tapply()

exp(coef(lm(log(rel) ~ ea:factor(period) - 1, prices)))

with(prices, t(tapply(rel, list(period, ea), gpindex::geometric_mean, na.rm = TRUE)))

# Extract the indexes like a matrix

epr["a", ]

epr[, 2]

epr[1, ] <- 1 # can be useful for doing specific imputations

# Aggregate (note the imputation for elemental index 'c')

aggregate(epr, pias)

(index <- aggregate(epr, pias, na.rm = TRUE))

# Works for non-arithmetic indexes, too

aggregate(epr, pias, na.rm = TRUE, r = -1)

# Merge two indexes prior to aggregation

prices2 <- data.frame(rel = 1:8, period = rep(1:2, each = 4), ea = rep(letters[3:4], 4))
epr2 <- with(prices2, elemental_index(rel, period, ea))
aggregate(merge(epr, epr2), pias)

# Stack two indexes prior to aggregation

prices3 <- data.frame(rel = 1:8, period = rep(3:4, each = 4), ea = rep(letters[1:2], 4))
epr3 <- with(prices3, elemental_index(rel, period, ea))
aggregate(stack(epr, epr3), pias)

# Unstack does the reverse

all.equal(c(unstack(epr), unstack(epr3)), unstack(stack(epr, epr3)))

# Extract useful features of the index

head(index, 1)
tail(index, 3)
levels(index)
time(index)
start(index)
end(index)

summary(index)

# Turn the index into a data frame/matrix

as.data.frame(index)
as.matrix(index)

# Calculate a CSWD index as a geometric index by constructing appropriate weights

library(gpindex)

fw <- function() {
  a2g <- transmute_weights(1, 0)
  h2g <- transmute_weights(-1, 0)
  function(x, w1, w2) {
      scale_weights(a2g(x, w1)) + scale_weights(h2g(x, w2))
  }
}

with(
    prices, 
    elemental_index(rel, period, ea, grouped(fw())(rel, group = interaction(period, ea)))
)
}