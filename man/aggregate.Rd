\name{aggregate}
\alias{aggregate.ind}
\alias{[.ind}
\alias{[<-.ind}
\alias{mean.ind}
\alias{is_aggregate_index}
\alias{is_index}
\alias{vcov.agg_ind}
\alias{as.data.frame.ind}
\alias{as.matrix.ind}
\alias{merge.ind}
\alias{stack.ind}
\alias{unstack.ind}
\alias{levels.ind}
\alias{time.ind}
\alias{start.ind}
\alias{end.ind}
\alias{head.ind}
\alias{tail.ind}
\alias{summary.ind}

\title{Aggregate elemental indexes}

\description{
Methods to aggregate period-over-period or fixed-based elemental price indexes using a price index aggregation structure, or average indexes over time.
}

\usage{
\method{aggregate}{ind}(x, pias, na.rm = FALSE, r = 1, ...)

\method{[}{ind}(x, i, j)

\method{[}{ind}(x, i, j) <- value

\method{mean}{ind}(x, window = 3, na.rm = FALSE, r = 1, ...)

is_index(x)

is_aggregate_index(x)

\method{vcov}{agg_ind}(object, repweights, mse = TRUE, ...)
}

\arguments{
\item{x}{A price index, usually period-over-period (or fixed-based) elemental price indexes. These can be made with \code{\link[=elemental_index]{elemental_index()}}.}

\item{pias}{A price index aggregation structure. This can be made with \code{\link[=aggregation_structure]{aggregation_structure()}}.}

\item{na.rm}{Should missing values in \code{x} be removed? By default, missing values are not removed. Setting \code{na.rm = TRUE} is equivalent to overall mean imputation.}

\item{r}{Order of the generalized mean used to aggregate the price indexes in \code{x}: 1 for an arithmetic index (the default), 0 for a geometric index, or -1 for a harmonic index. Other values are possible; see \code{\link[=generalized_mean]{generalized_mean()}} for details.}

\item{i, j, value}{See \link{Extract}, with \code{value} being a numeric vector (or something that can coerced into one.)}

\item{window}{The size of the rolling-window used to average index values across time periods. The default (3) turns a monthly index into into a quarterly one.}

\item{object}{An aggregate price index, as made by \code{aggregate()}.}

\item{repweights}{A matrix, or something that can be coerced into one, of bootstrap replicate weights with a row for each elemental aggregate and a column for each replicate.}

\item{mse}{Should covariances be centered off the value of the index in \code{object} (the default), or the mean of the replicates?}

\item{...}{Further arguments passed to or used by methods.}
}

\details{
The \code{aggregate()} method aggregates elemental indexes by looping over each time period in \code{x} and
\enumerate{
\item aggregating the elemental indexes (with \code{\link[=generalized_mean]{generalized_mean(r)}});
\item aggregating quote contributions in each level of \code{pias} (if there are any);
\item price updating the weights in \code{pias} with \code{\link[=factor_weights]{factor_weights(r)}} (only for period-over-period elemental indexes, i.e., \code{is_chained_index(x) == TRUE}).
}
The result is a collection of aggregated period-over-period indexes that can be chained together to get a fixed-base index when \code{x} are period-over-period elemental indexes. Otherwise, when \code{x} are fixed-base elemental indexes, the result is a collection of aggregated fixed-base indexes.

By default, any missing elemental indexes will propagate when aggregating the index. Setting \code{na.rm = TRUE} ignores missing values, and is equivalent to parental (or overall mean) imputation. As an aggregated price index generally cannot have missing values (for otherwise it can't be chained over time), any missing values for a level of \code{pias} are removed and recursively replaced by the value of its immediate parent.

In most cases aggregation is done with an arithmetic mean (the default), and this is detailed in chapter 8 (pp. 190--198) of the CPI manual (2020). Aggregating with a non-arithmetic mean follows the same steps, except that the elemental indexes are aggregated with a mean of a different order (e.g., harmonic for a Paasche index), and the method for price updating the weights is slightly different.

Aggregating quote contributions uses the method in chapter 9 of the CPI manual (equations 9.26 and 9.28) when aggregating with an arithmetic mean. With a non-arithmetic mean, arithmetic weights are constructed using \code{\link[=transmute_weights]{transmute_weights(r, 1)}} in order to apply this method.

There may not be contributions for all prices relatives in an elemental aggregate if the elemental indexes are built from several sources (as with \code{merge()}). In this case the contribution for a price relative in the aggregated index will be correct, but the sum of all contributions will not equal the change in the value of the index. This can also happen when aggregating an already aggregated index in which missing index values have been imputed (i.e., when \code{na.rm = TRUE}).

The extraction method treats \code{x} as a matrix of index values with (named) rows for each \code{level} and columns for each \code{period} in \code{x}. Unlike a matrix, dimensions are never dropped; indexing \code{x} always returns a matrix. This means that indexing with a matrix is not possible, and only a submatrix can be extracted. As \code{x} is not an atomic vector, indexing with a single index like `\code{x[1]} is taken to be the same as \code{x[1, ]}. The replacement method similarly treats \code{x} as a matrix, and behaves the same as replacing values in a matrix (except that \code{value} is coerced to numeric). Note that replacing the values of an index will remove the corresponding quote contributions (if any).

Indexes can be aggregated over time by taking the (usually arithmetic) mean of each index value over a rolling window. The \code{mean()} method constructs a set of rolling windows of length \code{window}, starting in the first period of the index, and takes the unweighted mean of each index value in these windows for each level of the index. The last window is discarded if it is incomplete, so that index values are always averaged over \code{window} periods. Note that quote contributions are discarded when aggregating over time.

The \code{vcov()} method is a simple wrapper to calculate the covariance matrix for an aggregated index when bootstrap replicate weights are available for the elemental aggregates. This approach ignores any sampling variance from the elemental indexes (which often use judgemental sampling), and ultimately depends on the method of generating replicate weights.
}

\value{
\code{aggregate()} returns an aggregate index. This is an object of class \code{agg_ind}, inheriting from class \code{ind}, which has the following components.

\item{index}{A named list with an entry for each \code{period} in \code{x} that gives a named vector of index values for each level in \code{pias}.}
\item{contrib}{A named list with an entry for each \code{period}, which itself contains a list with an entry for each level in \code{pias} with a named vector that gives the additive contribution for each price relative.}
\item{levels}{The levels for \code{pias}.}
\item{time}{The levels for \code{period} from \code{x}.}
\item{has_contrib}{The value of \code{has_contrib} from \code{x}.}
\item{chain}{The value for \code{chain} from \code{x}, usually \code{TRUE.}}
\item{r}{The value for \code{r}, usually \code{1}.}
\item{pias}{A list containing the \code{child}, \code{parent}, \code{eas}, and \code{height} components of \code{pias}.}

The \code{is_aggregate_index()} and \code{is_index()} return \code{TRUE} if \code{x} is an aggregate index and if \code{x} is any kind of index.

The \code{vcov()} method returns an array of covariance matrices for each \code{period}.
}

\references{
ILO, IMF, OECD, Eurostat, UN, and World Bank. (2020). \emph{Consumer Price Index Manual: Theory and Practice}. International Monetary Fund.
}

\author{
The \code{vcov()} method is based on a SAS routine by Justin Francis that was first ported to R by Ambuj Dewan.
}

\seealso{
\code{\link{aggregation_structure}} for making a price index aggregation structure.

\code{\link{elemental_index}} for making elemental price indexes.

\code{\link{chain}} for chaining period-over-period indexes.

\code{\link{contrib}} for extracting quote contributions.

The \code{sps_repweights()} function in the \pkg{sps} package to generate replicates weights when elemental aggregates are sampled using sequential Poisson sampling.
}

\examples{
prices <- data.frame(rel = 1:8, period = rep(1:2, each = 4), ea = rep(letters[1:2], 4))

# A two-level aggregation structure

pias <- aggregation_structure(list(c("top", "top", "top"), c("a", "b", "c")), 1:3)

# Calculate elemental indexes

epr <- with(prices, elemental_index(rel, period, ea))

# Extract the indexes like a matrix

epr["a", ]

epr[, 1:2]

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
}