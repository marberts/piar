\name{aggregate}
\alias{aggregate.index}
\alias{weights.aggregate}
\alias{update.aggregate}
\alias{vcov.aggregate}

\title{Aggregate elemental indexes}

\description{
A method to aggregate period-over-period or fixed-base elemental price indexes using a price index aggregation structure.
}

\usage{
\method{aggregate}{index}(x, pias, chained = TRUE, na.rm = FALSE, r = 1, ...)

\method{weights}{aggregate}(object, ...)

\method{update}{aggregate}(object, period = end(object), ...)

\method{vcov}{aggregate}(object, repweights, mse = TRUE, ...)
}

\arguments{
\item{x}{Period-over-period or fixed-base elemental price indexes. These can be made with \code{\link[=elemental_index]{elemental_index()}}.}

\item{pias}{A price index aggregation structure. This can be made with \code{\link[=aggregation_structure]{aggregation_structure()}}.}

\item{chained}{Should the weights in \code{pias} be price-updated for a chained calculation with period-over-period elemental indexes (the default), or should the weight be kept constant for a fixed-base index with fixed-base elemental indexes?}

\item{na.rm}{Should missing values in \code{x} be removed? By default, missing values are not removed. Setting \code{na.rm = TRUE} is equivalent to parental imputation.}

\item{r}{Order of the generalized mean used to aggregate the elemental price indexes: 0 for a geometric index, 1 for an arithmetic index (the default), or -1 for a harmonic index. Other values are possible; see \code{\link[=generalized_mean]{generalized_mean()}} for details.}

\item{object}{An object of class \code{aggregate}, as made by \code{aggregate()}.}

\item{period}{The time period used to price update the weights. The default uses last period in \code{object}.}

\item{repweights}{A matrix of bootstrap replicate weights with a row for each elemental aggregate and a column for each replicate.}

\item{mse}{Should covariances be centered off the value of the index in \code{object} (the default), or the mean of the replicates?}

\item{...}{Further arguments passed to or used by methods.}
}

\details{
The \code{aggregate()} method aggregates elemental indexes by looping over each time period in \code{x} and
\enumerate{
\item aggregating the elemental indexes (with \code{\link[=generalized_mean]{generalized_mean(r)}});
\item aggregating quote contributions in each level of \code{pias} (if there are any);
\item price updating the weights in \code{pias} if \code{chained = TRUE} (with \code{\link[=factor_weights]{factor_weights(r)}}).
}
The result is a collection of aggregated period-over-period indexes that can be chained together to get a fixed-base index when \code{x} are period-over-period elemental indexes and \code{chained = TRUE}. Otherwise, when \code{x} are fixed-base elemental indexes and \code{chained = FALSE}, the result is a collection of aggregated fixed-base indexes.

By default, any missing elemental indexes will propagate when aggregating the index. Setting \code{na.rm = TRUE} ignores missing values, and is equivalent to parental (or overall mean) imputation. As an aggregated price index generally cannot have missing values (for otherwise it can't be chained over time), any missing values for a level of \code{pias} are removed and recursively replaced by the value of its immediate parent.

In most cases aggregation is done with an arithmetic mean (the default), and this is detailed in chapter 8 (pp. 190--198) of the CPI manual (2020). Aggregating with a non-arithmetic mean follows the same steps, except that elemental indexes are aggregated with a mean of a different order (e.g., harmonic for a Paasche index), and the method for price updating the weights is slightly different.

Aggregating quote contributions uses the method in chapter 9 of the CPI manual (equations 9.26 and 9.28) when aggregating with an arithmetic mean. With a non-arithmetic mean, arithmetic weights are constructed using \code{\link[=transmute_weights]{transmute_weights(r, 1)}} in order to apply this method.

There may not be contributions for all prices relatives in an elemental aggregate if the elemental indexes are built from several sources (as with \code{\link[=merge.index]{merge()}}). In this case the contribution for a price relative in the aggregated index will be correct, but the sum of all contributions will not equal the change in the value of the index. This can also happen when aggregating an already aggregated index in which missing index values have been imputed (i.e., when \code{na.rm = TRUE}).

The \code{vcov()} method is a simple wrapper to calculate the covariance matrix for an aggregated index when bootstrap replicate weights are available for the elemental aggregates. This approach ignores any sampling variance from the elemental indexes (which often use judgemental sampling), and ultimately depends on the method of generating replicate weights.
}

\value{
An object of class \code{aggregate}, inheriting from class \code{index}, which has the following components.

\item{index}{A named list with an entry for each \code{period} in \code{x} that gives a named vector of index values for each level in \code{pias}.}
\item{contributions}{A named list with an entry for each \code{period}, which itself contains a list with an entry for each level in \code{pias} with a named vector that gives the additive contribution for each price relative.}
\item{levels}{The levels for \code{pias}.}
\item{periods}{The levels for \code{period}.}
\item{weights}{A named list with an entry for each \code{period} that contains the weights used to aggregate the elemental indexes for each period.}
\item{r, chained}{The values for \code{r} and \code{chained}, usually \code{1} and \code{TRUE}.}
\item{pias}{A list containing the \code{child}, \code{parent}, \code{eas}, and \code{height} components of \code{pias}.}

The \code{weights()} method extracts the matrix of weights used to aggregate the elemental indexes in each \code{period}.

The \code{update()} method price-updates the weights in the aggregation structured used to make \code{object}, and returns the resulting object of class \code{pias}.

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

\code{\link[=index_methods]{index methods}} for methods to extract index values/quote contributions, chain an index, and merge indexes together.

The \code{sps_repweights()} function in the \pkg{sps} package to generate replicates weights when elemental aggregates are sampled using sequential Poisson sampling.
}

\examples{
prices <- data.frame(rel = 1:8, period = rep(1:2, each = 4), ea = rep(letters[1:2], 4))

# A two-level aggregation structure

pias <- aggregation_structure(list(c("top", "top", "top"), c("a", "b", "c")), 1:3)

# Calculate elemental indexes

epr <- with(prices, elemental_index(rel, period, ea))

# Aggregate (note the imputation for elemental index 'c')

aggregate(epr, pias)

aggregate(epr, pias, na.rm = TRUE)

# Works for non-arithmetic indexes, too

aggregate(epr, pias, na.rm = TRUE, r = -1)

# Extract the weights

weights(aggregate(epr, pias, na.rm = TRUE))
}