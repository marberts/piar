\name{aggregate}
\alias{aggregate.pindex}
\alias{mean.pindex}

\title{Aggregate elemental price indexes}

\description{
Aggregate elemental price indexes with a price index aggregation structure, or average them over subperiods.
}

\usage{
\method{aggregate}{pindex}(x, pias, na.rm = FALSE, r = 1, ...)

\method{mean}{pindex}(x, w, window = 3, na.rm = FALSE, r = 1, ...)
}

\arguments{
\item{x}{A price index, usually made by \code{\link[=elemental_index]{elemental_index()}}.}

\item{pias}{A price index aggregation structure. This can be made with \code{\link[=aggregation_structure]{aggregation_structure()}}.}

\item{na.rm}{Should missing values be removed? By default, missing values are not removed. Setting \code{na.rm = TRUE} is equivalent to overall mean imputation.}

\item{r}{Order of the generalized mean to aggregate index values. 0 for a geometric index (the default for making elemental indexes), 1 for an arithmetic index (the default for aggregating elemental indexes and averaging indexes over subperiods), or -1 for a harmonic index (usually for a Paasche index). Other values are possible; see \code{\link[=generalized_mean]{generalized_mean()}} for details.}

\item{w}{A numeric vector of weights for the price relatives in \code{rel}/index values in \code{x}. The default is equal weights.}

\item{window}{The size of the window used to average index values across subperiods. The default (3) turns a monthly index into into a quarterly one.}

\item{...}{Further arguments passed to or used by methods.}
}

\details{
The \code{aggregate()} method aggregates elemental indexes by looping over each time period in \code{x} and
\enumerate{
\item aggregates the elemental indexes with \code{\link[=generalized_mean]{generalized_mean(r)}} for each level of \code{pias};
\item aggregates quote contributions for each level of \code{pias} (if there are any);
\item price updates the weights in \code{pias} with \code{\link[=factor_weights]{factor_weights(r)}} (only for period-over-period elemental indexes, i.e., \code{is_chainable_index(x) == TRUE}).
}
The result is a collection of aggregated period-over-period indexes that can be chained together to get a fixed-base index when \code{x} are period-over-period elemental indexes. Otherwise, when \code{x} are fixed-base elemental indexes, the result is a collection of aggregated fixed-base (direct) indexes.

By default, missing elemental indexes will propagate when aggregating the index. Missing elemental indexes can be due to both missingness of these values in \code{x}, and the presence of elemental aggregates in \code{pias} that are not part of \code{x}. Setting \code{na.rm = TRUE} ignores missing values, and is equivalent to parental (or overall mean) imputation. As an aggregated price index generally cannot have missing values (for otherwise it can't be chained over time), any missing values for a level of \code{pias} are removed and recursively replaced by the value of its immediate parent.

In most cases aggregation is done with an arithmetic mean (the default), and this is detailed in chapter 8 (pp. 190--198) of the CPI manual (2020). Aggregating with a non-arithmetic mean follows the same steps, except that the elemental indexes are aggregated with a mean of a different order (e.g., harmonic for a Paasche index), and the method for price updating the weights is slightly different.

Aggregating quote contributions uses the method in chapter 9 of the CPI manual (equations 9.26 and 9.28) when aggregating with an arithmetic mean. With a non-arithmetic mean, arithmetic weights are constructed using \code{\link[=transmute_weights]{transmute_weights(r, 1)}} in order to apply this method.

There may not be contributions for all prices relatives in an elemental aggregate if the elemental indexes are built from several sources (as with \code{\link[=merge.pindex]{merge()}}). In this case the contribution for a price relative in the aggregated index will be correct, but the sum of all contributions will not equal the change in the value of the index. This can also happen when aggregating an already aggregated index in which missing index values have been imputed (i.e., when \code{na.rm = TRUE}).

Indexes can be aggregated over subperiods by taking the (usually arithmetic) mean of index values for each level over consecutive windows of subperiods. The \code{mean()} method constructs a set of windows of length \code{window}, starting in the first period of the index, and takes the unweighted mean of each index value in these windows for each level of the index. The last window is discarded if it is incomplete, so that index values are always averaged over \code{window} periods. The names for the first time period in each window form the new names for the aggregated time periods. Note that quote contributions are discarded when aggregating over subperiods. 

An optional vector of weights can be specified when aggregating index values over subperiods, which is often useful when aggregating a Paasche index; see section 4.3 of Balk (2008) for details. It is usually easiest to specify these weights as a matrix with a row for each index value in \code{x} and a column for each time period.
}

\value{
\code{aggregate()} returns an aggregate index. This is an object of class \code{agg_pindex}, inheriting from class \code{pindex}, which has the following components.

\item{index}{A named list with an entry for each \code{period} in \code{x} that gives a named vector of index values for each level in \code{pias}.}
\item{contrib}{A named list with an entry for each \code{period}, which itself contains a list with an entry for each level in \code{pias} with a named vector that gives the additive contribution for each price relative.}
\item{levels}{The levels for \code{pias}.}
\item{time}{The levels for \code{period} from \code{x}.}
\item{has_contrib}{The value of \code{has_contrib} from \code{x}.}
\item{chainable}{The value for \code{chainable} from \code{x}, usually \code{TRUE.}}
\item{r}{The value for \code{r}, usually \code{1}.}
\item{pias}{A list containing the \code{child}, \code{parent}, \code{eas}, and \code{height} components of \code{pias}.}

\code{mean()} returns a index of the same type as \code{x}.
}

\references{
Balk, B. M. (2008). \emph{Price and Quantity Index Numbers}. Cambridge University Press.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2020). \emph{Consumer Price Index Manual: Theory and Practice}. International Monetary Fund.
}

\seealso{
\code{\link{aggregation_structure}} for making a price index aggregation structure.

\code{\link{chain}} for chaining period-over-period indexes, and \code{\link{rebase}} for rebasing an index.

\code{\link{contrib}} for extracting quote contributions.

\code{\link{index-methods}} for other methods.
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

(index <- aggregate(epr, pias, na.rm = TRUE))

# Aggregation can equivalently be done as matrix multiplication

as.matrix(pias) \%*\% as.matrix(chain(index[letters[1:3]]))

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

all.equal(as_index(as.data.frame(epr)), epr)
all.equal(as_index(as.matrix(epr)), epr)

# Calculate a CSWD index (same as the Jevons in this example) 
# as an arithmetic index by constructing appropriate weights

library(gpindex)

# A general function to calculate weights to turn the geometric
# mean of the arithmetic and harmonic mean (i.e., Fisher mean)
# into an arithmetic mean

fw <- grouped(nested_transmute(0, c(1, -1), 1))

with(
    prices, 
    elemental_index(rel, period, ea, fw(rel, group = interaction(period, ea)), r = 1)
)
}