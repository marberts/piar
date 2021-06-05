\name{aggregate}
\alias{aggregate.index}

\title{Aggregate elemental price indexes}

\description{
A method to aggregate period-over-period elemental price indexes using a price index aggregation structure.
}

\usage{
\method{aggregate}{index}(x, pias, na.rm = FALSE, r = 1, ...)
}

\arguments{
\item{x}{Period-over-period elemental price indexes. These can be made with \code{\link[=elemental_index]{elemental_index()}}.}

\item{pias}{A price index aggregation structure. This can be made with \code{\link[=aggregation_structure]{aggregation_structure()}}.}

\item{na.rm}{Should missing values in \code{x} be removed? By default, missing values are not removed. Setting to \code{TRUE} is equivalent to parental imputation.}

\item{r}{Order of the price index used to aggregate the elemental price indexes: 0 for a geometric index, 1 for an arithmetic index (the default), or -1 for a harmonic index. Other values are possible; see \code{\link[=generalized_mean]{generalized_mean()}} for details.}

\item{...}{Further arguments passed to or used by methods.}
}

\details{
This function aggregates period-over-period elemental indexes by looping over each time period in \code{x} and
\enumerate{
\item aggregating the elemental indexes and quote contributions in each level of \code{pias};
\item price updating the weights in \code{pias}.
}
The result is a collection of aggregated period-over-period indexes that can be chained together to get a fixed-base index. See section C in chapter 9 of the PPI manual for details when aggregating with an arithmetic index (the default).

By default, any missing elemental indexes will propagate when aggregating the index. Setting \code{na.rm = TRUE} ignores missing values, and is equivalent to parental imputation. As a price index generally cannot have missing values (for otherwise it can't be chained over time), any missing values for a level of \code{pias} are recursively imputed by the value of its immediate parent.

Aggregating quote contributions uses a generalization of the method in chapter 9, section C.8, of the PPI manual (equation 9.25) that is applicable to non-arithmetic indexes.
}

\value{
An object of class \code{'aggregate'}, inheriting from class \code{'index'}, which has the following components.

\item{index}{A named list with an entry for each \code{period} in \code{x} that gives a named vector of index values for each level in \code{pias}.}
\item{contributions}{A named list with an entry for each \code{period}, which itself contains a list with an entry for each level in \code{pias} with a named vector that gives the additive contribution for each price relative.}
\item{levels}{The levels for \code{pias}.}
\item{periods}{The levels for \code{period}.}
\item{weights}{A named list with an entry for each \code{period} that contains the price updated weights from \code{pias}.}
}

\references{
ILO, IMF, OECD, Eurostat, UN, and World Bank. (2004). Producer Price Index Manual: Theory and Practice. International Monetary Fund.
}

\seealso{
\code{\link{aggregation_structure}} for making a price index aggregation structure.

\code{\link{elemental_index}} for making elemental price indexes.

\code{\link[=index_methods]{index methods}} for methods to extract index values/quote contributions, chain an index, and merge indexes together.
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

# Extract the price updated weights

weights(aggregate(epr, pias, na.rm = TRUE))
}