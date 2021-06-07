\docType{package}
\name{piar-package}
\alias{piar-package}
\alias{piar}

\title{Price Index Aggregation in R}

\description{
Most price indexes are made with a two-step procedure, where period-over-period \emph{elemental indexes} are first calculated for a collection of \emph{elemental aggregates} at each point in time, and then aggregated according to a \emph{price index aggregation structure}. These indexes can then be chained together to form a time series that gives the evolution of prices with respect to a fixed base period. This package contains a collections of functions that revolve around this work flow, making it easy to build standard price indexes.
}

\section{Usage}{
The vignette \emph{Making price indexes} gives several extended examples of how to use the functions in this package to make different types of price indexes. Run \code{vignette("making_price_indexes", "piar")} to view it. But the basic work flow is fairly simple. 

The starting point is to make period-over-period elemental price indexes with the \code{\link[=elemental_index]{elemental_index()}} function and an aggregation structure with the \code{\link[=aggregation_structure]{aggregation_structure()}} function. The \code{\link[=aggregate.index]{aggregate()}} method can then be used to aggregate the elemental indexes according to the aggregation structure. There are a variety of \code{\link[=index_methods]{index methods}} to work with these index objects, including chaining them over time. This workflow is described in chapter 9 of the PPI manual.
}

\note{
This package is designed to work with \emph{bilateral} price indexes. The \code{'IndexNumR'} package on the CRAN has support for making \emph{multilateral} price indexes.
}

\references{
ILO, IMF, OECD, Eurostat, UN, and World Bank. (2004). \emph{Producer Price Index Manual: Theory and Practice}. International Monetary Fund.
}

\author{
\strong{Maintainer}: Steve Martin \email{steve.martin5@canada.ca}
}
