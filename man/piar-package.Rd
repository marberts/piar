\docType{package}
\name{piar-package}
\alias{piar-package}
\alias{piar}

\title{Price Index Aggregation}

\description{
Most price indexes are made with a two-step procedure, where period-over-period \emph{elemental indexes} are first calculated for a collection of \emph{elemental aggregates} at each point in time, and then aggregated according to a \emph{price index aggregation structure}. These indexes can then be chained together to form a time series that gives the evolution of prices with respect to a fixed base period. This package contains a collections of functions that revolve around this work flow, making it easy to build standard price indexes, and implement the methods described by Balk (2008), von der Lippe (2001), and the CPI manual (2020) for bilateral price indexes.
}

\section{Usage}{
The vignette \emph{Making price indexes} gives several extended examples of how to use the functions in this package to make different types of price indexes. Run \code{vignette("making_price_indexes", "piar")} to view it. But the basic work flow is fairly simple. 

The starting point is to make period-over-period elemental price indexes with the \code{\link[=elemental_index]{elemental_index()}} function and an aggregation structure with the \code{\link[=aggregation_structure]{aggregation_structure()}} function. The \code{\link[=aggregate.ind]{aggregate()}} method can then be used to aggregate the elemental indexes according to the aggregation structure. There are a variety of methods to work with these index objects, such as chaining them over time. 

The two-step workflow is described in chapter 8 of the CPI manual (2020) and chapter 5 of Balk (2008). A practical overview is given by Chiru et al. (2015) for the Canadian CPI, and a detailed discussion of chaining indexes is given by von der Lippe (2001).
}

\note{
This package is designed to work with \emph{bilateral} price indexes. The \pkg{IndexNumR} package on the CRAN has support for making \emph{multilateral} price indexes.
}

\references{
Balk, B. M. (2008). \emph{Price and Quantity Index Numbers}. Cambridge University Press.

Chiru, R., Huang, N., Lequain, M. Smith, P., and Wright, A. (2015). \emph{The Canadian Consumer Price Index Reference Paper}, Statistics Canada catalogue 62-553-X. Statistics Canada.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2020). \emph{Consumer Price Index Manual: Theory and Practice}. International Monetary Fund.

von der Lippe, P. (2001). \emph{Chain Indices: A Study in Price Index Theory}, Spectrum of Federal Statistics vol. 16. Federal Statistical Office, Wiesbaden.
}

\author{
\strong{Maintainer}: Steve Martin \email{steve.martin@statcan.gc.ca}
}
