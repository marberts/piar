#' Price Index Aggregation
#' 
#' Most price indexes are made with a two-step procedure, where
#' period-over-period *elemental indexes* are first calculated for a
#' collection of *elemental aggregates* at each point in time, and then
#' aggregated according to a *price index aggregation structure*. These
#' indexes can then be chained together to form a time series that gives the
#' evolution of prices with respect to a fixed base period. This package
#' contains a collections of functions that revolve around this work flow,
#' making it easy to build standard price indexes, and implement the methods
#' described by Balk (2008), von der Lippe (2001), and the CPI manual (2020)
#' for bilateral price indexes.
#' 
#' 
#' @name piar-package
#' @aliases piar-package piar
#' @docType package
#' @section Usage: The vignette *Making price indexes* gives several
#' extended examples of how to use the functions in this package to make
#' different types of price indexes. Run `vignette("making_price_indexes",
#' "piar")` to view it. But the basic work flow is fairly simple.
#' 
#' The starting point is to make period-over-period elemental price indexes
#' with the [`elemental_index()`][elemental_index] function and an
#' aggregation structure with the
#' [`aggregation_structure()`][aggregation_structure] function. The
#' [`aggregate()`][aggregate.piar_index] method can then be used to
#' aggregate the elemental indexes according to the aggregation structure.
#' There are a variety of methods to work with these index objects, such as
#' chaining them over time.
#' 
#' The two-step workflow is described in chapter 8 of the CPI manual (2020) and
#' chapter 5 of Balk (2008). A practical overview is given by Chiru et al.
#' (2015) for the Canadian CPI, and a detailed discussion of chaining indexes
#' is given by von der Lippe (2001).
#' @author **Maintainer**: Steve Martin \email{stevemartin041@@gmail.com}
#' @seealso <https://github.com/marberts/piar>
#' @references Balk, B. M. (2008). *Price and Quantity Index Numbers*.
#' Cambridge University Press.
#' 
#' Chiru, R., Huang, N., Lequain, M. Smith, P., and Wright, A. (2015).
#' *The Canadian Consumer Price Index Reference Paper*, Statistics Canada
#' catalogue 62-553-X. Statistics Canada.
#' 
#' ILO, IMF, OECD, Eurostat, UN, and World Bank. (2020). *Consumer Price
#' Index Manual: Theory and Practice*. International Monetary Fund.
#' 
#' von der Lippe, P. (2001). *Chain Indices: A Study in Price Index
#' Theory*, Spectrum of Federal Statistics vol. 16. Federal Statistical Office,
#' Wiesbaden.
NULL









