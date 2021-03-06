% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TCI_INDEX.r
\name{TCI}
\alias{TCI}
\title{the trade complementarity index}
\usage{
TCI(trade_data, commodity_code, Year = TRUE, country_names = NULL)
}
\arguments{
\item{trade_data}{the tibble of trade data}

\item{commodity_code}{a vector of commodity codes}

\item{Year}{logical, whether split the trade_data tibble by year or not}

\item{country_names}{the country names that used to calculate the TCI pairs, if its NULL then will determine automatically}
}
\value{
a TCI matrix list(split the trade data by year) or a single matrix
}
\description{
the trade complementarity index
}
\references{
Trade Intensities and the Analysis of Bilateral Trade Flows in a Many-Country World : A Survey,1982
}
