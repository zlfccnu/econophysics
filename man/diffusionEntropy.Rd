% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diffusionEntropy.R
\name{diffusionEntropy}
\alias{diffusionEntropy}
\title{use the difussion entropy method to calculate the Hurst exponent}
\usage{
diffusionEntropy(x, nVec = 2^(3:8), thread = 3, binSizeRatio = 0.5)
}
\arguments{
\item{x}{the time series}

\item{nVec}{the time scale}

\item{thread}{the multi threads}

\item{binSizeRatio}{the boxsize, fraction of standard deviation}
}
\value{
a data.frame
}
\description{
use the difussion entropy method to calculate the Hurst exponent
}
