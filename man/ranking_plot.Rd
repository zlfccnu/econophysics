% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ranking_plot.r
\name{ranking_plot}
\alias{ranking_plot}
\title{ranking plot of countries fro the uncomtrade data}
\usage{
ranking_plot(net_list, Y, n = 10, mode = c("import", "export"))
}
\arguments{
\item{net_list, }{a list of tidygraph/igraph objects, with year names}

\item{Y, }{the specific year index for the net_list}

\item{n, }{the number of countries to plot the ranking plot}

\item{mode, }{plot the import or export ranking}
}
\value{
a ggplot object
}
\description{
ranking plot of countries fro the uncomtrade data
}
