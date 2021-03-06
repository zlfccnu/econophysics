% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/layout_community.R
\name{layout_community}
\alias{layout_community}
\title{Function used to layout each sub community as the same layout}
\usage{
layout_community(graph, community_algorithm, layout_desired)
}
\arguments{
\item{graph}{A igraph object}

\item{community_algorithm}{the algorithm used to detect the community}

\item{layout_desired}{the layout for each sub community}

\item{algorithm}{A character name for the community detection method}

\item{layout}{A character name for the layout function you use}
}
\value{
A data frame with four columns, the vertex label, the membership of each node, two columns of the coordinates
}
\description{
Function used to layout each sub community as the same layout
}
