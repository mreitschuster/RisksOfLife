% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Graph_getAllPaths.R
\name{recursiveStep_getPath}
\alias{recursiveStep_getPath}
\title{recursive function to find the next step from node to node}
\usage{
recursiveStep_getPath(Rgraph, endNode, onlyActive, hop_number, paths)
}
\arguments{
\item{Rgraph}{The Rgraph object (package specific object to save the graph)}

\item{endNode}{Node to find}

\item{onlyActive}{Flag if we should consider only active edges of the graph}

\item{hop_number}{just counting. used to maintain integrity of data structure}

\item{paths}{the path we already use. this is necessary to avoid circular references}
}
\value{
new_paths
}
\description{
recursive function to find the next step from node to node
}
