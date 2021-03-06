% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-data.R
\name{add_precursors}
\alias{add_precursors}
\title{add_precursors}
\usage{
add_precursors(x, ...)
}
\arguments{
\item{x}{An object}

\item{...}{Names of precursor objects (character)}
}
\value{
\code{x} with units appended to any existing comments.
}
\description{
Add names of precursors (objects that contribute to the computation)
to a data system object. This allows for granular tracking
of dependencies across the data system.
}
