% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/posterior_draws.R
\name{posterior_draws}
\alias{posterior_draws}
\title{Extract Posterior Draws or Bootstrap Resamples from \code{marginaleffects} Objects}
\usage{
posterior_draws(x, shape = "long")
}
\arguments{
\item{x}{An object produced by a \code{marginaleffects} package function, such as \code{predictions()}, \code{avg_slopes()}, \code{hypotheses()}, etc.}

\item{shape}{string indicating the shape of the output format:
\itemize{
\item "long": long format data frame
\item "DxP": Matrix with draws as rows and parameters as columns
\item "PxD": Matrix with draws as rows and parameters as columns
\item "rvar": Random variable datatype (see \code{posterior} package documentation).
}}
}
\value{
A data.frame with \code{drawid} and \code{draw} columns.
}
\description{
Extract Posterior Draws or Bootstrap Resamples from \code{marginaleffects} Objects
}
