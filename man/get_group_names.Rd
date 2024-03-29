% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_group_names.R, R/methods_MASS.R,
%   R/methods_nnet.R, R/methods_brglm2.R, R/methods_brms.R, R/methods_mclogit.R,
%   R/methods_mlm.R, R/methods_ordinal.R, R/methods_pscl.R, R/methods_survey.R
\name{get_group_names}
\alias{get_group_names}
\alias{get_group_names.default}
\alias{get_group_names.polr}
\alias{get_group_names.multinom}
\alias{get_group_names.bracl}
\alias{get_group_names.brmsfit}
\alias{get_group_names.mblogit}
\alias{get_group_names.mlm}
\alias{get_group_names.clm}
\alias{get_group_names.hurdle}
\alias{get_group_names.svyolr}
\title{Get levels of the outcome variable in grouped or multivariate models}
\usage{
get_group_names(model, ...)

\method{get_group_names}{default}(model, ...)

\method{get_group_names}{polr}(model, ...)

\method{get_group_names}{multinom}(model, ...)

\method{get_group_names}{bracl}(model, ...)

\method{get_group_names}{brmsfit}(model, ...)

\method{get_group_names}{mblogit}(model, type, ...)

\method{get_group_names}{mlm}(model, ...)

\method{get_group_names}{clm}(model, ...)

\method{get_group_names}{hurdle}(model, type = "count", ...)

\method{get_group_names}{svyolr}(model, ...)
}
\arguments{
\item{model}{Model object}

\item{...}{Additional arguments are passed to the \code{predict()} method
supplied by the modeling package.These arguments are particularly useful
for mixed-effects or bayesian models (see the online vignettes on the
\code{marginaleffects} website). Available arguments can vary from model to
model, depending on the range of supported arguments by each modeling
package. See the "Model-Specific Arguments" section of the
\code{?slopes} documentation for a non-exhaustive list of available
arguments.}

\item{type}{string indicates the type (scale) of the predictions used to
compute contrasts or slopes. This can differ based on the model
type, but will typically be a string such as: "response", "link", "probs",
or "zero". When an unsupported string is entered, the model-specific list of
acceptable values is returned in an error message. When \code{type} is \code{NULL}, the
first entry in the error message is used by default.}
}
\value{
A character vector
}
\description{
Get levels of the outcome variable in grouped or multivariate models
}
\keyword{internal}
