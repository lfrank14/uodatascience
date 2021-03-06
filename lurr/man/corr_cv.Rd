% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat_functions.R
\name{corr_cv}
\alias{corr_cv}
\title{Cross-Validated Correlation}
\usage{
corr_cv(x, y)
}
\arguments{
\item{x}{A single vector.}

\item{y}{A single vector.}
}
\description{
This function allows you to run a correlation using leave-one-out cross-validation.
}
\examples{
corr_cv(got$`Book Intro Chapter`, got$`Death Chapter`)
}
\keyword{correlation}
\keyword{cross-validation,}
