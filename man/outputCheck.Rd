% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outputCheck.R
\name{outputCheck}
\alias{outputCheck}
\title{outputCheck}
\usage{
outputCheck(gdx)
}
\arguments{
\item{gdx}{GDX file}
}
\description{
Function to check a MAgPIE gdx file for known problems
(e.g. non-zero dummy variables). The function will throw warnings for
problem found in the outputs.
}
\examples{
\dontrun{
outputCheck(gdx)
}
}
\author{
Jan Philipp Dietrich
}
