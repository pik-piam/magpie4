% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/YieldsCropRaw.R
\name{YieldsCropRaw}
\alias{YieldsCropRaw}
\title{YieldsCropRaw}
\usage{
YieldsCropRaw(gdx, file = NULL, level = "cell")
}
\arguments{
\item{gdx}{GDX file}

\item{file}{a file name the output should be written to using write.magpie}

\item{level}{Level of regional aggregation}
}
\value{
A MAgPIE object containing values of potential yields after the calibration routines
}
\description{
Reads potential yields after calibration
}
\examples{
\dontrun{
x <- YieldsCropRaw(gdx)
}

}
\author{
Edna Molina Bacca
}
