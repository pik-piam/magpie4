% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wageDevelopment.R
\name{wageDevelopment}
\alias{wageDevelopment}
\title{wageDevelopment}
\usage{
wageDevelopment(gdx, baseYear = 2000, file = NULL, level = "regglo")
}
\arguments{
\item{gdx}{GDX file}

\item{baseYear}{year relative to which the wage development should be calculated}

\item{file}{a file name the output should be written to using write.magpie}

\item{level}{Level of regional aggregation ("iso", "reg", "glo", "regglo")}
}
\value{
MAgPIE object containing indicator on wage development
}
\description{
calculates indicator to describe wage development based on agricultural wages in MAgPIE (hourly labor
costs relative to a base year)
}
\examples{
\dontrun{
x <- wageDevelopment(gdx)
}

}
\author{
Debbora Leip
}
