% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/costsWholesale.R
\name{costsWholesale}
\alias{costsWholesale}
\title{costsWholesale}
\usage{
costsWholesale(gdx, file = NULL, level = "regglo")
}
\arguments{
\item{gdx}{GDX file}

\item{file}{a file name the output should be written to using write.magpie}

\item{level}{Level of regional aggregation; "reg" (regional), "glo" (global),
"regglo" (regional and global) or any other aggregation level defined in superAggregate}
}
\value{
A MAgPIE object containing values related with costs wholesale trade [million US$17/tDM]
}
\description{
Reads data to calculate wholesale costs
}
\examples{
\dontrun{
x <- costsWholesale(gdx)
}

}
\author{
David M Chen
}
