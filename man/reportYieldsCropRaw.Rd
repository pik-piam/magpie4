% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reportYieldsCropRaw.R
\name{reportYieldsCropRaw}
\alias{reportYieldsCropRaw}
\title{reportYieldsCropRaw}
\usage{
reportYieldsCropRaw(gdx, detail = FALSE)
}
\arguments{
\item{gdx}{GDX file}

\item{detail}{if detail=FALSE, the subcategories of groups are not reported (e.g. "soybean" within "oilcrops")}
}
\value{
yield as MAgPIE object (Mt DM/ha)
}
\description{
reports potential yields before calibration
}
\examples{
\dontrun{
x <- reportYieldsCropRaw(gdx)
}

}
\author{
Edna J. Molina Bacca
}
