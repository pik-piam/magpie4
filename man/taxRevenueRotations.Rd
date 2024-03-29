% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/taxRevenueRotations.R
\name{taxRevenueRotations}
\alias{taxRevenueRotations}
\title{taxRevenueRotations}
\usage{
taxRevenueRotations(
  gdx,
  file = NULL,
  level = "regglo",
  dir = ".",
  penalty = "onlyTaxRevenue"
)
}
\arguments{
\item{gdx}{GDX file}

\item{file}{a file name the output should be written to using write.magpie}

\item{level}{aggregation level, reg, glo or regglo}

\item{dir}{spamfiledirectory}

\item{penalty}{"OnlyTaxRevenue" provides the tax Revenues from a rotation tax/subsidy.
"OnlyInternalizedServices" provides the penalty by foregone Ecosystem Services,
the part of the externality which is internalized by the farmer independent of the tax.
"FullPenalty" provides the sum of both, which is what the model sees.}
}
\description{
calculates taxes of crop rotations as difference between the selected scenario
and the baseline scenario that shall capture the internalized incentives for crop rotations.
}
\examples{
\dontrun{
x <- wageRent(gdx)
}

}
\author{
Benjamin Leon Bodirsky
}
