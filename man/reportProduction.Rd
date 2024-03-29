% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reportProduction.R
\name{reportProduction}
\alias{reportProduction}
\title{reportProduction}
\usage{
reportProduction(gdx, detail = FALSE, agmip = FALSE)
}
\arguments{
\item{gdx}{GDX file}

\item{detail}{if detail=FALSE, the subcategories of groups are not reported
(e.g. "soybean" within "oilcrops")}

\item{agmip}{if agmip = TRUE, additional sector aggregates
required for AgMIP are reported (e.g. "AGR")}
}
\value{
production as MAgPIE object. Unit: see names
}
\description{
reports production
}
\examples{

  \dontrun{
    x <- reportProduction(gdx)
  }

}
\author{
Benjamin Leon Bodirsky, Isabelle Weindl
}
