% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/landCarbonSink.R
\name{landCarbonSink}
\alias{landCarbonSink}
\title{Land Carbon Sink Adjustment Factors}
\usage{
landCarbonSink(
  gdx,
  file = NULL,
  level = "reg",
  cumulative = FALSE,
  baseyear = 1995,
  source = "Grassi"
)
}
\arguments{
\item{gdx}{GDX file}

\item{file}{a file name the output should be written to using write.magpie}

\item{level}{level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global).}

\item{cumulative}{Logical; Determines if emissions are reported annually (FALSE) or cumulative (TRUE). The starting point for cumulative emissions is y1995.}

\item{baseyear}{Baseyear used for cumulative emissions (default = 1995)}

\item{source}{Currently only "Grassi", which uses pre-calculated adjustment factors from Grassi et al 2021 (DOI 10.1038/s41558-021-01033-6). Can be extended in the future to also include "PIK", based on data from LPJmL.}
}
\value{
Land Carbon Sink Adjustment Factors (Mt CO2 per year or cumulative)
}
\description{
Indirect human-induced emissions in the land use system
}
\details{
Calculates global and regional Land Carbon Sink Adjustment Factors
}
\examples{
\dontrun{
x <- landCarbonSink(gdx)
}

}
\author{
Florian Humpenoeder
}
