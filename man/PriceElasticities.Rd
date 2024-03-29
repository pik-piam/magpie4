% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PriceElasticities.R
\name{PriceElasticities}
\alias{PriceElasticities}
\title{PriceElasticities}
\usage{
PriceElasticities(
  gdx,
  file = NULL,
  level = "reg",
  calibrated = TRUE,
  products = "kfo"
)
}
\arguments{
\item{gdx}{GDX file}

\item{file}{a file name the output should be written to using write.magpie}

\item{level}{Level of regional aggregation; "iso" ISO country codes, "reg" (regional), "glo" (global)}

\item{calibrated}{if FALSE, the true regression outputs are used, if TRUE the values calibrated to the start years are used}

\item{products}{set of the products for which the elasticity should be estimated. Please note that this stills remains an elasticity relative to total food expenditure. So its the change in consumption of one good when the prices of all products change according to the scenario.}
}
\value{
magpie object with the livestock share in a region or country. Unit is dimensionsless, but value depends on chosen attribute
}
\description{
Calculates the physical elasticity for food demand
}
\examples{

  \dontrun{
    x <- PriceElasticities(gdx)
  }

}
\author{
Benjamin Bodirsky
}
