% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reportFeedConversion.R
\name{reportFeedConversion}
\alias{reportFeedConversion}
\title{reportFeedConversion}
\usage{
reportFeedConversion(gdx, livestockSystem = TRUE, balanceflow = FALSE)
}
\arguments{
\item{gdx}{GDX file}

\item{livestockSystem}{if TRUE, ruminant products and poultry products are aggregated}

\item{balanceflow}{If true, feed includes the calibration balanceflow}
}
\value{
feed demand as MAgPIE object (Mt DM)
}
\description{
reportes feed demand by animal type
}
\examples{

  \dontrun{
    x <- reportFeed()
  }


}
\author{
Benjamin Bodirsky
}
