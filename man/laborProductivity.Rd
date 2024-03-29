% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/laborProductivity.R
\name{laborProductivity}
\alias{laborProductivity}
\title{laborProductivity}
\usage{
laborProductivity(gdx, level = "reg", productAggr = TRUE)
}
\arguments{
\item{gdx}{GDX file}

\item{level}{spatial aggregation to report productivity ("cell","reg", "regglo", "glo")}

\item{productAggr}{Aggregate over products or not (boolean)}
}
\value{
labor productivity in crop sector (kg DM per hour)
}
\description{
calculates labor productivity in crop sector (kg DM per hour)
             from a MAgPIE gdx file
}
\examples{
\dontrun{
x <- laborProductivity(gdx)
}
}
\author{
Xiaoxi Wang, Ruiying Du, Debbora Leip
}
