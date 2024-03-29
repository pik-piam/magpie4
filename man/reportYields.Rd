% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reportYields.R
\name{reportYields}
\alias{reportYields}
\title{reportYields}
\usage{
reportYields(gdx, detail = FALSE, physical = TRUE)
}
\arguments{
\item{gdx}{GDX file}

\item{detail}{if detail=FALSE, the subcategories of groups are not reported (e.g. "soybean" within "oilcrops")}

\item{physical}{if true (default) physical area (croparea) used for yield calculation;
if false harvested area used for yield calculation}
}
\value{
yield as MAgPIE object (Mt DM/ha)
}
\description{
reports yields
}
\examples{
\dontrun{
x <- reportYields(gdx)
}

}
\author{
Florian Humpenoeder, Xiaoxi Wang, Kristine Karstens, Abhijeet Mishra, Felicitas Beier
}
