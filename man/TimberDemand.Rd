% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TimberDemand.R
\name{TimberDemand}
\alias{TimberDemand}
\title{TimberDemand}
\usage{
TimberDemand(gdx, file = NULL, level = "regglo")
}
\arguments{
\item{gdx}{GDX file}

\item{file}{a file name the output should be written to using write.magpie}

\item{level}{Level of regional aggregation; "cell", "reg" (regional), "glo" (global), "regglo" (regional and global) or any secdforest aggregation level defined in superAggregate}
}
\value{
Forest demandfor timber production
}
\description{
reads timber demand out of a MAgPIE gdx file
}
\details{
Forest demandfor timber production
}
\examples{

  \dontrun{
    x <- TimberDemand(gdx)
  }
}
\author{
Abhijeet Mishra
}
