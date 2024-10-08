% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CostTC.R
\name{CostTC}
\alias{CostTC}
\title{CostTC}
\usage{
CostTC(gdx, file = NULL, level = "reg")
}
\arguments{
\item{gdx}{GDX file}

\item{file}{a file name the output should be written to using write.magpie}

\item{level}{Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate}
}
\value{
A MAgPIE object containing values related with overall value of production [million US$17]
}
\description{
Reads data on TC costs
}
\examples{

  \dontrun{
    x <- CostTC(gdx)
  }

}
\author{
David Chen
}
