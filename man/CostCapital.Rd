% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CostCapital.R
\name{CostCapital}
\alias{CostCapital}
\title{CostCapital}
\usage{
CostCapital(gdx, type = "stocks", file = NULL, level = "cell")
}
\arguments{
\item{gdx}{GDX file}

\item{type}{either capital stocks ("stocks") or overall capital investment "investment"}

\item{file}{a file name the output should be written to using write.magpie}

\item{level}{Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate}
}
\value{
A MAgPIE object containing values related with overall value of production [million US$17]
}
\description{
Reads data to calculate capital stocks
}
\examples{

  \dontrun{
    x <- CostCapital(gdx)
  }

}
\author{
Edna Molina Bacca
}
