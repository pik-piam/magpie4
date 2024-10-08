% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/costsMACCS.R
\name{costsMACCS}
\alias{costsMACCS}
\title{costsMACCS}
\usage{
costsMACCS(gdx, file = NULL, level = "regglo")
}
\arguments{
\item{gdx}{GDX file}

\item{file}{a file name the output should be written to using write.magpie}

\item{level}{Level of regional aggregation ("reg", "glo", "regglo")}
}
\value{
MAgPIE object containing mitigation costs [million US$17]
}
\description{
reads costs entering the objective function from a MAgPIE gdx file
}
\examples{

  \dontrun{
    x <- costsMACCS(gdx)
  }

}
\author{
Debbora Leip
}
