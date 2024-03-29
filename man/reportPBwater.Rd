% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reportPBwater.R
\name{reportPBwater}
\alias{reportPBwater}
\title{reportPBwater}
\usage{
reportPBwater(gdx, level = "regglo")
}
\arguments{
\item{gdx}{GDX file}

\item{level}{level of aggregation (regglo: regions and global)}
}
\value{
MAgPIE object
}
\description{
reports water planetary boundaries
}
\examples{

  \dontrun{
    x <- reportPBwater(gdx)
  }

}
\author{
Felicitas Beier, Jens Heinke
}
