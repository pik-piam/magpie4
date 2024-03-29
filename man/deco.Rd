% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deco.R
\name{deco}
\alias{deco}
\title{deco}
\usage{
deco(data, names_factor = NULL, plot = FALSE)
}
\arguments{
\item{data}{Decomposition Data as a magpie object. The first column of the third dimension has to be the output (A), while the subsequent columns are the coefficients of the drivers (B,C,...). Example: Area = Population x Supply/Population x Area/Supply. 3rd-dimension column order then has to be: Area, Population, Supply.}

\item{names_factor}{Names of the output (A) and the Decomposition-Factors (B,B/C,C/A), if names_factor=NULL the names for the third column will be generated like the factors for decomposition (above example: Area, Population, Supply/Population, Area/Supply)}

\item{plot}{TRUE or FALSE}
}
\value{
Decomposes the impact of certain drivers to an output (A) value.
}
\description{
Function that quantifies the influences of the underlying drivers to a dependent output variable. It attributes the changes of the output variable (A) to changes of several drivers (B, B/C, C/A). The output must be the product of the drivers.
}
\details{
Use function deco_plot in library luplot to make a plot out of this. It is only usable for the decomposition of 5 or less drivers. For documentation, see paper Huber, Veronika, Ina Neher, Benjamin L. Bodirsky, Kathrin Hoefner, and Hans Joachim Schellnhuber. 2014. "Will the World Run out of Land? A Kaya-Type Decomposition to Study Past Trends of Cropland Expansion." Environmental Research Letters 9 (2): 024011. https://doi.org/10.1088/1748-9326/9/2/024011. Or see master Thesis of Ina Neher (2013)
}
\examples{
 Data<-array(c(1,1.1,1.15,1,1.05,1.1,1,1.05,1.15),c(3,3))
 dimnames(Data)<-list(paste("y",2000:2002,sep=""),c("Area","Population","Supply"))
 Data <- as.magpie(Data)
 deco(Data)

}
\author{
Ina Neher, Benjamin Leon Bodirsky
}
