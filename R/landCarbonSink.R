#' @title Land Carbon Sink Adjustment Factors
#' @description Indirect human-induced emissions in the land use system
#'
#' @export
#'
#' @param gdx         GDX file
#' @param file        a file name the output should be written to using write.magpie
#' @param level       level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global).
#' @param cumulative  Logical; Determines if emissions are reported annually (FALSE) or cumulative (TRUE). The starting point for cumulative emissions is y1995.
#' @param baseyear    Baseyear used for cumulative emissions (default = 1995)
#' @param source      Currently only "Grassi", which uses pre-calculated adjustment factors from Grassi et al 2021 (DOI 10.1038/s41558-021-01033-6). Can be extended in the future to also include "PIK", based on data from LPJmL.
#' @details Calculates global and regional Land Carbon Sink Adjustment Factors
#' @return Land Carbon Sink Adjustment Factors (Mt CO2 per year or cumulative)
#' @author Florian Humpenoeder
#' @importFrom magclass new.magpie getYears as.magpie setYears
#' @examples
#' \dontrun{
#' x <- landCarbonSink(gdx)
#' }
#'
landCarbonSink <- function(gdx, file = NULL, level = "reg", cumulative = FALSE, baseyear = 1995, source = "Grassi") {

  if (source == "Grassi") {
    a <- readGDX(gdx,"i52_land_carbon_sink",react = "silent")
    if(!is.null(a)) {
      t <- readGDX(gdx,"t")
      a <- a[,t,] * 1000
      a <- superAggregateX(a, level = level, aggr_type = "sum")
    }
  } else stop("This source is not available")

  if (cumulative & !is.null(a)) {
    years <- getYears(a,as.integer = T)
    im_years <- new.magpie("GLO",years,NULL)
    im_years[,,] <- c(1,diff(years))
    a[,"y1995",] <- 0
    a <- a*im_years[,getYears(a),]
    a <- as.magpie(apply(a,c(1,3),cumsum))
    a <- a - setYears(a[,baseyear,],NULL)
  }

  out(a, file)
}
