#' @title SOM2
#' @description Calculates soil organic carbon stock size
#'              based on a MAgPIE gdx file (for threepool realization)
#'
#' @export
#'
#' @param gdx GDX file
#' @param type "stock" (default) for absoulte values,
#'             "density" for per hectar values
#' @param noncropAggr aggregate non cropland types to 'noncropland'
#'                     (if FALSE all land types of pools59 will be reported)
#' @param level Level of regional aggregation;
#'                "reg" (regional),
#'                "glo" (global),
#'                "regglo" (regional and global)
#'
#' @return A MAgPIE object containing som values
#' @author Kristine Karstens
#' @examples
#' \dontrun{
#' x <- SOM2(gdx)
#' }
#'
SOM2 <- function(gdx, type = "stock", level = "regglo", noncropAggr = TRUE) {

  somStock        <- readGDX(gdx, "ov59_topsoilc_actualstate", select = list(type = "level"))
  equStockCrop    <- readGDX(gdx, "ov59_topsoilc_crop_steadystate", select = list(type = "level"))
  equStockNoncrop <- readGDX(gdx, "ov59_topsoilc_noncrop_steadystate", select = list(type = "level"))
  years           <- getYears(somStock)
  natStock        <- readGDX(gdx, "p59_topsoilc_naturalstate")[, years, ]
  landuse         <- readGDX(gdx, "ov_land", select = list(type = "level"))
  landuse         <- gdxAggregate(gdx, landuse, to = "reg", absolute = TRUE)

  .aggregatePools <- function(x) {
    return(dimSums(x, dim = "sPools59"))
  }

  somStock <- .aggregatePools (somStock)
  equStock <- mbind(setNames(dimSums(equStockCrop, dim = 3), "crop"),
                    .aggregatePools(equStockNoncrop))
  natStock <- .aggregatePools(natStock)

  if(noncropAggr == TRUE) {
    .aggregateNoncrop <- function(x) {
      nc59 <- readGDX(gdx, "noncropland59", types = "sets", react = "silent")
      return(mbind(x[, , "crop"], setNames(dimSums(x[, , nc59], dim = 3.1), "natveg")))
    }
    somStock <- .aggregateNoncrop(somStock)
    equStock <- .aggregateNoncrop(equStock)
    natStock <- .aggregateNoncrop(natStock)
    landuse  <- .aggregateNoncrop(landuse)
  }

  .addTotal <- function(x){
    return(mbind(x, add_dimension(dimSums(x, dim = 3.1), add = "landuse", nm = "total")))
  }

  somStock <- .addTotal(somStock)
  equStock <- .addTotal(equStock)
  natStock <- .addTotal(natStock)
  landuse  <- .addTotal(landuse)

  if (type == "density") {

    .calcDensity <- function(x, land) {
       x <- x / land
       x[is.infinite(x)] <- NA
       return(x)
    }

    somStock <- .calcDensity(somStock, landuse)
    equStock <- .calcDensity(equStock, landuse)
    natStock <- .calcDensity(natStock, landuse)
    somStock <- gdxAggregate(gdx, somStock, weight = landuse, to = level, absolute = FALSE)
    equStock <- gdxAggregate(gdx, equStock, weight = landuse, to = level, absolute = FALSE)
    natStock <- gdxAggregate(gdx, natStock, weight = landuse, to = level, absolute = FALSE)

  } else {

    somStock <- gdxAggregate(gdx, somStock, to = level, absolute = TRUE)
    equStock <- gdxAggregate(gdx, equStock, to = level, absolute = TRUE)
    natStock <- gdxAggregate(gdx, natStock, to = level, absolute = TRUE)
  }

  out <- mbind(add_dimension(somStock, dim = 3.1, add = "state", nm =  "actualstate"),
               add_dimension(natStock, dim = 3.1, add = "state", nm = "naturalstate"),
               add_dimension(equStock, dim = 3.1, add = "state", nm = "steadystate"))

  return(out)
}
