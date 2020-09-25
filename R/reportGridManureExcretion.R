#' @title reportGridManureExcretion
#' @description reports Croparea from gridded (disaggregated) output
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param dir for gridded outputs: magpie output directory which contains a mapping file (rds or spam) disaggregation
#' @param spamfiledirectory deprecated. please use \code{dir} instead
#' 
#' @return area of cropland as MAgPIE object (million ha)
#' @author Jannes Breier
#' @examples
#' 
#'   \dontrun{
#'     x <- reportGridManureExcretion(gdx)
#'   }
#' 

reportGridManureExcretion <- function(gdx,dir=".",spamfiledirectory="") {
  
  dir <- getDirectory(dir,spamfiledirectory)
  
  manure <- collapseNames(readGDX(gdx, "ov_manure", select = list(type = "level"))[,,"nr"])

  ruminants = manure[,,readGDX(gdx,"kli_rum")]
  
  monogastrics = dimSums(manure[,,readGDX(gdx,"kli_mon")],dim=3.2)
  ruminants_pasture <- dimSums(ruminants[,,c("grazing","fuel")],dim=3.2)
  ruminants_crop <- dimSums(ruminants[,,c("stubble_grazing","confinement")],dim=3.2)
  
  ruminants_pasture<-gdxAggregate(
    gdx=gdx,
    x = ruminants_pasture,
    weight = "production", products = "pasture",
    absolute = TRUE,to = "grid",
    dir = dir)
  
  kcr_without_bioenergy = setdiff(findset("kcr"),c("betr","begr"))
  ruminants_crop<-gdxAggregate(
    gdx=gdx,
    x = ruminants_crop,
    weight = "production", products = kcr_without_bioenergy, product_aggr=TRUE, attributes="nr",
    absolute = TRUE,to = "grid",
    dir = dir)
  
  ruminants <- ruminants_crop + ruminants_pasture
  
  monogastrics<-gdxAggregate(
    gdx=gdx,
    x = monogastrics,
    weight = "land", types="urban",
    absolute = TRUE,to = "grid",
    dir = dir)
  
  x <- mbind(monogastrics,ruminants)
  
  ##testing
  if (abs((sum(x)-sum(manure)))>10^-10) { warning("disaggregation failure: mismatch of sums after disaggregation")}
  
  getNames(x,dim = 1) <- magpiesets::reportingnames(getNames(x,dim = 1))
  x <- metadata_comments(x=x,unit="Mt Nr", description="Total manure excretion, before animal waste management losses",comment="",note="")
  
  return(x)
}

