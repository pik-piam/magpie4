#' @title reportGridManureExcretion
#' @description reports Manure with reortingnames on grid level.
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param dir for gridded outputs: magpie output directory which contains a mapping file (rds or spam) disaggregation
#' @param spamfiledirectory deprecated. please use \code{dir} instead
#' 
#' @return MAgPIE object
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#'   \dontrun{
#'     x <- reportGridManureExcretion(gdx)
#'   }
#' 

reportGridManureExcretion <- function(gdx,dir=".",spamfiledirectory="") {
  
  dir <- getDirectory(dir,spamfiledirectory)
  
  manure <- collapseNames(readGDX(gdx, "ov_manure", select = list(type = "level"))[,,"nr"])
  #downscale to cell using magpie info
  manure_cell <- gdxAggregate(gdx = gdx,weight = 'production',x = manure,to = "cell",absolute = TRUE,dir = dir, products = readGDX(gdx,"kli"), product_aggr = FALSE)
  
  ruminants = manure_cell[,,readGDX(gdx,"kli_rum")]
  
  monogastrics = manure_cell[,,readGDX(gdx,"kli_mon")]
  
  ruminants_pasture <- ruminants[,,c("grazing","fuel")]
  ruminants_crop <- ruminants[,,c("stubble_grazing","confinement")]
  
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
  
  ruminants <- mbind(ruminants_crop, ruminants_pasture)
  
  dev <- readGDX(gdx,"im_development_state")[,getYears(monogastrics),]
  monogastrics_cities <- monogastrics*(1-dev)
  monogastrics_cropland <- monogastrics*dev
  
  monogastrics_cities<-gdxAggregate(
    gdx=gdx,
    x = monogastrics_cities,
    weight = "land", types="urban",
    absolute = TRUE,to = "grid",
    dir = dir)
  
  monogastrics_cropland<-gdxAggregate(
    gdx=gdx,
    x = monogastrics_cropland,
    weight = "land", types="crop",
    absolute = TRUE,to = "grid",
    dir = dir)
  
  monogastrics <- monogastrics_cities + monogastrics_cropland
  
  x <- mbind(monogastrics,ruminants)
  
  'Resources|Nitrogen|Manure
  Resources|Nitrogen|Manure||Other Use
  Resources|Nitrogen|Manure||Total Storage Losses in Animal Waste Management
  Emissions|N2|Agriculture||Animal Waste Management
  Emissions|NH3|Agriculture||Animal Waste Management
  Emissions|total N2O-N emissions|Agriculture||Animal Waste Management
  Emissions|NO2|Agriculture||Animal Waste Management
  Emissions|NO3-|Agriculture||Animal Waste Management'
  
  ##testing
  if (abs((sum(x)-sum(manure)))>10^-10) { warning("disaggregation failure: mismatch of sums after disaggregation")}
  
  getNames(x,dim = 1) <- magpiesets::reportingnames(getNames(x,dim = 1))
  x <- metadata_comments(x=x,unit="Mt Nr", description="Total manure excretion, before animal waste management losses",comment="",note="")
  
  return(x)
}

