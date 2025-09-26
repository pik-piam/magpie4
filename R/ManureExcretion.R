#' @title ManureExcretion
#' @description downscales Manure Excretion
#' @importFrom memoise memoise
#' @importFrom rlang hash
#' @export
#'
#' @param gdx GDX file
#' @param level aggregation level: glo, reg, cell, grid, iso
#' @param dir for gridded outputs: magpie output directory which contains a mapping file (rds) for disaggregation
#' @param products livestock products
#' @param awms large animal waste management categories: "grazing","stubble_grazing","fuel","confinement"),
#' @param agg aggregation over "awms" or over "products".
#' @param dir directory with spamfiles
#'
#' @return MAgPIE object
#' @author Benjamin Leon Bodirsky
#' @examples
#'
#'   \dontrun{
#'     x <- ManureExcretion(gdx)
#'   }
#'

ManureExcretion <- memoise(function(gdx,level="reg",products="kli",awms=c("grazing","stubble_grazing","fuel","confinement"),agg=TRUE,dir=".") {

  products=findset(products,noset = "original")

  manure <- collapseNames(readGDX(gdx, "ov_manure", select = list(type = "level"))[,,"nr"])

  if(level%in%c("cell")){
    #downscale to cell using magpie info
    manure <- gdxAggregate(gdx = gdx,weight = 'production',x = manure,to = "cell",absolute = TRUE,dir = dir, products = readGDX(gdx,"kli"), product_aggr = FALSE)
  }
  if(level %in% c("grid","iso")) {
    #kli_rum=readGDX(gdx,"kli_rum")
    #kli_mon=readGDX(gdx,"kli_mon")
    kli_rum=c("livst_rum","livst_milk")
    kli_mon=c("livst_pig","livst_chick","livst_egg")

    ruminants = manure[,,kli_rum]
    monogastrics = manure[,,kli_mon]

    ruminants_pasture <- ruminants[,,c("grazing","fuel")]
    ruminants_crop <- ruminants[,,c("stubble_grazing","confinement")]

    ruminants_pasture<-gdxAggregate(
      gdx=gdx,
      x = ruminants_pasture,
      weight = "production", products = "pasture",
      absolute = TRUE,to = level,
      dir = dir)

    kcr_without_bioenergy = setdiff(findset("kcr"),c("betr","begr"))
    ruminants_crop<-gdxAggregate(
      gdx=gdx,
      x = ruminants_crop,
      weight = "production", products = kcr_without_bioenergy, product_aggr=TRUE, attributes="nr",
      absolute = TRUE,to = level,
      dir = dir)

    ruminants <- mbind(ruminants_crop, ruminants_pasture)

    dev <- readGDX(gdx,"im_development_state")[,getYears(monogastrics),]
    monogastrics_cities <- monogastrics*(1-dev)
    monogastrics_cropland <- monogastrics*dev

    monogastrics_cities<-gdxAggregate(
      gdx=gdx,
      x = monogastrics_cities,
      weight = "land", types="urban",
      absolute = TRUE,to = level,
      dir = dir)

    monogastrics_cropland<-gdxAggregate(
      gdx=gdx,
      x = monogastrics_cropland,
      weight = "land", types="crop",
      absolute = TRUE,to = level,
      dir = dir)

    monogastrics <- monogastrics_cities + monogastrics_cropland

    manure <- mbind(monogastrics,ruminants)
  }
  x=manure

  ##testing
  if (abs((sum(x)-sum(manure)))>10^-10) { warning("disaggregation failure: mismatch of sums after disaggregation")}

  x = x[,,list(kli = products,awms = awms)]
  if("awms"%in%agg){
    x=dimSums(x,dim="awms")
  }
  if("products"%in%agg){
    x=dimSums(x,dim="kli")
  }

  return(x)
}
# the following line makes sure that a working directory change leads to new
# caching, which is important if the function is called with relative path args.
,hash = function(x) hash(list(x,getwd())))

