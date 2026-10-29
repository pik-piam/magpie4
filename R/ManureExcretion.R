#' @title ManureExcretion
#' @description downscales Manure Excretion
#' @importFrom memoise memoise
#' @importFrom rlang hash
#' @importFrom R.utils lastModified
#' @export
#'
#' @param gdx GDX file
#' @param level aggregation level: glo, reg, cell, grid, iso
#' @param products livestock products
#' @param awms animal waste management systems: "grazing", "stubble_grazing", "fuel", "confinement"
#' @param agg aggregation over "awms" or over "products".
#' @param disaggLvst grid-level disaggregation method (level "grid"/"iso" only):
#' "landbased" (default) weights ruminant manure by pasture or crop production and monogastric
#' manure by urban or cropland area; "glw" weights by the gridded livestock distribution
#' (f71_livestock_distribution_0.5.mz next to gdx) times the pasture share (grazing, fuel)
#' or cropland share (stubble_grazing).
#' @param recycled "glw" only: if TRUE, confinement manure is also weighted by the cropland share.
#'
#' @return MAgPIE object
#' @author Benjamin Leon Bodirsky, Bin Lin
#' @examples
#'
#'   \dontrun{
#'     x <- ManureExcretion(gdx)
#'   }
#'

ManureExcretion <- memoise(function(gdx, level = "reg", products = "kli",
                                    awms = c("grazing", "stubble_grazing", "fuel", "confinement"), agg = TRUE,
                                    disaggLvst = "landbased", recycled = FALSE) {

  products=findset(products,noset = "original")

  manure <- collapseNames(readGDX(gdx, "ov_manure", select = list(type = "level"))[,,"nr"])
  manureOrigSum <- sum(manure)

  if(level%in%c("cell")){
    #downscale to cell using magpie info
    manure <- gdxAggregate(gdx = gdx,weight = 'production',x = manure,to = "cell",absolute = TRUE, products = readGDX(gdx,"kli"), product_aggr = FALSE)
  }
  if(level %in% c("grid","iso")) {

    useGLWDisagg <- switch(disaggLvst,
                           "landbased" = FALSE,
                           "glw" = TRUE,
                           stop("disaggLvst must be one of 'landbased', 'glw'"))

    if (useGLWDisagg) {
      lvstDist <- readLivestockDistribution(gdx, readGDX(gdx, "kli"), getYears(manure))

      # land share per grid cell; the offset keeps a non-zero weight where the land type is absent
      gridLand <- land(gdx, level = "grid")[, getYears(manure), ]
      totalLand <- dimSums(gridLand, dim = 3)
      # cells without any land get a land share of 0 instead of 0/0
      totalLand[totalLand == 0] <- 1
      landShare <- function(type) {
        collapseDim(gridLand[getItems(lvstDist, 1), , type], dim = 3) / totalLand[getItems(lvstDist, 1), , ] + 1e-6
      }
      awmsWeight <- function(a) {
        type <- switch(a, grazing = "past", fuel = "past", stubble_grazing = "crop",
                       confinement = if (recycled) "crop" else NULL)
        if (is.null(type)) {
          return(lvstDist)
        }
        w <- lvstDist
        w[] <- as.vector(lvstDist) * as.vector(landShare(type))
        w
      }

      # weights differ by awms, so disaggregate each slice separately
      awmsAll <- getItems(manure, dim = 3.2)
      manure <- mbind(lapply(awmsAll, function(a) {
        sliceManure <- collapseNames(manure[, , a])
        # the weight is at grid level, so disaggregate to grid and aggregate to iso from there
        disagg <- gdxAggregate(gdx = gdx, x = sliceManure, weight = awmsWeight(a), absolute = TRUE, to = "grid")
        if (level == "iso") {
          disagg <- gdxAggregate(gdx = gdx, x = disagg, to = "iso", absolute = TRUE)
        }
        add_dimension(disagg, dim = 3.2, add = "awms", nm = a)
      }))
    }

    if (!useGLWDisagg) {
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
      absolute = TRUE,to = level)

    kcr_without_bioenergy = setdiff(findset("kcr"),c("betr","begr"))
    ruminants_crop<-gdxAggregate(
      gdx=gdx,
      x = ruminants_crop,
      weight = "production", products = kcr_without_bioenergy, product_aggr=TRUE, attributes="nr",
      absolute = TRUE,to = level)

    ruminants <- mbind(ruminants_crop, ruminants_pasture)

    dev <- readGDX(gdx,"im_development_state")[,getYears(monogastrics),]
    monogastrics_cities <- monogastrics*(1-dev)
    monogastrics_cropland <- monogastrics*dev

    monogastrics_cities<-gdxAggregate(
      gdx=gdx,
      x = monogastrics_cities,
      weight = "land", types="urban",
      absolute = TRUE,to = level)

    monogastrics_cropland<-gdxAggregate(
      gdx=gdx,
      x = monogastrics_cropland,
      weight = "land", types="crop",
      absolute = TRUE,to = level)

    monogastrics <- monogastrics_cities + monogastrics_cropland

    manure <- mbind(monogastrics,ruminants)
    }
  }
  x=manure

  # check mass conservation
  if (abs(sum(x) - manureOrigSum) > 10^-10) {
    warning("disaggregation failure: mismatch of sums after disaggregation")
  }

  x = x[,,list(kli = products,awms = awms)]
  if("awms"%in%agg){
    x=dimSums(x,dim="awms")
  }
  if("products"%in%agg){
    x=dimSums(x,dim="kli")
  }

  return(x)
}
# the following line makes sure that a changing timestamp of the gdx file and
# a working directory change leads to new caching, which is important if the
# function is called with relative path args.
,hash = function(x) hash(list(x, getwd(), lastModified(x$gdx))))

