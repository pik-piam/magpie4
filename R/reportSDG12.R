#' @title reportSDG12
#' @description reports all SDG indicators relevant for SD12 - Sustainable Production and Consumption
#' @import magpiesets
#'
#' @export
#'
#' @param gdx GDX file
#' @return MAgPIE object
#' @author Benjamin Bodirsky
#' @examples
#'
#'   \dontrun{
#'     x <- reportSDG12(gdx)
#'   }
#'
#'
#' @section SDG12 Sustainable consumption variables:
#' Name | Unit | Meta
#' ---|---|---
#' SDG\|SDG12\|Material footprint | tDM/capita/yr | Per-capita crop demand (material footprint proxy)
#' SDG\|SDG12\|Food waste | kcal/cap/day | Per-capita daily food waste (caloric availability minus intake)
#' SDG\|SDG12\|Food waste total | Mt DM/yr | Total food waste in dry matter
#' SDG\|SDG12\|Food loss | Mt DM/yr | Food losses in supply chain (pre-consumer waste)
#' @md


reportSDG12 <- function(gdx) {
  x <- NULL

  indicatorname="SDG|SDG12|Material footprint"
  unit="tDM/capita/yr"
  # better backcalculation of footprint would be nice! E.g impacts by ton, accounting for average trade patterns
  out <- demand(gdx,level="regglo")
  out <- out[,,findset("kcr")]
  out <- dimSums(out)
  pop <- population(gdx,level="regglo")
  out <- out/pop
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)

  indicatorname="SDG|SDG12|Food waste"
  unit="kcal/cap/day"
  out <- Kcal(gdx,level="regglo")
  tmp <- IntakeDetailed(gdx,level = "regglo",product_aggr=TRUE)
  out<-out-tmp
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)

  indicatorname="SDG|SDG12|Food waste total"
  unit="Mt DM/yr"
  att <- collapseNames(readGDX(gdx=gdx,"fm_nutrition_attributes","f15_nutrition_attributes",
                               format = "first_found")[,,"kcal"]) * 1000000 # kcal per tDM
  out <- Kcal(gdx,level="regglo",product_aggr = FALSE) * population(gdx,level = "regglo") * 365 # mio. kcal
  tmp <- IntakeDetailed(gdx,level = "regglo",product_aggr=FALSE) * population(gdx,level = "regglo") * 365 # mio. kcal
  out <- dimSums(out/att[,getYears(out),getNames(out,dim=1)],dim=3)
  tmp <- dimSums(tmp/att[,getYears(tmp),getNames(tmp,dim=1)],dim=3)
  out<-out-tmp
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)

  indicatorname="SDG|SDG12|Food loss"
  unit="Mt DM/yr"
  out <- demand(gdx,level="regglo")
  out <- out[,,readGDX(gdx,"kall")][,,"waste"]
  out <- dimSums(out)
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)

  #x <- x[,,sort(getNames(x))]
  return(x)
}

